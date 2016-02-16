{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE JavaScriptFFI       #-}

module AdPlatform.Main (main) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative

import           Data.Monoid
import           Data.String                    (fromString)
import           GHCJS.Types                    (JSString, JSVal)

import           Web.VirtualDom.Html            (Property, br, button, div,
                                                 form, h1, hr, img, p, table,
                                                 tbody, td, text, th, thead, tr)
import qualified Web.VirtualDom.Html            as E
import           Web.VirtualDom.Html.Attributes (class_, src, width)
import qualified Web.VirtualDom.Html.Attributes as A
import           Web.VirtualDom.Html.Events     (change, click, preventDefault, Event(),
                                                 stopPropagation, submit, value)

import           Lubeck.App                     (Html, runAppReactiveX, KbdEvents(..))
import           Lubeck.Forms
import           Lubeck.FRP

import qualified BD.Data.Account                as Account
import qualified BD.Data.Ad                     as Ad
import qualified BD.Data.AdCampaign             as AdCampaign
import qualified BD.Data.Count                  as C
import qualified BD.Data.Image                  as Im
import           BD.Data.Interaction
import           BD.Data.SearchPost             (SearchPost)
import qualified BD.Data.SearchPost             as P
import           BD.Types
import           BD.Utils
import           BD.Api

import           AdPlatform.Pages.Campaign      (campaignPage, getCampaigns)
import           AdPlatform.Pages.CreateAd      (createAdPage)
import           AdPlatform.Pages.ImageLibrary  (imageLibraryPage)
import           AdPlatform.Pages.Login         (loginPage, Username)
import           AdPlatform.Pages.PostSearch    (searchPage)
import           AdPlatform.Pages.User          (userPage)

import           Components.BusyIndicator       (BusyCmd (..), withBusy,
                                                 busyIndicatorComponent)
import           Components.Notifications       (notificationsComponent)
import           Components.MainMenu            (mainMenuComponent)

import           Lubeck.Util
import           AdPlatform.Types
import           AdPlatform.Config


menuItems =
  [ (NavUser,     "User")
  , (NavSearch,   "Search")
  , (NavImages,   "Image Library")
  , (NavCreateAd, "Create Ad")
  , (NavLogin,    "Logout") -- last item is special in that it will be positioned far right
  ]

rootLayout goTo menu err busy login user ads search createAd imlib = case goTo of
  NavLogin    -> layoutLogin busy err login
  NavUser     -> layout menu busy err user
  NavCampaign -> layout menu busy err ads
  NavSearch   -> layout menu busy err search
  NavCreateAd -> layout menu busy err createAd
  NavImages   -> layout menu busy err imlib
  where
    layoutLogin busy err page =
      div [class_ "container login-top-buffer"]
        [ div [class_ "col-xs-12"]
          [ busy
          , err
          , page ] ]

    layout menu busy err page =
      div [class_ "container"]
        [ menu
        , div [class_ "col-xs-12 top-buffer"]
          [ busy
          , err
          , page
          ] ]



adPlatform :: IO (Signal Html, Maybe (Sink KbdEvents))
adPlatform = do
  (kbdSink, kbdEvents)                  <- newEventOf (undefined :: KbdEvents)
  (ipcSink, ipcEvents)                  <- newEventOf (undefined :: IPCMessage)

  (notifView, notifSink, notifKbdSink)  <- notificationsComponent []
  (busyView, busySink)                  <- busyIndicatorComponent []

  (loginView, userLoginE)               <- loginPage (fromString defaultUsername, fromString defaultPassword)
  userLoginB                            <- stepper Nothing (fmap (Just . fst) userLoginE) :: IO (Behavior (Maybe Username))

  authOk                                <- withErrorIO notifSink $ fmap (withBusy busySink Account.authenticateOrError) userLoginE :: IO (Events Account.AuthToken)
  let validUserLoginE                   = sample userLoginB authOk :: Events (Maybe Username)

  let bypassAuthUserE                   = fmap fst userLoginE
  userE                                 <- withErrorIO notifSink $ fmap (withBusy busySink Account.getUserOrError)
                                                                        (if useAuth then (filterJust validUserLoginE)
                                                                                    else bypassAuthUserE)

  camapaignsE                           <- withErrorIO notifSink $ fmap (withBusy busySink getCampaigns) userE

  userS                                 <- stepperS Nothing (fmap Just userE)
  campaignsS                            <- stepperS Nothing (fmap Just camapaignsE)

  let userB                             = current userS
  let usernameB                         = fmap (fmap Account.username) userB

  (userView, loadAdsE)                  <- userPage         busySink notifSink                   userB campaignsS
  adsView                               <- campaignPage     busySink notifSink loadAdsE          userB
  (imageLibView, imsB, imlibKbdSink)    <- imageLibraryPage busySink notifSink ipcSink ipcEvents userE
  searchPageView                        <- searchPage       busySink notifSink ipcSink           usernameB
  createAdView                          <- createAdPage     busySink notifSink                   usernameB imsB (current campaignsS)

  let firstPage                         = NavUser

  -- first time menu gets rendered with initial state argument
  (menuView, menuNavE)                  <- mainMenuComponent menuItems "Ad Platform" firstPage

  let postLoginNavE                     = fmap (const firstPage) validUserLoginE --(updates userS)
  let campaignNavE                      = fmap (const NavCampaign) (updates adsView)
  navS                                  <- stepperS NavLogin (postLoginNavE <> campaignNavE <> menuNavE)

  -- composition of keyboard listeners, looks like an inverse to Html signal distribution & flow
  subscribeEvent kbdEvents $ \e -> do
    print . showJS $ e

    nav <- pollBehavior (current navS)
    -- global listeners
    notifKbdSink e -- notifications always get keys to close from anywhere on esc etc

    -- local listeners
    case nav of
      NavImages -> imlibKbdSink e
      _         -> return ()

    return ()

  let mainView = rootLayout <$> navS
                            <*> menuView
                            <*> notifView
                            <*> busyView
                            <*> loginView
                            <*> userView
                            <*> adsView
                            <*> searchPageView
                            <*> createAdView
                            <*> imageLibView

  return (mainView, Just kbdSink)

main = adPlatform >>= runAppReactiveX
