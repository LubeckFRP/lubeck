{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE JavaScriptFFI       #-}

module BDPlatform.Main (main) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative

import           Data.Monoid
import           Data.String                    (fromString)
import           GHCJS.Types                    (JSString, JSVal)
import           GHCJS.Concurrent               (synchronously)

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

import           BDPlatform.Pages.Campaign      (campaignPage, getCampaigns)
import           BDPlatform.Pages.CreateAd      (createAdPage)
import           BDPlatform.Pages.ImageLibrary  (imageLibraryPage)
import           BDPlatform.Pages.Login         (loginPage, Username)
import           BDPlatform.Pages.PostSearch    (searchPage)
import           BDPlatform.Pages.AccountSearch (accountSearchPage)
import           BDPlatform.Pages.User          (userPage)
import           BDPlatform.Pages.Interactions  (interactionsMain)

import           Components.BusyIndicator       (BusyCmd (..), withBusy,
                                                 busyIndicatorComponent)
import           Components.Notifications       (notificationsComponent)
import           Components.MainMenu            (mainMenuComponent, MenuItems())

import           Lubeck.Util
import           Lubeck.Types
import           BDPlatform.Types
import           BDPlatform.Config


menuItems :: MenuItems Nav
menuItems =
  [ (NavUser,          "User")
  , (NavSearch,        "Search")
  , (NavImages,        "Image Library")
  , (NavCreateAd,      "Create Ad")
  , (NavInteractions,  "Shoutout browser")
  , (NavAccountSearch, "Account search")
  , (NavLogin,         "Logout") -- last item is special in that it will be positioned far right
  ]

rootLayout goTo menu err busy login user ads search createAd imlib interactions accountSearchPageView = case goTo of
  NavLogin         -> layoutLogin busy err login
  NavUser          -> layout menu busy err user
  NavCampaign      -> layout menu busy err ads
  NavSearch        -> layout menu busy err search
  NavCreateAd      -> layout menu busy err createAd
  NavImages        -> layout menu busy err imlib
  NavInteractions  -> layout menu busy err interactions
  NavAccountSearch -> layout menu busy err accountSearchPageView
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
  (kbdSink', kbdEvents)                  <- newEventOf (undefined :: KbdEvents)
  (ipcSink', ipcEvents)                  <- newEventOf (undefined :: IPCMessage)

  let ipcSink = synchronously . ipcSink'
  let kbdSink = synchronously . kbdSink'

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
  -- searchPageView                        <- searchPage       busySink notifSink ipcSink           usernameB
  createAdView                          <- createAdPage     busySink notifSink                   usernameB imsB (current campaignsS)

  interactionsView                      <- interactionsMain busySink notifSink

  let firstPage                         = NavUser

  -- first time menu gets rendered with initial state argument
  (menuView, menuNavE)                  <- mainMenuComponent menuItems "BD Platform" firstPage

  let postLoginNavE                     = fmap (const firstPage) validUserLoginE --(updates userS)
  let campaignNavE                      = fmap (const NavCampaign) (updates adsView)
  navS                                  <- stepperS NavLogin (postLoginNavE <> campaignNavE <> menuNavE)

  searchPageView                        <- searchPage        busySink notifSink ipcSink           usernameB navS
  accountSearchPageView                 <- accountSearchPage busySink notifSink ipcSink           usernameB navS

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
                            <*> interactionsView
                            <*> accountSearchPageView

  return (mainView, Just kbdSink)
  where
    sample = snapshotWith const

main = adPlatform >>= runAppReactiveX
