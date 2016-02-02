{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

module Main where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import           Data.Monoid

import           Web.VirtualDom.Html            (Property, br, button, div,
                                                 form, h1, hr, img, p, table,
                                                 tbody, td, text, th, thead, tr)
import qualified Web.VirtualDom.Html            as E
import           Web.VirtualDom.Html.Attributes (class_, src, width)
import qualified Web.VirtualDom.Html.Attributes as A
import           Web.VirtualDom.Html.Events     (change, click, preventDefault,
                                                 stopPropagation, submit, value)

import           Lubeck.App                     (Html, runAppReactive)
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

import           Pages.Campaign                 (campaignPage, getCampaigns)
import           Pages.CreateAd                 (createAdPage)
import           Pages.ImageLibrary             (imageLibraryPage, getImages)
import           Pages.Login                    (loginPage)
import           Pages.PostSearch               (searchPage)
import           Pages.User                     (userPage)

import           Components.BusyIndicator       (BusyCmd (..), withBusy,
                                                 busyIndicatorComponent)
import           Components.ErrorMessages       (errorMessagesComponent)
import           Components.MainMenu            (mainMenuComponent)

import           Lubeck.Util


defaultUsername = "forbestravelguide"

menuItems =
  [ (NavUser,     "User")
  , (NavSearch,   "Search")
  , (NavImages,   "Image Library")
  , (NavCreateAd, "Create Ad")
  , (NavLogin,    "Logout") -- last item is special in that it will be positioned far right
  ]

rootLayout goTo menu err busy login user ads search createAd imlib = case goTo of
  NavLogin    -> layout mempty busy err login
  NavUser     -> layout menu   busy err user
  NavCampaign -> layout menu   busy err ads
  NavSearch   -> layout menu   busy err search
  NavCreateAd -> layout menu   busy err createAd
  NavImages   -> layout menu   busy err imlib
  where
    layout menu busy err page =
      div [class_ "container"]
        [ menu
        , div [class_ "col-xs-12 top-buffer"]
          [ busy
          , err
          , page
          ] ]


adPlatform :: IO (Signal Html)
adPlatform = do
  (menuView, menuNavE)    <- mainMenuComponent menuItems "Ad Platform" NavLogin
  (errorsView, errorSink) <- errorMessagesComponent []
  (busyView, busySink)    <- busyIndicatorComponent []

  (loginView, userLoginE) <- loginPage                       defaultUsername

  let userE               = withError errorSink $ fmap (withBusy busySink Account.getUserOrError) userLoginE
  let camapaignsE         = withError errorSink $ fmap (withBusy busySink getCampaigns) userE
  -- let imagesE             = withError errorSink $ fmap (withBusy busySink getImages) userE
  userS                   <- stepperS Nothing (fmap Just userE)
  campaignsS              <- stepperS Nothing (fmap Just camapaignsE)
  -- imagesS                 <- stepperS Nothing (fmap Just imagesE)
  let userAndCampaignsS   = liftA2 (liftA2 (,)) userS campaignsS :: Signal (Maybe (Account.Account, [AdCampaign.AdCampaign]))
  let usernameB           = fmap (fmap Account.username) $ current userS

  (userView, loadAdsE)    <- userPage                        userAndCampaignsS
  createAdView            <- createAdPage busySink errorSink usernameB
  adsView                 <- campaignPage busySink errorSink loadAdsE (current userS)
  imageLibView            <- imageLibraryPage busySink errorSink userE
  searchPageView          <- searchPage   busySink errorSink usernameB

  let postLoginNavE       = fmap (const NavUser) (updates userS)
  let campaignNavE        = fmap (const NavCampaign) (updates adsView)
  navS                    <- stepperS NavLogin (postLoginNavE <> campaignNavE <> menuNavE)

  let mainView = rootLayout <$> navS
                          <*> menuView
                          <*> errorsView
                          <*> busyView
                          <*> loginView
                          <*> userView
                          <*> adsView
                          <*> searchPageView
                          <*> createAdView
                          <*> imageLibView

  return mainView

main = adPlatform >>= runAppReactive
