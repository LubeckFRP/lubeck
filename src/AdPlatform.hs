{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
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

import           Pages.Campaign                 (campaignPage)
import           Pages.CreateAd                 (createAdPage)
import           Pages.ImageLibrary             (imageLibraryPage)
import           Pages.Login                    (loginPage)
import           Pages.PostSearch               (searchPage)
import           Pages.User                     (userPage)

import           Components.BusyIndicator       (BusyCmd (..),
                                                 withBusy,
                                                 busyIndicatorComponent)
import           Components.ErrorMessages       (errorMessagesComponent)
import           Components.MainMenu            (mainMenuComponent)

import           Lib.Helpers


getCampaigns :: Account.Account -> IO (Either AppError [AdCampaign.AdCampaign])
getCampaigns acc = AdCampaign.getUserCampaignsOrError (Account.username acc)

getImages :: Account.Account -> IO (Either AppError [Im.Image])
getImages acc = Im.getAllImagesOrError (Account.username acc)

menuItems =
  [ (NavUser,     "User")
  , (NavSearch,   "Search")
  , (NavImages,   "Image Library")
  , (NavCreateAd, "Create Ad")
  , (NavLogin, "  Logout")
  ]

adPlatform :: IO (Signal Html)
adPlatform = do
  (menuView, menuNavE)    <- mainMenuComponent menuItems NavLogin
  (errorsView, errorSink) <- errorMessagesComponent ([] :: [AppError])
  (busyView, busySink)    <- busyIndicatorComponent []

  (loginView, userLoginE) <- loginPage "forbestravelguide"

  let userE               = withErrorSink errorSink $ fmap (withBusy busySink Account.getUserOrError) userLoginE
  let camapaignsE         = withErrorSink errorSink $ fmap (withBusy busySink getCampaigns) userE
  let imagesE             = withErrorSink errorSink $ fmap (withBusy busySink getImages) userE
  userS                   <- stepperS Nothing (fmap Just userE)
  campaignsS              <- stepperS Nothing (fmap Just camapaignsE)
  imagesS                 <- stepperS Nothing (fmap Just imagesE)
  let userAndCampaignsS   = liftA2 (liftA2 (,)) userS campaignsS :: Signal (Maybe (Account.Account, [AdCampaign.AdCampaign]))
  let usernameB           = fmap (fmap Account.username) $ current userS

  (userView, loadAdsE)    <- userPage userAndCampaignsS
  createAdView            <- createAdPage busySink errorSink usernameB
  adsView                 <- campaignPage busySink errorSink loadAdsE (current userS)
  imageLibView            <- imageLibraryPage imagesS
  searchPageView          <- searchPage busySink usernameB

  let postLoginNavE       = fmap (const NavUser) (updates userS)
  let campaignNavE        = fmap (const NavCampaign) (updates adsView)
  navS                    <- stepperS NavLogin (postLoginNavE <> campaignNavE <> menuNavE)

  let view = nav <$> navS <*> menuView
                          <*> errorsView
                          <*> busyView
                          <*> loginView
                          <*> userView
                          <*> adsView
                          <*> searchPageView
                          <*> createAdView
                          <*> imageLibView

  return view

nav goTo menu errMsg busy login user ads search createAd imlib = case goTo of
  NavLogin    -> wrap mempty login
  NavUser     -> wrap menu user
  NavCampaign -> wrap menu ads
  NavSearch   -> wrap menu search
  NavCreateAd -> wrap menu createAd
  NavImages   -> wrap menu imlib
  where
    wrap menu page =
      div [class_ "container"]
        [ menu
        , div [class_ "col-xs-12 top-buffer"]
          [ busy
          , errMsg
          , page
          ] ]


main = adPlatform >>= runAppReactive
