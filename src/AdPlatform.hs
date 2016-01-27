{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module Main where

import           Prelude              hiding (div)
import qualified Prelude

import           Control.Applicative
import           Control.Concurrent   (forkIO, threadDelay)
import           Control.Lens         (over, set)
import           Control.Lens.TH      (makeLenses)
import           Control.Monad        (forM_, forever, unless)
import           Data.Default         (def)
import qualified Data.List
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Maybe           (fromMaybe)
import           Data.Monoid
import           Data.String          (fromString)

import           Data.JSString.Text   (textFromJSString)
import           GHCJS.Foreign.QQ     (js, jsu, jsu')
import           GHCJS.Types          (JSString, jsval)
import           Web.VirtualDom.Html.Attributes (class_, src, width)
import qualified Web.VirtualDom.Html.Attributes as A
import           Web.VirtualDom.Html   (Property, br, button, div, form, h1, hr,
                                       img, p, table, tbody, td, text, th,
                                       thead, tr)
import qualified Web.VirtualDom.Html   as E
import           Web.VirtualDom.Html.Events     (change, click, preventDefault,
                                       stopPropagation, submit, value)
-- import           GHCJS.VDOM.Unsafe    (Attributes', unsafeToAttributes)

import           Lubeck.App           (Html, runApp, runAppReactive)
import           Lubeck.Forms
import           Lubeck.FRP
import           Lubeck.Web.URI       (encodeURIComponent)

import qualified BD.Data.Account      as Account
import qualified BD.Data.Ad           as Ad
import qualified BD.Data.AdCampaign   as AdCampaign
import qualified BD.Data.Count        as C
import qualified BD.Data.Image        as Im
import           BD.Data.Interaction
import           BD.Data.SearchPost   (SearchPost)
import qualified BD.Data.SearchPost   as P
import           BD.Types
import           BD.Utils

import           Pages.PostSearch     (searchPage)
import           Pages.CreateAd       (createAdPage)
import           Pages.Login          (loginPage)
import           Pages.User           (userPage)
import           Pages.Campaign           (campaignPage)

import Components.ErrorMessages (errorMessagesComponent)
import Components.BusyIndicator (busyIndicatorComponent, BusyCmd(..))
import Components.MainMenu (mainMenuComponent)


row6H content = div [class_ "row"] [ div [class_ "col-md-6 col-lg-4 col-md-offset-3 col-lg-offset-4"] [content] ]
row12H content = div [class_ "row"] [ div [class_ "col-xs-12"] [content] ]

panel12H :: Html -> Html
panel12H bd =
  div [class_ "panel panel-default"]
    [ --div [class_ "panel-heading"] hd
     div [class_ "panel-body"] [bd]
    ]

contentPanel content = row12H $ panel12H content


imageLibraryPageW :: Widget [Im.Image] ()
imageLibraryPageW _ [] =
  contentPanel $ text "No images in library"

imageLibraryPageW _ ims =
  contentPanel $
    table [class_ "table table-striped table-hover"]
      [ tbody [] $ map (tr [] . map imageCell) (divide 5 ims) ]


imageCell img =
  let imgUrl = case Im.fb_thumb_url img of
        Nothing ->  Im.fb_image_url img
        Just url -> Just url
  in td [] [ imgFromWidthAndUrl' 150 (imgUrl) []
           , br [] []
           , showImagePred $ Im.prediction img
           , br [] []
           , text ("Hash: " <> (fromMaybe "none" $ Im.fb_image_hash img)) ]

showImagePred Nothing = text "No prediction"
showImagePred (Just x) = text $ "Score: "<>showJS x


-- BACKEND

getCampaigns :: Account.Account -> IO (Either AppError [AdCampaign.AdCampaign])
getCampaigns acc = AdCampaign.getUserCampaignsOrError (Account.username acc)

getImages :: Account.Account -> IO (Either AppError [Im.Image])
getImages acc = Im.getAllImagesOrError (Account.username acc)

eitherToError :: Sink (Maybe AppError) -> Either AppError a -> IO (Maybe a)
eitherToError sink (Left x)  = sink (Just x) >> return Nothing
eitherToError sink (Right x) = return (Just x)

withErrorSink :: Sink (Maybe AppError) -> Events (IO (Either AppError a)) -> Events a
withErrorSink errorSink bl = filterJust $ reactimate $ reactimate $ fmap (fmap (eitherToError errorSink)) bl

-- FIXME what about lazyness etc?
withBusy sink f = \x -> do
  sink PushBusy
  y <- f x
  sink PopBusy
  return y



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

  -- Login form
  (loginView, userLoginE) <- loginPage "forbestravelguide"

  let userE       = withErrorSink errorSink $ fmap (withBusy busySink Account.getUserOrError) userLoginE
  let camapaignsE = withErrorSink errorSink $ fmap (withBusy busySink getCampaigns) userE
  let imagesE     = withErrorSink errorSink $ fmap (withBusy busySink getImages) userE
  userS           <- stepperS Nothing (fmap Just userE)
  campaignsS      <- stepperS Nothing (fmap Just camapaignsE)
  imagesS         <- stepperS Nothing (fmap Just imagesE)

  -- User page
  let userAndCampaignsS = liftA2 (liftA2 (,)) userS campaignsS :: Signal (Maybe (Account.Account, [AdCampaign.AdCampaign]))
  (userView, loadAdsE) <- userPage userAndCampaignsS

  -- Create ad page
  createAdView <- createAdPage busySink errorSink (fmap (fmap Account.username) $ current userS)

  -- Campaign page
  adsView <- campaignPage errorSink busySink loadAdsE (current userS)


  -- Image library page
  let imageLibView = fmap ((altW mempty imageLibraryPageW) emptySink) imagesS


  -- Determines what page we are viewing
  let postLoginNavE = fmap (const NavUser) (updates userS)
  let campaignNavE = fmap (const NavCampaign) (updates adsView)
  navS <- stepperS NavLogin (postLoginNavE <> campaignNavE <> menuNavE)

  -- Integrate post search
  searchPageView <- searchPage busySink (fmap (fmap Account.username) $ current userS)

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
    wrap menu page = div [class_ "container"]
      [ div [] []
      , menu
      , div [class_ "col-xs-12 top-buffer"]
        [ div [] []
        , busy
        , errMsg
        , page
        ]
      ]


-- MAIN

main = do
  adPlatform >>= runAppReactive


-- UTILITY


imgFromWidthAndUrl' :: Int -> Maybe JSString -> [Property] -> Html
imgFromWidthAndUrl' w (Just url) attrs = img (attrs ++ [width w, src url, class_ "img-thumbnail"]) []
imgFromWidthAndUrl' w Nothing attrs = text "No URL"


showJS :: Show a => a -> JSString
showJS = fromString . show

-- -- | A limitation in ghcjs-vdom means that non-standard attributes aren't always defined properly.
-- -- This works around the issue. The value returned here should take the place of the standard
-- -- attribute definition, i.e. instead of (div () ..) or (div [..] ..), use (div (customAttrs []) ..).
-- --
-- -- If possible, use the functions exported by GHCJS.VDOM.Attribute instead.
-- customAttrs :: Map String String -> Attributes'
-- customAttrs attrs = let str = (fromString $ ("{"++) $ (++"}") $ drop 2 $ Map.foldWithKey (\k v s -> s++", "++show k++":"++show v) "" attrs) :: JSString
--   in unsafeToAttributes [jsu'| {attributes:JSON.parse(`str)} |]

tableHeaders :: [JSString] -> Html
tableHeaders hs = thead [] [ tr [] $ map (th [] . (:[]) . text) hs]
