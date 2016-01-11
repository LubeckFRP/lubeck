
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import Control.Applicative
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forM_, forever, unless)
import Data.String (fromString)
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.List
import Data.Monoid
import Data.Maybe(fromMaybe)
import Data.Default (def)
import Control.Lens (over, set)
import Control.Lens.TH(makeLenses)

import GHCJS.Foreign.QQ (js, jsu, jsu')
import GHCJS.Types(JSString, jsval)
import GHCJS.VDOM.Event (click, change, submit, stopPropagation, preventDefault, value)
import GHCJS.VDOM.Element (p, h1, div, text, form, button, img, hr, custom, table, td, tr, th, tbody, thead)
import GHCJS.VDOM.Attribute (src, width, class_)
import qualified GHCJS.VDOM.Element as E
import qualified GHCJS.VDOM.Attribute as A
import GHCJS.VDOM.Unsafe (unsafeToAttributes, Attributes')
import Data.JSString.Text (textFromJSString)

import Lubeck.FRP
import Lubeck.App (Html, runApp, runAppReactive)
import Lubeck.Forms
import Lubeck.Web.URI (encodeURIComponent)

import PostSearch (searchPage)

import qualified BD.Data.Account as Account
import qualified BD.Data.AdCampaign as AdCampaign
import qualified BD.Data.Ad as Ad
import qualified BD.Data.Count as C
import qualified BD.Data.SearchPost as P
import BD.Data.SearchPost(SearchPost)
import BD.Data.Interaction
import BD.Types

data Nav = NavLogin | NavUser | NavCampaign | NavSearch
  deriving (Show, Eq)

menu :: Widget' Nav
menu sink value = div ()
  [ E.h2 () $ text "Menu"
  , E.ul ()
    [ E.li () $ E.a (click $ \_ -> sink $ NavSearch) [text "Search"]
    , E.li () $ E.a (click $ \_ -> sink $ NavUser)   [text "User"]
    , E.li () $ E.a (click $ \_ -> sink $ NavLogin)  [text "Logout"]
    ]
  ]

loginPageW :: Widget JSString (Submit JSString)
loginPageW sink name = form
  [ submit $ \e -> preventDefault e >> return () ]
  [ E.input [A.value name, change $ \e -> preventDefault e >> sink (DontSubmit $ value e)] ()
   , button (click $ \_ -> sink (Submit name)) [text "Login"] ]

-- | Display user information and current campaings.
-- Emits campaign to view.
userPageW :: Widget (Account.Account, [AdCampaign.AdCampaign]) AdCampaign.AdCampaign
userPageW sink (acc, camps) =
  div
  ( customAttrs $ Map.fromList [("style", "width: 600px; margin-left: auto; margin-right: auto") ])
  [ h1 () [text "Hello"]
  , div ()
    [text $ Account.username acc ]
  , div ()
    [text $ showJS $ Account.latest_count acc ]
  , div ()
    [ text "number of campaigns: "
    , text $ showJS (length camps)]
  , campaignTable sink camps
  ]
  where
    campaignTable :: Widget [AdCampaign.AdCampaign] AdCampaign.AdCampaign
    campaignTable sink camps = table () [
        tableHeaders ["FB id", "Name", ""]
      , tbody () (map (campaignRow sink) $ zip [0..] camps)
      ]

    campaignRow :: Widget (Int, AdCampaign.AdCampaign) AdCampaign.AdCampaign
    campaignRow sink (ix, camp) = tr ()
      [ td () [text $ showJS $ AdCampaign.fbid camp]
      , td () [text $ AdCampaign.campaign_name camp]
      , td () [E.a (click $ \_ -> sink camp) [text "view"]]
      ]

-- | Display info about a campaign.
campaignPageW :: Widget (AdCampaign.AdCampaign, [Ad.Ad]) ()
campaignPageW sink (camp, ads) =
  div ()
      [ h1 () [text $ AdCampaign.campaign_name camp]
      , div ()
        [text "daily budget:"
        , text $ showJS $ AdCampaign.daily_budget camp ]
      , renderAdList emptySink ads
      ]
  where
    renderAdList :: Widget [Ad.Ad] ()
    renderAdList _ ads = table () [
        tableHeaders ["FB adset id", "Name", "Budget"]
      , tbody () (map (adRow emptySink) ads)
      ]

    adRow :: Widget Ad.Ad ()
    adRow _ ad = tr ()
      [ td () [text $ showJS $ Ad.fb_adset_id ad]
      , td () [text $ Ad.ad_title ad]
      , td () [text $ showJS $ Ad.current_budget ad]
      ]

-- BACKEND

getCampaigns :: Account.Account -> IO [AdCampaign.AdCampaign]
getCampaigns acc = do
  AdCampaign.getUserCampaigns $ Account.username acc

loadAds :: Maybe (Account.Account) -> AdCampaign.AdCampaign -> IO [Ad.Ad]
loadAds account camp =  do
  let campid = showJS $ AdCampaign.fbid camp
      username = maybe "" Account.username $ account
  Ad.getCampaignAds username campid

adPlatform :: IO (Signal Html)
adPlatform = do
  -- Menu
  (menuView, menuNavE) <- component NavLogin menu

  -- Login form
  (loginView, userLoginE) <- formComponent "forbestravelguide" loginPageW
  let userE       = reactimate $ fmap Account.getUser userLoginE
  let camapaignsE = reactimate $ fmap getCampaigns userE
  userS      <- stepperS Nothing (fmap Just userE)
  campaignsS <- stepperS Nothing (fmap Just camapaignsE)

  -- User page
  (fetchCampaignAds, loadAdsE) <- newEvent
  let userAndCampaignsS = liftA2 (liftA2 (,)) userS campaignsS :: Signal (Maybe (Account.Account, [AdCampaign.AdCampaign]))
  let userView = fmap ((altW mempty userPageW) fetchCampaignAds) userAndCampaignsS

  -- Campaign page
  let adsE = reactimate $ snapshotWith loadAds (current userS) loadAdsE
  latestLoadedCampaignS <- stepperS Nothing (fmap Just loadAdsE) :: IO (Signal (Maybe AdCampaign.AdCampaign))
  adsS <- stepperS Nothing (fmap Just adsE) :: IO (Signal (Maybe [Ad.Ad]))
  let lastestAndAdsS = liftA2 (liftA2 (,)) latestLoadedCampaignS adsS :: (Signal (Maybe (AdCampaign.AdCampaign, [Ad.Ad])))
  let adsView = fmap ((altW mempty campaignPageW) emptySink) lastestAndAdsS

  -- Determines what page we are viewing
  let postLoginNavE = fmap (const NavUser) (updates userS)
  navS <- stepperS NavLogin (postLoginNavE <> menuNavE)

  -- Integrate post search
  searchPageView <- searchPage (current userS)

  let view = liftA2 (\nav rest -> h1 ()
            [ text "Ad platform: "
            , rest
            ])
            navS
            (mconcat [menuView, loginView, userView, adsView, searchPageView])

  return view



-- MAIN

main = do
  (adPlatformView,_,_) <- adPlatform
  runAppReactive adPlatformView
-- main :: IO ()
-- main = runApp update render


-- UTILITY

showJS :: Show a => a -> JSString
showJS = fromString . show

-- | A limitation in ghcjs-vdom means that non-standard attributes aren't always defined properly.
-- This works around the issue. The value returned here should take the place of the standard
-- attribute definition, i.e. instead of (div () ..) or (div [..] ..), use (div (customAttrs []) ..).
--
-- If possible, use the functions exported by GHCJS.VDOM.Attribute instead.
customAttrs :: Map String String -> Attributes'
customAttrs attrs = let str = (fromString $ ("{"++) $ (++"}") $ drop 2 $ Map.foldWithKey (\k v s -> s++", "++show k++":"++show v) "" attrs) :: JSString
  in unsafeToAttributes [jsu'| {attributes:JSON.parse(`str)} |]

tableHeaders :: [JSString] -> Html
tableHeaders hs = thead () [ tr () $ map (th () . (:[]) . text) hs]

-- | Modify a widget to accept 'Maybe' and displays the text nothing on 'Nothing'.
altW :: Html -> Widget a b -> Widget (Maybe a) b
altW alt w s Nothing  = alt
altW alt w s (Just x) = w s x
