
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

import qualified BD.Data.Account as Account
import qualified BD.Data.AdCampaign as AdCampaign
import qualified BD.Data.Ad as Ad
import qualified BD.Data.Count as C
import qualified BD.Data.SearchPost as P
import BD.Data.SearchPost(SearchPost)
import BD.Data.Interaction
import BD.Types

data Action
  = LoginGo
  | Logout
  | Pure (Model -> Model)
  | GotUser Account.Account
  | GoTo ViewSection

-- -- For debugging only
-- instance Show Action where
--   show = g where
--     g LoginGo     = "LoginGo"
--     g Logout      = "Logout"
--     g (GotUser _) = "GotUser"
--     g (Pure _)    = "Pure"
--     g (GoTo _)    = "GoTo"

data Model = NotLoggedIn { _loginPage :: LoginPage}
           | LoadingUser
           | AsUser { _user :: Account.Account
                    , _userModel :: UserModel }

data ViewSection = UserView
                 | CampaignView { _campaignIx :: Int
                                , _campaignAds :: (Maybe [Ad.Ad]) }
              -- | CampaignsImageLibrary

data LoginPage = LoginPage { _loginUsername :: JSString
                           , _loginPass :: JSString }

data UserModel = UserModel { _campaigns :: [AdCampaign.AdCampaign]
                           , _viewSection :: ViewSection }

makeLenses ''Model
makeLenses ''LoginPage
makeLenses ''UserModel
makeLenses ''ViewSection

update :: Events Action -> IO (Behavior (Model, Maybe (IO Action)))
update = foldpR step initial
  where
    initial = (NotLoggedIn (LoginPage "forbestravelguide" "bar"), Nothing)

    -- step LoginGo              (NotLoggedIn lp,_) = (LoadingUser, Just $ loginUser lp)
    step LoginGo              (m,_) = (m, Nothing)
    -- step (GotUser acc)        (_,_) = (AsUser acc (UserModel [] UserView), Just $ getCampaigns acc)
    step Logout               (_,_) = initial
    step (GoTo vs)            (m,_) = (set (userModel . viewSection) vs m, goToViewSection vs m)
    step (Pure f)             (m,_) = (f m, Nothing)

    goToViewSection (CampaignView n Nothing) model
      = Just $ loadAds n model
    goToViewSection _ model
      = Nothing


-- render :: Widget Model Action
-- render sink LoadingUser = text "Loading User"
-- render sink (NotLoggedIn lp) = loginPageW sink lp

-- render sink (AsUser acc (UserModel camps UserView)) =
userPageW :: Widget (Account.Account, [AdCampaign.AdCampaign]) (AdCampaign.AdCampaign)
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
    campaignTable :: Widget [AdCampaign.AdCampaign] Action
    campaignTable sink camps = table () [
        tableHeaders ["FB id", "Name", ""]
      , tbody () (map (campaignRow sink) $ zip [0..] camps)
      ]

    campaignRow :: Widget (Int, AdCampaign.AdCampaign) Action
    campaignRow sink (ix, camp) = tr ()
      [ td () [text $ showJS $ AdCampaign.fbid camp]
      , td () [text $ AdCampaign.campaign_name camp]
      , td () [E.a (click $ \_ -> sink camp) [text "view"]]
      ]


-- render sink (AsUser acc (UserModel camps (CampaignView ix mads))) =

campaignPageW :: Widget (AdCampaign.AdCampaign, Maybe [Ad.Ad]) ()
campaignPageW sink (camp, mads) =
  div ()
      [ h1 () [text $ AdCampaign.campaign_name camp]
      , div ()
        [text "daily budget:"
        , text $ showJS $ AdCampaign.daily_budget camp ]
      , renderAdList emptySink mads
      ]
  where
    renderAdList :: Widget (Maybe [Ad.Ad]) ()
    renderAdList _ Nothing = text "Loading ads"
    renderAdList _ (Just ads) = table () [
        tableHeaders ["FB adset id", "Name", "Budget"]
      , tbody () (map (adRow emptySink) ads)
      ]

    adRow :: Widget Ad.Ad ()
    adRow _ ad = tr ()
      [ td () [text $ showJS $ Ad.fb_adset_id ad]
      , td () [text $ Ad.ad_title ad]
      , td () [text $ showJS $ Ad.current_budget ad]
      ]






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

tableHeaders :: [JSString] -> Html
tableHeaders hs = thead () [ tr () $ map (th () . (:[]) . text) hs]



-- BCampaignKEND

-- loginUser :: JSString -> IO Account.Account
-- loginUser s = do
--   Account.getUser s

getCampaigns :: Account.Account -> IO [AdCampaign.AdCampaign]
getCampaigns acc = do
  AdCampaign.getUserCampaigns $ Account.username acc

loadAds :: Int -> Model -> IO Action
loadAds n model =  do
  let campid = showJS $ AdCampaign.fbid $ (_campaigns $ _userModel $ model)!!n
      username = Account.username $ _user model
  ads <- Ad.getCampaignAds username campid
  return $ Pure $ set (userModel . viewSection . campaignAds) (Just ads)


data Nav = NavLogin | NavUser | NavCampaign | NavSearch
  deriving (Show, Eq)

adPlatform :: IO (Signal Html, Signal Nav, Signal (Maybe (Account.Account)))
adPlatform = do
  -- Menu
  (menuView, menuNavE) <- component NavLogin menu

  -- Login form
  (loginView, userLoginE) <- formComponent "" loginPageW
  let userE       = reactimate $ fmap Account.getUser userLoginE
  let camapaignsE = reactimate $ fmap getCampaigns userE
  userS      <- stepperS Nothing (fmap Just userE)
  campaignsS <- stepperS Nothing (fmap Just camapaignsE)

  -- User page
  -- Campaign page

  -- Determines what page we are viewing
  let postLoginNavE = fmap (const NavUser) (updates userS)
  navS <- stepperS NavLogin (postLoginNavE <> menuNavE)

  let view = liftA2 (\nav rest -> h1 ()
            [ text "Ad platform: "
            , text (showJS nav)
            , rest
            ])
            navS
            (mconcat [menuView, loginView])
  return (view, navS, userS)

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
