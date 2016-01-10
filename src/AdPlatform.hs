
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
import Lubeck.App (Html, runApp)
import Lubeck.Forms (Widget, Widget')
import Lubeck.Web.URI (encodeURIComponent)

import qualified BD.Data.Account as A
import qualified BD.Data.AdCampaign as AC
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
  | GotUser A.Account
  | Then Action Action
  | GoTo ViewSection

-- -- For debugging only
-- instance Show Action where
--   show = g where
--     g LoginGo     = "LoginGo"
--     g Logout      = "Logout"
--     g (GotUser _) = "GotUser"
--     g (Pure _)    = "Pure"
--     g (Then _ _)  = "Then"
--     g (GoTo _)    = "GoTo"

data Model = NotLoggedIn { _loginPage :: LoginPage}
           | LoadingUser
           | AsUser { _user :: A.Account
                    , _userModel :: UserModel }

data ViewSection = UserView
                 | CampaignView { _campaignIx :: Int
                                , _campaignAds :: (Maybe [Ad.Ad]) }
              -- | CampaignsImageLibrary

data LoginPage = LoginPage { _loginUsername :: JSString
                           , _loginPass :: JSString }

data UserModel = UserModel { _campaigns :: [AC.AdCampaign]
                           , _viewSection :: ViewSection }

makeLenses ''Model
makeLenses ''LoginPage
makeLenses ''UserModel
makeLenses ''ViewSection

update :: Events Action -> IO (Behavior (Model, Maybe (IO Action)))
update = foldpR step initial
  where
    initial = (NotLoggedIn (LoginPage "forbestravelguide" "bar"), Nothing)

    step LoginGo              (NotLoggedIn lp,_) = (LoadingUser, Just $ loginUser lp)
    step LoginGo              (m,_) = (m, Nothing)
    step (Pure f)             (m,_) = (f m, Nothing)
    step Logout               (_,_) = initial
    step (GotUser acc)        (_,_) = (AsUser acc (UserModel [] UserView), Just $ getCampaigns acc)
    step (GoTo vs)            (m,_) = (set (userModel . viewSection) vs m, goToViewSection vs m)

    goToViewSection (CampaignView n Nothing) model
      = Just $ loadAds n model
    goToViewSection _ model
      = Nothing


render :: Widget Model Action
render sink LoadingUser = text "Loading User"
render sink (NotLoggedIn lp) = loginPageW sink lp
render sink (AsUser acc (UserModel camps UserView)) = div
  ( customAttrs $ Map.fromList [("style", "width: 600px; margin-left: auto; margin-right: auto") ])
  [ h1 () [text "Hello"]
  , div ()
    [text $ A.username acc ]
  , div ()
    [text $ showJS $ A.latest_count acc ]
  , div ()
    [ text "number of campaigns: "
    , text $ showJS (length camps)]
  , campaignTable sink camps
  , menu sink acc
  ]
render sink (AsUser acc (UserModel camps (CampaignView ix mads))) =
  let camp = camps !! ix
  in div ()
      [ h1 () [text $ AC.campaign_name camp]
      , div ()
        [text "daily budget:"
        , text $ showJS $ AC.daily_budget camp ]
      , renderAdList sink mads
      , menu sink acc
      ]

renderAdList :: Widget (Maybe [Ad.Ad]) Action
renderAdList sink Nothing = text "Loading ads"
renderAdList sink (Just ads) = table () [
    tableHeaders ["FB adset id", "Name", "Budget"]
  , tbody () (map (adRow sink) ads)
  ]

adRow :: Widget Ad.Ad Action
adRow sink ad = tr ()
  [ td () [text $ showJS $ Ad.fb_adset_id ad]
  , td () [text $ Ad.ad_title ad]
  , td () [text $ showJS $ Ad.current_budget ad]
  ]

tableHeaders :: [JSString] -> Html
tableHeaders hs = thead () [ tr () $ map (th () . (:[]) . text) hs]

menu :: Widget A.Account Action
menu sink acc = div ()
  [ E.h2 () $ text "Menu"
  , E.ul ()
    [ E.li () $ E.a (A.href $ "/posts/?user=" <> encodeURIComponent (A.username acc)) [text "Search"]
    , E.li () $ E.a (click $ \_ -> sink $ GoTo UserView) [text "User"]
    , E.li () $ E.a (click $ \_ -> sink Logout) [text "Logout"]
    ]
  ]

campaignTable :: Widget [AC.AdCampaign] Action
campaignTable sink camps = table () [
    tableHeaders ["FB id", "Name", ""]
  , tbody () (map (campaignRow sink) $ zip [0..] camps)
  ]

campaignRow :: Widget (Int, AC.AdCampaign) Action
campaignRow sink (ix, camp) = tr ()
  [ td () [text $ showJS $ AC.fbid camp]
  , td () [text $ AC.campaign_name camp]
  , td () [E.a (click $ \_ -> sink $ GoTo (CampaignView ix Nothing)) [text "view"]]
  ]

loginPageW :: Widget LoginPage Action
loginPageW sink (LoginPage u pw) = form
  [ submit $ \e -> preventDefault e >> return () ]
  [
    -- E.input [ change $ \e -> [jsu|console.log(`e)|] ] [text "abc"]
  -- ,
    E.input [A.value u,
             change $ \e -> preventDefault e >> sink (Pure (set (loginPage . loginUsername) (value e)))] ()
   , button (click $ \_ -> sink LoginGo) [text "Login"] ]


-- BACKEND

loginUser :: LoginPage -> IO Action
loginUser (LoginPage s _) = do
  u <- A.getUser s
  return $ GotUser u

getCampaigns :: A.Account -> IO Action
getCampaigns acc = do
  cs <- AC.getUserCampaigns $ A.username acc
  return $ Pure $ set (userModel . campaigns) cs

loadAds :: Int -> Model -> IO Action
loadAds n model =  do
  let campid = showJS $ AC.fbid $ (_campaigns $ _userModel $ model)!!n
      username = A.username $ _user model
  ads <- Ad.getCampaignAds username campid
  return $ Pure $ set (userModel . viewSection . campaignAds) (Just ads)

-- MAIN

main :: IO ()
main = runApp update render


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
