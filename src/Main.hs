
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

import Prelude hiding (div)
import qualified Prelude

import Control.Applicative
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forM_, forever, unless)
import Data.String (fromString)
import qualified Data.Map as Map
import Data.Map(Map)
import Control.Monad.STM (atomically)
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Data.Text
import qualified Data.List
import Data.Text(Text)
import Data.Monoid
import Data.Maybe(fromMaybe)
import Data.Default (def)

--import Data.JSString.Text

import GHCJS.VDOM.Event (click, change, submit, stopPropagation, preventDefault)
import GHCJS.Foreign.QQ (js, jsu, jsu')
import GHCJS.Types(JSString, jsval)
import GHCJS.VDOM.Element (p, h1, div, text, form, button, img, hr, custom)
import GHCJS.VDOM.Attribute (src, width, class_)
import qualified GHCJS.VDOM.Element as E
import qualified GHCJS.VDOM.Attribute as A
import GHCJS.VDOM.Unsafe (unsafeToAttributes, Attributes')
import Data.JSString.Text (textFromJSString)

import FRP2
import App

import qualified BD.Data.Account as A
import qualified BD.Data.Count as C
import qualified BD.Data.SearchPost as P
import BD.Data.SearchPost(SearchPost)
import BD.Data.Interaction


type Widget i o = Sink o -> i -> Html

type Campaign = ()

type Account = A.Account

username :: Account -> Text
username = A.username

data Action
  = LoginGo
  | LoginName JSString
  | Logout
  | GotUser Account
--  | GotCampaigns [Campaign]
--  | GoToCampaign Int

-- For debugging only
instance Show Action where
  show = g where
    g LoginGo         = "LoginGo"
    g Logout         = "Logout"
    g (LoginName _) = "LoginName"
    g (GotUser _) = "GotUser"

data Model = NotLoggedIn JSString
           | LoadingUser
           | AsUser Account UserModel

data ViewSection = UserView -- | CampaignsImageLibrary | Campaign Int

data UserModel = UserModel { campaigns :: [Campaign]
                           , viewSection :: ViewSection }

update :: E Action -> IO (R (Model, Maybe (IO Action)))
update = foldpR step initial
  where
    initial = (NotLoggedIn "", Nothing)

    step (LoginName s)        (NotLoggedIn _,_) = (NotLoggedIn s,Nothing)
    step (LoginName s)        (m,_) = (m,Nothing)
    step LoginGo              (NotLoggedIn s,_) = (LoadingUser, Just $ loginUser "forbestravelguide")
    step LoginGo              (m,_) = (m, Nothing)
    step Logout               (_,_) = (NotLoggedIn "", Nothing)
    step (GotUser acc)        (_,_) = (AsUser acc (UserModel [] UserView), Nothing)

--    step (LoadAction a b)     (model,_) = (model,Just $ fmap ReplaceModel (loadShoutouts a b))
--    step (ReplaceModel model) (_,_)     = (model,Nothing)

render :: Widget Model Action
render sink LoadingUser = text "Loading User"
render sink (NotLoggedIn s) = form
  [ submit $ \e -> preventDefault e >> return () ]
  [
    -- E.input [ change $ \e -> [jsu|console.log(`e)|] ] [text "abc"]
  -- ,
    E.input () [text s]
  , button (click $ \_ -> sink LoginGo) [text "Login"] ]

render sink (AsUser acc (UserModel _ UserView)) = div
  (customAttrs $ Map.fromList [("style", "width: 600px; margin-left: auto; margin-right: auto") ])
  [ h1 () [text "Hello"]
  , div ()
    [text $ textToJSString $ username acc ]
  , div ()
    [text $ showJS $ A.latest_count acc ]
  ]

loginUser :: JSString -> IO Action
loginUser s = do
  u <- A.getUser s
  return $ GotUser u

-- MAIN

main :: IO ()
main = runApp update render


-- UTILITY

(.:)  :: a -> (a -> b) -> b
(.:)  x f = f x

(.:?) :: Maybe a -> (a -> b) -> Maybe b
(.:?) x f = fmap f x

showJS :: Show a => a -> JSString
showJS = fromString . show

textToJSString :: Text -> JSString
textToJSString = fromString . Data.Text.unpack

-- A data URL representing a grey image
greyImgUrl :: JSString
greyImgUrl = "data:image/jpeg;base64,/9j/4AAQSkZJRgABAQAAAQABAAD/2wCEAAkGBxQSEhQUEhQUFBQUFBQUFBQUFBQUFBQUFBQXFxQUFBQYHCggGBwlHBQUITEhJSksLi4uFx8zODMsNygtLiwBCgoKDAwMDgwMDiwZFBksLCwsKywsLDc3Kyw3LCwsLDcsNzcsNyssLCwsLDc3LDcsLCwsLDcsNyw3NzcsNyw3LP/AABEIAOEA4QMBIgACEQEDEQH/xAAYAAEBAQEBAAAAAAAAAAAAAAAAAQIDB//EABkQAQEBAQEBAAAAAAAAAAAAAAABEQJBMf/EABUBAQEAAAAAAAAAAAAAAAAAAAAB/8QAFBEBAAAAAAAAAAAAAAAAAAAAAP/aAAwDAQACEQMRAD8A9QoqIpCCwRKjVTAIBgGqIAqKKCsiKmqCoEBEqiAWhQBFQDVRoEZrVSgzgqCurLWIIKkUBFQBUxQQi4AoEFQVBC1FMASBAEVJABQERSAAoIjTPQM6ADrWWkAVFACmgAAAAsEUBFqABFoJaACCgIilBAAWEIAJ0RKDI1gDdSrUAVFAAACAAACiAqKlAEAFEAAoIKgBQAWJFARUoICA6AgC0KBSIsAAAIRQVAASqlBAAVCKAlWoBAQBFQFixIArPSpQAQHSoqAsEXAAAAAFQBQQACAgAJjSRQTRYgFhUKCKigasTFBWbFS0EEAbqWrTAIuoAoAARYBAqSAqVQEouIAYQoJGkKAioCFKYAgtBGmWoAlq1mgmAgOqKgEVFAABRFADFBAQCoqAuloaAGgIKgCAAqAEWJABK0zQTEVAdUWoAqAKIoKJFALQASqzQAUEKAAQ0AwAEpqAtBAWBIAanQz1QTFAHWpWqyAACiKAAABBQEEChQAQFgqABUAAAQoC2oADNarNBFAHWotQUEAUAQWCQFEoCoqUCAQAAAQAoqAAkAAoEXA0GUqs0AMUHRKpUEgaKCooqpagIoIClEBRAAVAAQFEUEAACgAmroJUWs0BWdAdgqVFRYigqUAFSKqBEUAogoEKIAgLUVAFqKCQADEUBAAGVqAgKK6JV1lBSJQFBAWNRICAgqqBoAqURAAAAAANRagLpqYAi6lQFZtWsgirig2lBBFQBTkFVYAIiwABAVq+FARKAKCAi0vxAFKACIAVmqClZUEAEH//2Q=="

-- | A limitation in ghcjs-vdom means that non-standard attributes aren't always defined properly.
-- This works around the issue. The value returned here should take the place of the standard
-- attribute definition, i.e. instead of (div () ..) or (div [..] ..), use (div (customAttrs []) ..).
--
-- If possible, use the functions exported by GHCJS.VDOM.Attribute instead.
customAttrs :: Map String String -> Attributes'
customAttrs attrs = let str = (fromString $ ("{"++) $ (++"}") $ drop 2 $ Map.foldWithKey (\k v s -> s++", "++show k++":"++show v) "" attrs) :: JSString
  in unsafeToAttributes [jsu'| {attributes:JSON.parse(`str)} |]
