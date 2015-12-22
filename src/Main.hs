
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

import GHCJS.VDOM.Event (click, change, submit, stopPropagation, preventDefault)
import GHCJS.Foreign.QQ (js, jsu, jsu')
import GHCJS.Types(JSString, jsval)
import GHCJS.VDOM.Element (p, h1, div, text, form, button, img, hr, custom)
import GHCJS.VDOM.Attribute (src, width, class_)
import qualified GHCJS.VDOM.Element as E
import qualified GHCJS.VDOM.Attribute as A
import GHCJS.VDOM.Unsafe (unsafeToAttributes, Attributes')

import FRP2
import App

import qualified BD.Data.Account as A
import qualified BD.Data.Count as C
import qualified BD.Data.SearchPost as P
import BD.Data.SearchPost(SearchPost)
import BD.Data.Interaction


type Widget i o = Sink o -> i -> Html
type Widget' a  = Widget a a

data Action
  = NoAction
  | LoadAction (Maybe JSString) (Maybe JSString)
  | ReplaceModel Model

-- For debugging only
instance Show Action where
  show = g where
    g NoAction         = "NoAction"
    g (LoadAction _ _) = "LoadAction"
    g (ReplaceModel _) = "ReplaceModel"

type Model = InteractionSet SearchPost

update :: E Action -> IO (R (Model, Maybe (IO Action)))
update = foldpR step initial
  where
    initial = (InteractionSet Nothing Nothing [], Nothing)

    step NoAction             (model,_) = (model,Nothing)
    step (LoadAction a b)     (model,_) = (model,Just $ fmap ReplaceModel (loadShoutouts a b))
    step (ReplaceModel model) (_,_)     = (model,Nothing)

render :: Widget Model Action
render actions model = div
  (customAttrs $ Map.fromList [("style", "width: 900px; margin-left: auto; margin-right: auto") ])
  [ h1 () [text "Shoutout browser"]
  , div ()
    [buttonW actions ()]
  , div
    ()
    [ interactionSetW actions model ]
  ]

-- TODO make this a Widget (Maybe JSString, Maybe JSString) Action
buttonW :: Widget () Action
buttonW sink () = form
  [ submit $ \e -> preventDefault e >> return () ]
  [
    -- E.input [ change $ \e -> [jsu|console.log(`e)|] ] [text "abc"]
  -- ,
    E.input () [text "def"]
  , button (click $ \_ -> sink (LoadAction (Just "tomjauncey") Nothing)) [text "Load shoutouts!"] ]

interactionSetW :: Widget (InteractionSet SearchPost) Action
interactionSetW actions model = div ()
  [ p () [ text $ ""       <> textToJSString (fromMaybe "(anyone)" $ fmap ("@" <>) $ model .: from_account .:? A.username)
         , text $ " to "   <> textToJSString (fromMaybe "(anyone)" $ fmap ("@" <>) $ model .: to_account .:? A.username) ]
  , div () (Data.List.intersperse (hr () ()) $ fmap (interactionW actions) $ model .: interactions)
  ]

interactionW :: Widget (Interaction SearchPost) Action
interactionW actions model = div ()
  [ p () [text (showJS $ model .: interaction_time)]
  -- Growth graph
  , div [class_ "row"]
    [ div [class_ "col-xs-8 col-lg-8"] [img [src greyImgUrl, width 600] ()]
    , div [class_ "col-xs-4 col-lg-4"] [img [src (textToJSString $ model .: medium .: P.url), width 200] ()]
    ]
  , p () [text "Estimated impact: (?)"]
  ]


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
