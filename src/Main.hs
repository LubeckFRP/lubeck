
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, OverloadedStrings #-}

import Prelude hiding (div)
import qualified Prelude

import Control.Applicative
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forM_)
import Control.Monad (forever, unless)
import Data.String (fromString)
import Control.Monad.STM (atomically)
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Data.Text
import qualified Data.List
import Data.Text(Text)
import Data.Monoid
import Data.Maybe(fromMaybe)
import Data.Default (def)

import GHCJS.VDOM (mount, diff, patch, VNode, DOMNode)
import GHCJS.VDOM.Element (p, h1, div, text, form, button, img, hr)
import GHCJS.VDOM.Attribute (src, width, class_, style)
import GHCJS.VDOM.Event (initEventDelegation, click, submit, stopPropagation, preventDefault)
import GHCJS.Foreign.QQ (js)
import GHCJS.Types(JSString, jsval)

import FRP2

import qualified BD.Data.Account as A
import qualified BD.Data.Count as C
import qualified BD.Data.SearchPost as P
import BD.Data.SearchPost(SearchPost)
import BD.Data.Interaction


type Html   = VNode
type Widget i o = Sink o -> i -> [Html]
type Widget' a = Widget a a

type Action = ()
type Model = InteractionSet SearchPost

update :: Model -> E Action -> IO (R Model)
update defModel actions = do
  return $ pure defModel

render :: Sink () -> Model -> Html
render actions model = div ()
  [ h1 () [text "Shoutout browser"]
  , div
    [ style $ "width: 1170px; margin-left: auto; margin-right: auto"]
    [ interactionSetW actions model ]
  ]
  -- where
    -- j x = jsval (x :: JSString)

interactionSetW :: Sink () -> InteractionSet SearchPost -> Html
interactionSetW actions model = div ()
  [ p () [ text $ ""       <> textToJSString (fromMaybe "(anyone)" $ fmap ("@" <>) $ model .: from_account .:? A.username)
         , text $ " to "   <> textToJSString (fromMaybe "(anyone)" $ fmap ("@" <>) $ model .: to_account .:? A.username) ]
  , div () (Data.List.intersperse (hr () ()) $ fmap (interactionW actions) $ model .: interactions)
  ]

interactionW :: Sink () -> Interaction SearchPost -> Html
interactionW actions model = div ()
  [ p () [text (showJS $ model .: interaction_time)]
  -- Growth graph
  , div [class_ "row"]
    [ div [class_ "col-xs-8 col-lg-8"] [img [src greyImgUrl, width 600] ()]
    , div [class_ "col-xs-4 col-lg-4"] [img [src (textToJSString $ model .: medium .: P.url), width 200] ()]
    ]
  , p () [text "Estimated impact: (?)"]
  ]

main :: IO ()
main = do
  -- TODO currently just preloading this
  interactions <- loadShoutouts (Just "tomjauncey") Nothing

  -- Setup chans/vars to hook into the FRP system
  frpIn      <- (TChan.newTChanIO :: IO (TChan.TChan Action))
  frpUpdated <- (TChan.newTChanIO :: IO (TChan.TChan ()))
  frpState   <- (TVar.newTVarIO (error "Should not be sampled") :: IO (TVar.TVar Model))

  -- Launch FRP system
  forkIO $ do
    system <- runER' (update interactions)
    -- Propagate initial value (or we won't see anything)
    (state system) (\st -> atomically $ TVar.writeTVar frpState st >> TChan.writeTChan frpUpdated ())
    -- Register output
    (output system) (\st -> atomically $ TVar.writeTVar frpState st >> TChan.writeTChan frpUpdated ())
    forever $ do
      i <- atomically $ TChan.readTChan frpIn
      (input system) i

  -- Enter rendering renderingLoop on main thread
  do
    initEventDelegation []
    renderingNode <- createRenderingNode
    renderingLoop renderingNode $ do
      atomically $ TChan.readTChan frpUpdated
      st <- atomically $ TVar.readTVar frpState
      return $ render (atomically . TChan.writeTChan frpIn) st

  where
    createRenderingNode :: IO DOMNode
    createRenderingNode = do
      root <- [js| (function(){ var r = window.document.createElement('div'); window.document.body.appendChild(r); return r }()) |]
      return root

    -- Repeatedly call the given function to produce a VDOM, then patch it into the given DOM node.
    renderingLoop :: DOMNode -> IO VNode -> IO ()
    renderingLoop domNode k = do
      node1 <- k
      vMount <- mount domNode node1
      forever $ do
        insist $ do
          node <- k
          delta <- diff vMount node
          patch vMount delta

    -- | Repeat a computation until it succeeds.
    insist :: Monad m => m Bool -> m ()
    insist k = do
      r <- k
      unless r (insist k)
      return ()


-- UTILITY

(.:)  :: a -> (a -> b) -> b
(.:)  x f = f x

(.:?) :: Maybe a -> (a -> b) -> Maybe b
(.:?) x f = fmap f x

showJS :: Show a => a -> JSString
showJS = fromString . show

textToJSString :: Text -> JSString
textToJSString = fromString . Data.Text.unpack

-- TODO serve
greyImgUrl :: JSString
greyImgUrl = "data:image/jpeg;base64,/9j/4AAQSkZJRgABAQAAAQABAAD/2wCEAAkGBxQSEhQUEhQUFBQUFBQUFBQUFBQUFBQUFBQXFxQUFBQYHCggGBwlHBQUITEhJSksLi4uFx8zODMsNygtLiwBCgoKDAwMDgwMDiwZFBksLCwsKywsLDc3Kyw3LCwsLDcsNzcsNyssLCwsLDc3LDcsLCwsLDcsNyw3NzcsNyw3LP/AABEIAOEA4QMBIgACEQEDEQH/xAAYAAEBAQEBAAAAAAAAAAAAAAAAAQIDB//EABkQAQEBAQEBAAAAAAAAAAAAAAABEQJBMf/EABUBAQEAAAAAAAAAAAAAAAAAAAAB/8QAFBEBAAAAAAAAAAAAAAAAAAAAAP/aAAwDAQACEQMRAD8A9QoqIpCCwRKjVTAIBgGqIAqKKCsiKmqCoEBEqiAWhQBFQDVRoEZrVSgzgqCurLWIIKkUBFQBUxQQi4AoEFQVBC1FMASBAEVJABQERSAAoIjTPQM6ADrWWkAVFACmgAAAAsEUBFqABFoJaACCgIilBAAWEIAJ0RKDI1gDdSrUAVFAAACAAACiAqKlAEAFEAAoIKgBQAWJFARUoICA6AgC0KBSIsAAAIRQVAASqlBAAVCKAlWoBAQBFQFixIArPSpQAQHSoqAsEXAAAAAFQBQQACAgAJjSRQTRYgFhUKCKigasTFBWbFS0EEAbqWrTAIuoAoAARYBAqSAqVQEouIAYQoJGkKAioCFKYAgtBGmWoAlq1mgmAgOqKgEVFAABRFADFBAQCoqAuloaAGgIKgCAAqAEWJABK0zQTEVAdUWoAqAKIoKJFALQASqzQAUEKAAQ0AwAEpqAtBAWBIAanQz1QTFAHWpWqyAACiKAAABBQEEChQAQFgqABUAAAQoC2oADNarNBFAHWotQUEAUAQWCQFEoCoqUCAQAAAQAoqAAkAAoEXA0GUqs0AMUHRKpUEgaKCooqpagIoIClEBRAAVAAQFEUEAACgAmroJUWs0BWdAdgqVFRYigqUAFSKqBEUAogoEKIAgLUVAFqKCQADEUBAAGVqAgKK6JV1lBSJQFBAWNRICAgqqBoAqURAAAAAANRagLpqYAi6lQFZtWsgirig2lBBFQBTkFVYAIiwABAVq+FARKAKCAi0vxAFKACIAVmqClZUEAEH//2Q=="
