
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
import Data.Text(Text)
import Data.Monoid

import GHCJS.VDOM (mount, diff, patch, VNode, DOMNode)
import GHCJS.VDOM.Element (p, h1, div, text, form, button)
import GHCJS.VDOM.Event (initEventDelegation, click, submit, stopPropagation, preventDefault)
import GHCJS.Foreign.QQ (js)

import Data.Default (def)

import JavaScript.Web.XMLHttpRequest -- TODO

import FRP2

import BD.Data.Account
import BD.Data.Count
import BD.Data.SearchPost
import BD.Data.Interaction


type Html   = VNode
type Widget i o = Sink o -> i -> [Html]
type Widget' a = Widget a a

newtype Action = Action ()
  deriving (Show)

newtype Model  = Model {
    interactions :: Maybe (InteractionSet SearchPost)
  }
  deriving (Show)
defModel = Model Nothing

initial :: Model
initial = Model Nothing

update :: E Action -> IO (R Model)
update actions = do
  return $ pure defModel

render :: Sink Action -> Model -> Html
render actions model = div () [ h1 () [text "Shoutout browser"]
   ]



renderInteractionSet :: InteractionSet SearchPost -> Html
renderInteractionSet _ = div ()
  [ h1 () [text "Shoutout Browser"]
  , p () [text "From"]
  , p () [text "To"]
  , p () [text "TODO date"]
  -- all thge

  ]

renderInteraction :: Interaction SearchPost -> Html
renderInteraction _ = div ()
  [ p () [text "(date)"]
  , p () [text "(the growth)"]
  , p () [text "(the image)"]
  , p () [text "Estimated impact"]
  ]
-- just account names
-- Time
-- index in interaction list?
-- growth graph
-- estimated impact

main :: IO ()
main = do
  interactions <- loadShoutouts (Just "tomjauncey") Nothing
  print interactions

  -- Setup chans/vars to hook into the FRP system
  -- TODO extract "initial" from FRP system below instead of passing it explicitly here
  frpIn    <- (TChan.newTChanIO :: IO (TChan.TChan Action))
  frpState <- (TVar.newTVarIO initial :: IO (TVar.TVar Model))

  -- Launch FRP system
  forkIO $ do
    system <- runER' update
    (output system) (\st -> atomically $ TVar.writeTVar frpState st)
    forever $ do
      i <- atomically $ TChan.readTChan frpIn
      (input system) i
  -- Enter rendering renderingLoop on main thread
  do
    initEventDelegation []
    renderingNode <- createRenderingNode
    renderingLoop renderingNode $ do
      -- TODO wake up on update instead of polling
      threadDelay (round $ 1000000/30)
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
