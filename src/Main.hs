
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

newtype Action = Action
  deriving (Eq, Ord, Show)

newtype Model  = Model {
    interactions :: Maybe (InteractionSet SearchPost)
  }
  deriving (Eq, Ord, Show)
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
renderInteractionSet = div () []

renderInteraction :: Interaction SearchPost -> Html
renderInteraction = div () []
-- just account names
-- Time
-- index in interaction list?
-- growth graph
-- estimated impact

renderSearchPost :: InteractionSet SearchPost -> Html
renderSearchPost = div () []
-- the media/image

main = do
  interactions <- loadShoutouts (Just "tomjauncey") Nothing
  print interactions

  w <- getW
  initEventDelegation []

  frpIn    <- (TChan.newTChanIO :: IO (TChan.TChan Action))
  -- TODO extract initial from FRP system below
  frpState <- (TVar.newTVarIO initial :: IO (TVar.TVar Model))

  forkIO $ do
    system <- runER (\e -> update e >>= \r -> return (r, sample r e))
    (output system) (\st -> atomically $ TVar.writeTVar frpState st)
    forever $ do
      i <- atomically $ TChan.readTChan frpIn
      (input system) i

  loop w $ do
    threadDelay (round $ 1000000/30)
    st <- atomically $ TVar.readTVar frpState
    return $ render (atomically . TChan.writeTChan frpIn) st

  where
    getW :: IO DOMNode
    getW = do
      root <- [js| (function(){ var r = window.document.createElement('div'); window.document.body.appendChild(r); return r }()) |]
      return root

    -- Repeatedly call the given function to produce a VDOM, then patch it into the given DOM node.
    loop :: DOMNode -> IO VNode -> IO ()
    loop domNode k = do
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
