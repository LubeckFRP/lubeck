
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

import Control.Lens

import GHCJS.VDOM (mount, diff, patch, VNode, DOMNode)
import GHCJS.VDOM.Element (p, h1, div, text, form, button)
import GHCJS.VDOM.Event (initEventDelegation, click, submit, stopPropagation, preventDefault)
import GHCJS.Foreign.QQ (js)

import Data.Default (def)

-- TODO
import FRP

getW :: IO DOMNode
getW = do
  root <- [js| (function(){ var r = window.document.createElement('div'); window.document.body.appendChild(r); return r }()) |]
  return root

network :: (Event String -> Event String)
network inp =
      let
        as = counter $ filterE (== "A") inp
        bs = counter $ filterE (== "B") inp
        qs = counter $ filterE (== "Q") inp
        info = liftA3 (\na nb nq -> "Received " ++ show na ++ " as, " ++ show nb ++ " bs, " ++ show nq ++ " qs") as bs qs
  in sample info inp

mainView :: (String -> IO ()) -> String -> VNode
mainView sink st = div () [ h1 () [text "Example"]
   , p () [text (fromString $ st)]
   , form [submit $ \e -> preventDefault e >> return ()] (fmap (\a -> button [click $ \_ -> (sink [a])] [text $ fromString [a]]) ['A'..'Z'])
   ]


main = do
  w <- getW
  initEventDelegation []

  frpIn  <- (TChan.newTChanIO :: IO (TChan.TChan String))
  -- frpOut <- (TChan.newTChanIO :: IO (TChan.TChan String))
  frpState <- (TVar.newTVarIO "Press A, B or Q!" :: IO (TVar.TVar String))

  forkIO $ do
    runR network (atomically $ TChan.readTChan frpIn) (atomically . TVar.writeTVar frpState)

  loop w $ do
    threadDelay (round $ 1000000/30)
    st <- atomically $ TVar.readTVar frpState
    return $ mainView (atomically . TChan.writeTChan frpIn) st


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
