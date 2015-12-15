
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, OverloadedStrings #-}

import Prelude hiding (div)
import qualified Prelude

import Control.Applicative
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forM_)
import Control.Monad (forever, unless)
import Control.Monad.Plus (partial, predicate)
import Control.Monad.STM (atomically)
import Data.String (fromString)
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Data.Time.Clock
import qualified System.Random as Random

import GHCJS.VDOM (mount, diff, patch, VNode, DOMNode)
import GHCJS.VDOM.Element (p, h1, div, text, form, button)
import GHCJS.VDOM.Event (initEventDelegation, click, submit, stopPropagation, preventDefault)
import GHCJS.Foreign.QQ (js)


getW :: IO DOMNode
getW = do
  root <- [js| (function(){ var r = window.document.createElement('div'); window.document.body.appendChild(r); return r }()) |]
  return root

main = do

  w <- getW
  randomVals <- (TVar.newTVarIO 0 :: IO (TVar.TVar Double))
  counter <- (TVar.newTVarIO 15 :: IO (TVar.TVar Int))
  threadsLaunched <- (TVar.newTVarIO 0 :: IO (TVar.TVar Int))

  forkIO $ do
    forever $ do
      threadDelay (round $ 1000000*1.5)
      Random.randomIO >>= (atomically . TVar.writeTVar randomVals)

  forkIO $ do
    threadDelay (round $ 1000000*1)
    forM_ [0..14000] $ \_ -> forkIO $ do
      atomically $ TVar.modifyTVar threadsLaunched succ
      forever $ threadDelay (round $ 1000000*1)


  initEventDelegation []

  loop w $ do
    threadDelay (round $ 1000000/30)
    (Data.Time.Clock.UTCTime day time) <- Data.Time.Clock.getCurrentTime
    randomVal <- atomically $ TVar.readTVar randomVals
    counterVal <- atomically $ TVar.readTVar counter
    threadsLaunchedVal <- atomically $ TVar.readTVar threadsLaunched

    let theNode = div () [ h1 () [text "Hello, Hans!"]
                         , p () [text (fromString $ show time)]
                         , p () [text (fromString $ show randomVal)]
                         , p () [text (fromString $ show counterVal)]
                         , p () [text (fromString $ show threadsLaunchedVal ++ " threads launched")]
                         , form [submit $ \e -> preventDefault e >> return ()] [
                              button [click $ \e -> (atomically $ TVar.modifyTVar counter succ)] [text "Increase"]
                            , button [click $ \e -> (atomically $ TVar.modifyTVar counter pred)] [text "Decrease"]
                          ]
                         , div [click $ \e -> (atomically $ TVar.modifyTVar counter succ)] [text "Click above me!"]
                         , div [click $ \_ -> print "!"] [text "Click above me!"]

                         ]
    return theNode


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
