
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Lubeck.App
    ( Html
    , runApp
    , runAppPure
    , runAppStatic
    , runAppReactive
    ) where
import Prelude hiding (div)
import qualified Prelude

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forM_, forever, unless)
import Control.Monad.STM (atomically)
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Data.List
import Data.Monoid
import Data.Maybe(fromMaybe)
import Data.Default (def)

import GHCJS.VDOM (mount, diff, patch, VNode, DOMNode)
import GHCJS.VDOM.Event (initEventDelegation)
import GHCJS.Foreign.QQ (js, jsu, jsu')
import GHCJS.Types(JSString, jsval)

import Lubeck.FRP
import Lubeck.Html

runAppStatic :: Html -> IO ()
runAppStatic x = runAppReactive (pure x)

runAppReactive :: Behavior Html -> IO ()
runAppReactive r = runAppPure (pure $ pure r) (flip const)


runAppPure
  :: (Events action -> IO (Behavior model))
  -> (Sink action -> model -> Html)
  -> IO ()
runAppPure update render = runApp (fmap (fmap $ \x -> (x,Nothing)) . update) render

runApp
  :: (Events action -> IO (Behavior (model, Maybe (IO action))))
  -> (Sink action -> model -> Html)
  -> IO ()
runApp update render = do
  -- Setup chans/vars to hook into the FRP system

  -- Actions to run (from user or finished jobs)
  frpIn      <- (TChan.newTChanIO :: IO (TChan.TChan action))
  -- Fired whenever state has been updated
  frpUpdated <- (TChan.newTChanIO :: IO (TChan.TChan ()))
  -- Current state
  -- Should not be read before frpUpdated has been emitted at least once
  frpState   <- (TVar.newTVarIO (error "Should not be sampled") :: IO (TVar.TVar model))
  -- Jobs for worked thread
  frpJobs    <- (TChan.newTChanIO :: IO (TChan.TChan (IO action)))

  -- Compile FRP system
  forkIO $ do
    system <- runFRP' update
    -- Propagate initial value (or we won't see anything)
    (frpSystemState system) (\(st, _) -> atomically $ TVar.writeTVar frpState st >> TChan.writeTChan frpUpdated ())
    -- Register output
    (frpSystemOutput system) $ \(st, job) -> do
        atomically $ TVar.writeTVar frpState st
        case job of
            Nothing -> return ()
            Just job -> atomically $ TChan.writeTChan frpJobs job
        atomically $ TChan.writeTChan frpUpdated ()
    forever $ do
      i <- atomically $ TChan.readTChan frpIn
      -- putStrLn $ "Processing event: " ++ show i
      (frpSystemInput system) i

  -- Job thread
  forkIO $
    forever $ do
      job <- atomically $ TChan.readTChan frpJobs
      -- putStrLn "Starting a job"
      res <- job
      -- putStrLn "Job finished"
      atomically $ TChan.writeTChan frpIn res

  -- Enter rendering loop on main thread
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
      root <- [js| (function(){
        var root = window.document.createElement('div');
        window.document.body.appendChild(root);
        return root;
      }()) |]
      return root

    -- Repeatedly call the given function to produce a VDOM, then patch it into the given DOM node.
    renderingLoop :: DOMNode -> IO VNode -> IO ()
    renderingLoop domNode k = do
      node1 <- k
      vMount <- mount domNode node1
      forever $ do
        -- insist $ do -- TODO insist should not be needed
          node <- k
          delta <- diff vMount node
          patch vMount delta

    -- | Repeat a computation until it succeeds.
    insist :: Monad m => m Bool -> m ()
    insist k = do
      r <- k
      unless r (insist k)
      return ()
