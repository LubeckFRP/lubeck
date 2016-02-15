
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Scotty
import Data.Monoid (mconcat)
import Util.RunMake(runMake, runGitPull)
import Control.Monad.IO.Class(liftIO)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM(atomically)
import Control.Concurrent(forkIO)
import Control.Exception(SomeException, catch)
import Control.Monad(forever)

main = do
  ch <- atomically $ newTChan :: IO (TChan ())
  forkIO $ forever $ do
    atomically $ readTChan ch
    print "Fetching code"
    liftIO $ runGitPull `catch` (\e -> print (e :: SomeException))
    print "Starting build"
    liftIO $ runMake `catch` (\e -> print (e :: SomeException))
    print "Rebuild done"
    return ()
  scotty 3001 $ do
    post "/:word" $ do
        liftIO $ atomically $ writeTChan ch ()
        -- beam <- param "word"
        html $ mempty -- TODO just the 200
