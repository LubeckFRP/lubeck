
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Scotty
import Data.Monoid (mconcat)
import Util.RunMake(runMake, runGitPull)
import Control.Monad.IO.Class(liftIO)

main = scotty 3000 $ do
    get "/:word" $ do
        liftIO $ runGitPull >> runMake
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!!!</h1>"]
