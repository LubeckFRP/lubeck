
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}

-- | Utilities for extracting compiler name and package database locations from the environment.
module Util.StackEnv (
  getStackEnv
) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable (asum)
import Data.Monoid
import qualified Data.List
import System.Process -- TODO
import System.Exit (ExitCode(..))
import System.Environment(getEnvironment)
import qualified Data.Map as Map

-- | Extract the environment as set up by Stack ().
getStackEnv :: IO String
getStackEnv = do
  let stackExe = "stack"
  stackEnv <- inheritSpecifically ["HOME","PATH"]
  let cwd = Nothing -- Meaning: yes, do inherit it
  (r,out,err) <- flip System.Process.readCreateProcessWithExitCode "" $ (\x -> x { cwd = cwd, env = stackEnv }) $
    System.Process.proc stackExe ["exec", "/usr/bin/env"]
  case r of
    ExitSuccess -> return out
    ExitFailure e -> fail $ stackExe ++ " exited with code " ++ show e ++ " and message " ++ err

-- | Create an environment inheriting exactly the given properties from the system environment
inheritSpecifically :: [String] -> IO (Maybe [(String, String)])
inheritSpecifically ks = do
  base <- fmap Map.fromList $ System.Environment.getEnvironment
  return $ Just $ Map.toList (appAll (fmap (\k -> case (Map.lookup k base) of { Just v -> Map.insert k v ; Nothing -> id }) ks) Map.empty)
    where
      appAll = Prelude.foldr (.) id
