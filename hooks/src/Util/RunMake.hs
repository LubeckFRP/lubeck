
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}

module Util.RunMake (
    runGitPull
  , runMake
  , startServer
  , stopServer
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

-- TODO find out
kServerPath = "/root/.local/bin/lubeck-server"

-- stopServer :: IO ()
stopServer ph = do
  terminateProcess ph
  getProcessExitCode ph

startServer = do
  (_,_,_,ph) <- System.Process.createProcess_ "startServer" ((proc kServerPath []) { cwd = Just ".." })
  return ph

-- | Run `git pull` in `..`
runGitPull :: IO ()
runGitPull = do
  let exe = "git"
  env <- inheritSpecifically ["HOME","PATH","TERM"]
  let cwd = Just ".."
  (r,out,err) <- flip System.Process.readCreateProcessWithExitCode "" $ (\x -> x
        { std_out = Inherit, std_err = Inherit, cwd = cwd, env = env }) $
    System.Process.proc exe ["pull"]
  case r of
    ExitSuccess   -> return ()
    ExitFailure e -> fail $ exe ++ " exited with code " ++ show e ++ " and message " ++ err

-- | Run `make` in `..`
runMake :: IO ()
runMake = do
  let exe = "make"
  env <- inheritSpecifically ["HOME","PATH","TERM"]
  let cwd = Just ".."
  (r,out,err) <- flip System.Process.readCreateProcessWithExitCode "" $ (\x -> x
        { std_out = Inherit, std_err = Inherit, cwd = cwd, env = env }) $
    System.Process.proc exe ["build-client-fast", "build-server"]
  case r of
    ExitSuccess   -> return ()
    ExitFailure e -> fail $ exe ++ " exited with code " ++ show e ++ " and message " ++ err

-- | Create an environment inheriting exactly the given properties from the system environment
inheritSpecifically :: [String] -> IO (Maybe [(String, String)])
inheritSpecifically ks = do
  base <- fmap Map.fromList $ System.Environment.getEnvironment
  return $ Just $ Map.toList (appAll (fmap (\k -> case (Map.lookup k base) of { Just v -> Map.insert k v ; Nothing -> id }) ks) Map.empty)
    where
      appAll = Prelude.foldr (.) id
