

import Servant
import Servant.Utils.StaticFiles (serveDirectory)
import qualified Network.Wai.Handler.Warp
import qualified System.Process
import qualified Data.Map as Map
import Data.Map(Map)
import Control.Monad (forever)
import Control.Monad.Except
import System.Process -- TODO
import System.Exit (ExitCode(..))
import System.Environment(getEnvironment)
import System.Directory(copyFile)
import Control.Concurrent (threadDelay, forkIO)

import Util.ParseEnv (getJsExeBinPathFromEnv)

type GhcJsTestServer = Raw

server :: String -> Server GhcJsTestServer
server jsExeDir = serveDirectory jsExeDir

-- | Extract the environment as set up by Stack ().
stackEnv :: IO String
stackEnv = do
  let stackExe = "stack"
  stackEnv <- inheritSpecifically ["HOME","PATH"]
  let cwd = Nothing -- Meaning: yes, do inherit it
  (r,out,err) <- flip System.Process.readCreateProcessWithExitCode "" $ (\x -> x { cwd = cwd, env = stackEnv }) $
    System.Process.proc stackExe ["exec", "/usr/bin/env"]
  case r of
    ExitSuccess -> return out
    ExitFailure e -> fail $ stackExe ++ " exited with code " ++ show e ++ " and message " ++ err

-- | Create an environment inheriting exactly the given properties from the system environment (the environment used to invoke alan).
inheritSpecifically :: [String] -> IO (Maybe [(String, String)])
inheritSpecifically ks = do
  base <- fmap Map.fromList $ System.Environment.getEnvironment
  return $ Just $ Map.toList (appAll (fmap (\k -> case (Map.lookup k base) of { Just v -> Map.insert k v ; Nothing -> id }) ks) Map.empty)
    where
      appAll = Prelude.foldr (.) id

main :: IO ()
main = do
  let port = 8080
  let appName = "bd-interactions" -- TODO get from cmdline

  -- Extracts environment with the Stack additions
  -- Uses some heuristics to find the location of the compiled code (see getJsExeBinPathFromEnv)
  jsExeDir <- fmap getJsExeBinPathFromEnv stackEnv

  -- If successful, serve the compiled code
  case jsExeDir of
    Left msg -> print $ "Could not find compiled code: " ++ msg
    Right jsExeDir -> do

      putStrLn "Serving compiled client from"
      putStrLn $ " " ++ jsExeDir ++ "/" ++ appName ++ ".jsexe"
      putStrLn $ "Listening on " ++ show port

      -- TODO Hacky copying of index file (gets overwritten by GHCJS)
      forkIO $ forever $ do
        threadDelay (1000000)
        copyFile ("static/index.html") (jsExeDir++"/" ++ appName ++ ".jsexe/index.html")

      Network.Wai.Handler.Warp.run port (serve (Proxy::Proxy GhcJsTestServer) (server $ jsExeDir++"/" ++ appName ++ ".jsexe"))
