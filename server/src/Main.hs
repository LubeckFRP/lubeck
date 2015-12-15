

import Servant
import Servant.Utils.StaticFiles (serveDirectory)
import qualified Network.Wai.Handler.Warp
import qualified System.Process
import qualified Data.Map as Map
import Data.Map(Map)
import Control.Monad.Except
import System.Process -- TODO
import System.Exit (ExitCode(..))
import System.Environment(getEnvironment)

import Util.ParseEnv (getJsExeBinPathFromEnv)

type GhcJsTestServer = Raw

{-
GHCJS_PACKAGE_PATH=/Users/Hoglund/Code/hs/ghcjs-test/.stack-work/install/x86_64-osx/nightly-2015-12-14/ghcjs-0.2.0_ghc-7.10.2/pkgdb

PATH=/Users/Hoglund/Code/hs/ghcjs-test/.stack-work/install/x86_64-osx/nightly-2015-12-14/ghcjs-0.2.0_ghc-7.10.2/bin

-}

server :: String -> Server GhcJsTestServer
-- TODO how to get this from Stack
server jsExeDir = serveDirectory jsExeDir

-- foo = "/Users/Hoglund/Code/hs/ghcjs-test/.stack-work/install/x86_64-osx/nightly-2015-12-14/ghcjs-0.2.0_ghc-7.10.2/bin/ghcjs-test.jsexe"
-- getJsExeBinPathFromEnv

stackEnv = do
  let stackExe = "stack"
  stackEnv <- inheritSpecifically ["HOME","PATH"]
  let cwd = Nothing -- Meaning: yes, do inherit it
  (r,out,err) <- flip System.Process.readCreateProcessWithExitCode "" $ (\x -> x { cwd = cwd, env = stackEnv }) $
    System.Process.proc stackExe ["exec", "/usr/bin/env"]
  case r of
    ExitSuccess -> return out
    -- TODO throwError instead of fail
    ExitFailure e -> fail $ stackExe ++ " exited with code " ++ show e ++ " and message " ++ err

-- | Create an environment inheriting exactly the given properties from the system environment (the environment used to invoke alan).
inheritSpecifically :: [String] -> IO (Maybe [(String, String)])
inheritSpecifically ks = do
  base <- fmap Map.fromList $ System.Environment.getEnvironment
  return $ Just $ Map.toList (appAll (fmap (\k -> case (Map.lookup k base) of { Just v -> Map.insert k v ; Nothing -> id }) ks) Map.empty)
    where
      appAll = Prelude.foldr (.) id


main = do
  env <- stackEnv
  let port = 8080
  jsExeDir = getJsExeBinPathFromEnv env
  case jsExeDir of
    Left msg -> print $ "Could not find compiled code: " ++ msg
    Right jsExeDir -> do
      putStrLn "Serving compiled client from"
      putStrLn $ " " ++ jsExeDir ++ "/ghcjs-test.jsexe"
      putStrLn $ "Listening on " ++ port
      Network.Wai.Handler.Warp.run port (serve (Proxy::Proxy GhcJsTestServer) (server (jsExeDir++"/ghcjs-test.jsexe")))
