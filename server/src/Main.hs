
{-# LANGUAGE DataKinds, TypeOperators #-}

import Data.Map(Map)
import Control.Monad (forever)
import Control.Monad.Except
import System.Exit (ExitCode(..))
import System.Directory(copyFile)
import Control.Concurrent (threadDelay, forkIO)
import Servant
import Servant.Utils.StaticFiles (serveDirectory)
import qualified Network.Wai.Handler.Warp
import qualified System.Process

import Util.ParseEnv (getJsExeBinPathFromEnv)
import Util.StackEnv (getStackEnv)

type Layout =
  "example" :> Raw
    :<|>
  "adplatform" :> Raw
    :<|>
  "interactions" :> Raw
    :<|>
  "example-static" :> Raw
    :<|>
  "example-static-thomasd" :> Raw
    :<|>
  Raw

resources =
  [ ("static/index.html", "index.html")
  , ("static/bootstrap.css", "bootstrap.css")
  , ("static/bootstrap-theme.css", "bootstrap-theme.css")
  , ("static/ajax-loader.gif", "ajax-loader.gif")
  , ("static/custom.css", "custom.css")
  ]

main :: IO ()
main = do
  let port = 8090

  -- Extracts environment with the Stack additions
  -- Uses some heuristics to find the location of the compiled code (see getJsExeBinPathFromEnv)
  jsExeDir <- fmap getJsExeBinPathFromEnv getStackEnv

  -- If successful, serve the compiled code
  case jsExeDir of
    Left msg -> print $ "Could not find compiled code: " ++ msg
    Right jsExeDir -> do
      exampleServer       <- serveApp jsExeDir "bd-example-app"
      adplatformServer    <- serveApp jsExeDir "bd-adplatform"
      interactionsServer  <- serveApp jsExeDir "bd-interactions"
      exampleStaticServer <- serveApp jsExeDir "bd-example-static-page"
      exampleStaticServerThomasD <- serveApp jsExeDir "bd-example-static-page-thomasd"
      indexServer         <- serveApp jsExeDir "bd-index"

      putStrLn $ "Listening on " ++ show port
      Network.Wai.Handler.Warp.run port $ serve (Proxy::Proxy Layout) $
        exampleServer
          :<|> adplatformServer
          :<|> interactionsServer
          :<|> exampleStaticServer
          :<|> exampleStaticServerThomasD
          :<|> indexServer

serveApp :: String -> String -> IO (Server Raw)
serveApp jsExeDir appName = do
  putStrLn $ "Serving app '" ++ appName ++ "', from"
  putStrLn $ " " ++ jsExeDir ++ "/" ++ appName ++ ".jsexe"
  mapM (copyResource jsExeDir appName) resources
  return $ serveDirectory $ jsExeDir ++ "/" ++ appName ++ ".jsexe"

  where
    copyResource :: String -> String -> (String, String) -> IO ()
    copyResource jsExeDir appName (from, to) =
      copyFile from (jsExeDir ++ "/" ++ appName ++ ".jsexe/" ++ to)
