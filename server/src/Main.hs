
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

main :: IO ()
main = do
  let port          = 8090
  let indexHtmlFile = "static/index.html"
  let bootstrapFile = "static/bootstrap.css"
  let bootstrapThemeFile = "static/bootstrap-theme.css"

  -- Extracts environment with the Stack additions
  -- Uses some heuristics to find the location of the compiled code (see getJsExeBinPathFromEnv)
  jsExeDir <- fmap getJsExeBinPathFromEnv getStackEnv

  -- If successful, serve the compiled code
  case jsExeDir of
    Left msg -> print $ "Could not find compiled code: " ++ msg
    Right jsExeDir -> do
      exampleServer       <- serveApp jsExeDir "bd-example-app"         indexHtmlFile bootstrapFile bootstrapThemeFile
      adplatformServer    <- serveApp jsExeDir "bd-adplatform"          indexHtmlFile bootstrapFile bootstrapThemeFile
      interactionsServer  <- serveApp jsExeDir "bd-interactions"        indexHtmlFile bootstrapFile bootstrapThemeFile
      exampleStaticServer <- serveApp jsExeDir "bd-example-static-page" indexHtmlFile bootstrapFile bootstrapThemeFile
      exampleStaticServerThomasD <- serveApp jsExeDir "bd-example-static-page-thomasd" indexHtmlFile bootstrapFile bootstrapThemeFile
      indexServer         <- serveApp jsExeDir "bd-index"               indexHtmlFile bootstrapFile bootstrapThemeFile

      putStrLn $ "Listening on " ++ show port
      Network.Wai.Handler.Warp.run port $ serve (Proxy::Proxy Layout) $
        exampleServer
          :<|> adplatformServer
          :<|> interactionsServer
          :<|> exampleStaticServer
          :<|> exampleStaticServerThomasD
          :<|> indexServer

serveApp :: String -> String -> String -> String -> String -> IO (Server Raw)
serveApp jsExeDir appName indexHtmlFile bootstrapFile bootstrapThemeFile  = do
  putStrLn $ "Serving app '" ++ appName ++ "', from"
  putStrLn $ " " ++ jsExeDir ++ "/" ++ appName ++ ".jsexe"
  putStrLn $ " index.html is '" ++ indexHtmlFile ++ "'"
  copyFile indexHtmlFile (jsExeDir ++ "/" ++ appName ++ ".jsexe/index.html")
  copyFile bootstrapFile (jsExeDir ++ "/" ++ appName ++ ".jsexe/bootstrap.css")
  copyFile bootstrapThemeFile (jsExeDir ++ "/" ++ appName ++ ".jsexe/bootstrap-theme.css")
  return $ serveDirectory $ jsExeDir ++ "/" ++ appName ++ ".jsexe"
