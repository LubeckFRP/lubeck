

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Exception
import           Control.Monad             (forever)
import           Data.Map                  (Map)
import           Data.Text                 (Text (), pack, replace, unpack)
import qualified Network.Wai.Handler.Warp
import           Servant
import           Servant.Utils.StaticFiles (serveDirectory)
import           System.Directory          (copyFile)
import           System.Exit               (ExitCode (..))
import           System.IO.Error
import           System.Process
import qualified System.Process
import           System.Random

import           Util.ParseEnv             (getJsExeBinPathFromEnv)
import           Util.StackEnv             (getStackEnv)

type Layout =
  "example" :> Raw
    :<|>
  "adplatform" :> Raw
    :<|>
  "interactions" :> Raw
    :<|>
  "example-static" :> Raw
    :<|>
  "example-dynamic" :> Raw
    :<|>
  "example-widget-composition" :> Raw
    :<|>
  "example-api-req" :> Raw
    :<|>
  "example-plots" :> Raw
    :<|>
  Raw



resources =
  [ ("static/bootstrap.css",             "bootstrap.css")
  , ("static/bootstrap-theme.css",       "bootstrap-theme.css")
  , ("static/custom.css",                "custom.css")
  , ("static/font-awesome.min.css",      "font-awesome.min.css") -- modified font paths: s#../fonts/##g
  , ("static/fontawesome-webfont.ttf",   "fontawesome-webfont.ttf")
  , ("static/fontawesome-webfont.woff",  "fontawesome-webfont.woff")
  , ("static/fontawesome-webfont.woff2", "fontawesome-webfont.woff2")
  , ("static/favicon.png",               "favicon.png")
  ]

-- pattern to look for in `recources'` and replace with git commit hash
replacePattern = "%xxx%" :: Text

-- resources to search and replace `replacePattern` with git commit hash
resources' =
  [ ("static/index.html",                "index.html")
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
      rnd <- getGitCommitHashOrRandom

      exampleServer            <- serveApp rnd jsExeDir "bd-example-app"
      adplatformServer         <- serveApp rnd jsExeDir "bd-adplatform"
      interactionsServer       <- serveApp rnd jsExeDir "bd-interactions"
      exampleStaticServer      <- serveApp rnd jsExeDir "bd-example-static-page"
      exampleDynamicServer     <- serveApp rnd jsExeDir "bd-example-dynamic-page"
      exampleWidgetComposition <- serveApp rnd jsExeDir "bd-example-widget-composition"
      exampleApiReq            <- serveApp rnd jsExeDir "bd-example-api-req"
      examplePlots             <- serveApp rnd jsExeDir "bd-example-plots"
      indexServer              <- serveApp rnd jsExeDir "bd-index"

      putStrLn $ "Listening on " ++ show port
      Network.Wai.Handler.Warp.run port $ serve (Proxy::Proxy Layout) $
        exampleServer
          :<|> adplatformServer
          :<|> interactionsServer
          :<|> exampleStaticServer
          :<|> exampleDynamicServer
          :<|> exampleWidgetComposition
          :<|> exampleApiReq
          :<|> examplePlots
          :<|> indexServer

serveApp :: String -> String -> String -> IO (Server Raw)
serveApp rnd jsExeDir appName = do
  putStrLn $ "Serving app '" ++ appName ++ "', from"
  putStrLn $ " " ++ jsExeDir ++ "/" ++ appName ++ ".jsexe"
  mapM (copyResource jsExeDir appName) resources
  mapM (copyResource' rnd jsExeDir appName) resources'
  return $ serveDirectory $ jsExeDir ++ "/" ++ appName ++ ".jsexe"

  where
    copyResource' :: String -> String -> String -> (String, String) -> IO ()
    copyResource' rnd jsExeDir appName (from, to) = do
      fileContent <- readFile from
      let fileContent' = unpack (replace replacePattern (pack rnd) (pack fileContent))
      writeFile (jsExeDir ++ "/" ++ appName ++ ".jsexe/" ++ to) fileContent'

    copyResource :: String -> String -> (String, String) -> IO ()
    copyResource jsExeDir appName (from, to) =
      copyFile from (jsExeDir ++ "/" ++ appName ++ ".jsexe/" ++ to)

getGitCommitHashOrRandom = do
  putStr "Looking for git commit... "
  rc <- tryIOError $ readProcess "git" ["log", "--pretty=format:'%H'", "-n", "1"] ""
  rnd <- case rc of
    Right stdout -> do
      let commit = take 40 . drop 1 $ stdout
      putStrLn $ "found: " ++ commit
      return commit

    Left x -> do
      g <- getStdGen
      let rs = take 10 $ (randomRs ('a', 'z') g)

      putStrLn $ "not found: " ++ show x
      putStrLn $ "Using random value " ++ rs

      return rs

  return rnd
