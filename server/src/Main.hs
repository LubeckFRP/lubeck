

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

import           Util.ParseEnv             (getJsExeBinPathFromEnv, getApiDocPathFromEnv)
import           Util.StackEnv             (getStackEnv)

type Layout =
         "example"                    :> Raw
    :<|> "adplatform"                 :> Raw
    :<|> "interactions"               :> Raw
    :<|> "labelrefiner"               :> Raw
    :<|> "example-static"             :> Raw
    :<|> "example-dynamic"            :> Raw
    :<|> "example-widget-composition" :> Raw
    :<|> "example-api-req"            :> Raw
    :<|> "example-plots"              :> Raw
    :<|> "example-plots2"             :> Raw
    :<|> "example-plots3"             :> Raw
    :<|> "example-plots4"             :> Raw
    :<|> "example-plots5"             :> Raw
    :<|> "example-plots6"             :> Raw
    :<|> "example-history"            :> Raw
    :<|> "doc"                        :> Raw
    :<|> Raw

resources =
  [ ("static/bootstrapcss/bootstrap.css",        "bootstrap.css")
  , ("static/bootstrapcss/bootstrap-theme.css",  "bootstrap-theme.css")
  , ("static/customcss/custom.css",              "custom.css")
  , ("static/fonts/font-awesome.min.css",        "font-awesome.min.css") -- modified font paths: s#../fonts/##g
  , ("static/fonts/fontawesome-webfont.ttf",     "fontawesome-webfont.ttf")
  , ("static/fonts/fontawesome-webfont.woff",    "fontawesome-webfont.woff")
  , ("static/fonts/fontawesome-webfont.woff2",   "fontawesome-webfont.woff2")
  , ("static/images/favicon.png",                "favicon.png")

  , ("static/leaflet/leaflet.css",               "leaflet.css")
  , ("static/leaflet/leaflet.js",                "leaflet.js")
  , ("static/leaflet/leaflet.markercluster.js",  "leaflet.markercluster.js")
  , ("static/leaflet/MarkerCluster.css",         "MarkerCluster.css")
  , ("static/leaflet/MarkerCluster.Default.css", "MarkerCluster.Default.css")

  , ("static/leaflet/images/layers-2x.png",      "layers-2x.png")
  , ("static/leaflet/images/layers.png",         "layers.png")
  , ("static/leaflet/images/marker-icon-2x.png", "marker-icon-2x.png")
  , ("static/leaflet/images/marker-icon.png",    "marker-icon.png")
  , ("static/leaflet/images/marker-shadow.png",  "marker-shadow.png")
  ]

-- pattern to look for in `resources'` and replace with git commit hash
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
  docDir   <- fmap getApiDocPathFromEnv getStackEnv

  -- If successful, serve the compiled code
  case jsExeDir of
    Left msg -> print $ "Could not find compiled code: " ++ msg
    Right jsExeDir -> do
      rnd <- getGitCommitHashOrRandom

      exampleServer            <- serveApp rnd jsExeDir "bd-example-app"
      adplatformServer         <- serveApp rnd jsExeDir "bd-adplatform"
      interactionsServer       <- serveApp rnd jsExeDir "bd-interactions"
      labelRefinerServer       <- serveApp rnd jsExeDir "bd-label-refiner"
      exampleStaticServer      <- serveApp rnd jsExeDir "bd-example-static-page"
      exampleDynamicServer     <- serveApp rnd jsExeDir "bd-example-dynamic-page"
      exampleWidgetComposition <- serveApp rnd jsExeDir "bd-example-widget-composition"
      exampleApiReq            <- serveApp rnd jsExeDir "bd-example-api-req"
      examplePlots             <- serveApp rnd jsExeDir "bd-example-plots"
      examplePlots2            <- serveApp rnd jsExeDir "bd-example-plots2"
      examplePlots3            <- serveApp rnd jsExeDir "bd-example-plots3"
      examplePlots4            <- serveApp rnd jsExeDir "bd-example-plots4"
      examplePlots5            <- serveApp rnd jsExeDir "bd-example-plots5"
      examplePlots6            <- serveApp rnd jsExeDir "bd-example-plots6"
      exampleHistoryServer     <- serveApp rnd jsExeDir "bd-example-history"
      indexServer              <- serveApp rnd jsExeDir "bd-index"
      docServer <- case docDir of
        Left msg     -> print "Warning: Could not find documentation" >> serveNothing "documentation"
        Right docDir -> serveDir "documentation" docDir

      putStrLn $ "Listening on " ++ show port
      Network.Wai.Handler.Warp.run port $ serve (Proxy::Proxy Layout) $
        exampleServer
          :<|> adplatformServer
          :<|> interactionsServer
          :<|> labelRefinerServer
          :<|> exampleStaticServer
          :<|> exampleDynamicServer
          :<|> exampleWidgetComposition
          :<|> exampleApiReq
          :<|> examplePlots
          :<|> examplePlots2
          :<|> examplePlots3
          :<|> examplePlots4
          :<|> examplePlots5
          :<|> examplePlots6
          :<|> exampleHistoryServer
          :<|> docServer
          :<|> indexServer

-- TODO nicer way of generating a non-existent endpoint?
serveNothing :: String -> IO (Server Raw)
serveNothing hint = serveDir hint "directory-that-does-not-exist-91276341"

serveDir :: String -> String -> IO (Server Raw)
serveDir hint dir = do
  putStrLn $ "Serving " ++ hint ++ ", from"
  putStrLn $ " " ++ dir
  return $ serveDirectory $ dir

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
