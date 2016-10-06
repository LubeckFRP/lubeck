--
{-# LANGUAGE
    TemplateHaskell
  , NoMonomorphismRestriction
  , MultiParamTypeClasses
  , FunctionalDependencies
  , ScopedTypeVariables
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  , TupleSections
  , FlexibleContexts
  , NoImplicitPrelude
  , BangPatterns
  , QuasiQuotes
  #-}

module Main where

import BasePrelude
import Control.Lens.TH

import Control.Lens(Getter, to)
import Control.Lens(_1, _2, _3, _4) -- TODO debug
import Control.Lens.Operators hiding ((<~))
import Data.Functor.Contravariant (Contravariant(..))
import Data.Time(UTCTime, getCurrentTime, formatTime, defaultTimeLocale)
import Linear.Affine (Point(..))
import Linear.V1 (V1(..), _x)
import Linear.V2 (V2(..), _y)
import NeatInterpolation(string)
import qualified Data.Char
import qualified Data.List
import Data.Map(Map)
import qualified Data.Map
import Data.IntMap(IntMap)
import qualified Data.IntMap
import Data.Colour(withOpacity)
import qualified Data.Colour.Names as Colors

import Lubeck.Str (Str, toStr, packStr, unpackStr)
import Lubeck.Drawing (Draft, SVG, RenderingOptions(..), OriginPlacement(..)  )
import Lubeck.DV
import Lubeck.DV.Styling(HoverSelect(..))
import qualified Lubeck.Drawing
import qualified Lubeck.Drawing as D
import qualified Lubeck.DV.Styling

-- TODO HashSVG
import qualified System.Process as S
import NeatInterpolation (string)
import System.Directory(createDirectoryIfMissing, getCurrentDirectory)
import System.IO.Temp(withSystemTempDirectory, withSystemTempFile)
import Crypto.Hash(hash, hashlazy, SHA256, Digest)
import qualified Data.ByteString.Lazy as LB
import Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.FilePath (takeDirectory)
-- TODO end HashSVG

import qualified StandardDrawingTests as Std


-- TODO HashSvg

-- Map String (FilePath -> IO (), FilePath -> IO String)
-- IO FilePath

-- | Name of an image test suite
type TestSuiteName = String

-- | Name of a test image
type ImageName = String

-- | Test image represented as an SVG string.
type SvgString = String

{-|
Hash and store the given image set. Always succeeds.
The given file path should be commited intothe repo to allow other developers/tests to call compareHashes.
-}
updateHashes :: FilePath -> Map ImageName SvgString -> IO ()
updateHashes path oldData = do
  ensureParent path
  oldDataH <- mapM rasterizeAndHashSvgString oldData
  LB.writeFile path $ A.encode oldDataH
  where
    ensureParent path = createDirectoryIfMissing True (takeDirectory path)

-- hashStr :: String -> String
-- hashStr input = show hashRes
--   where
--     hashRes :: Digest SHA256
--     hashRes = hash (TE.encodeUtf8 $ T.pack input)

{-|
Assure given image set is the same as the last call to updateHashes on this machine.
Assumes that somebody called updateHashes on this machine (or commited the result on a different machine).
-}
compareHashes :: TestSuiteName -> FilePath -> Map ImageName SvgString -> IO Bool
compareHashes suiteName path newData = do
  putStrLn $ msg suiteName path
  res <- compareHashes1 path newData
  if Data.Map.null res
    then do
      putStrLn $ "Success"
      return True
    else do
      putStrLn $ "Failed, listing differences: "
      for_ (Data.Map.toList res) $ \(name, (hash1, hash2)) -> do
        putStrLn $ " '" <> name <> "' differed"
        putStrLn $ "    Current codebase gives   " <> showsPrec 0 hash1 ""
        putStrLn $ "    But the cached result is " <> showsPrec 0 hash2 ""
      putStrLn $ "You can run this test again with --report to view the current images"
      putStrLn $ ""
      putStrLn $ "If you have changed/added test suites, verify their looks with report,"
      putStrLn $ "run test with --generate to update hashes, and commit the result"
      return False
  where
    msg a b = "Running image test suite '" <> a <> "' against pregenerated hashes in '" <> b <> "'"

compareHashes1 :: FilePath -> Map ImageName SvgString -> IO (Map ImageName (Maybe String, Maybe SvgString))
compareHashes1 path newData = do
  newHashes <- mapM rasterizeAndHashSvgString newData
  (maybeOldHashes :: Maybe (Map ImageName SvgString)) <- fmap A.decode $ LB.readFile path
  case maybeOldHashes of
    Nothing -> error "No such file"
    Just oldHashes -> return $
      (\f -> unionIncomplete f newHashes oldHashes) $ \name hash1 hash2 ->
        if hash1 == hash2 then Nothing
          else Just $ (hash1, hash2)
      -- Data.Map.unionWithKey (\k h1 h2 -> ) oldHashes

unionIncomplete :: Ord k => (k -> Maybe a -> Maybe b -> Maybe c) -> Map k a -> Map k b -> Map k c
unionIncomplete f m1 m2 = Data.Map.fromList $ fmap (fmap fromJust) $ filter (isJust . snd) $ zip ks (fmap g ks)
  where
    fromJust (Just x) = x
    fromJust _ = error "Impossible"
    g k = f k (Data.Map.lookup k m1) (Data.Map.lookup k m2)
    ks = sort $ nub $ Data.Map.keys m1 <> Data.Map.keys m2


{-
Compute the hash of the given SVG string by rasterizing it and hashing the result.

Yields identical hashes for images that look the same, but doesn't necessarily
have the same internal SVG structure.

Requires PhantomJS 2.1.1. The phantomjs binary must in the path, so that
@phantomjs --version@ prints @2.1.1@.

This a pure function from the file semantics of the file contents to the result.
IO is for exception handling when invoking PhantomJS.
-}
rasterizeAndHashSvgString :: String -> IO String
rasterizeAndHashSvgString contents = withSystemTempDirectory "rasterizeAndHashSvgFile" $ \dir -> do
  let filePath = dir <> "/image.svg"
  writeFile filePath contents
  rasterizeAndHashSvgFile filePath

{-
Compute the hash of an SVG file by rasterizing it and hashing the result.

Yields identical hashes for images that look the same, but doesn't necessarily
have the same internal SVG structure.

Requires PhantomJS 2.1.1. The phantomjs binary must in the path, so that
@phantomjs --version@ prints @2.1.1@.

This a pure function from the file semantics of the file contents to the result.
IO is for file input exception handling when invoking PhantomJS.
-}
rasterizeAndHashSvgFile :: FilePath -> IO String
rasterizeAndHashSvgFile path = do
  hasCorrectPhantomVersion_ <- hasCorrectPhantomVersion
  if not hasCorrectPhantomVersion_
    then error "Requires phantom 2.1.1"
    else do
      runPhantomHashResult path
  where
    -- Assumes hasCorrectPhantomVersion, returns hash
    runPhantomHashResult :: FilePath -> IO String
    runPhantomHashResult svgFilePath = withSystemTempDirectory "rasterizeAndHashSvgFile" $ \dir -> do
      -- phantomjs rasterize.js http://ariya.github.io/svg/tiger.svg tiger.png
      let rasterizeJsPath = dir <> "/rasterize.js"
      let outPath = dir <> "/out.png"
      -- print rasterizeJsPath
      -- print outPath
      writeFile rasterizeJsPath rasterizeJsSrc
      -- S.readProcess "cat" [rasterizeJsPath] "" >>= putStrLn
      _ <- S.readProcess "phantomjs" [rasterizeJsPath, svgFilePath, outPath] ""
      outLb <- LB.readFile outPath
      let (dig :: Digest SHA256) = hashlazy outLb
      return $ show dig
      -- return "asjhdshj"

    hasCorrectPhantomVersion :: IO Bool
    hasCorrectPhantomVersion = do
      res <- try $ S.readProcess "phantomjs" ["--version"] ""
      case res of
        Left e -> const (return False) (e :: SomeException)
        Right res1 -> return $ res1 == "2.1.1\n"


    rasterizeJsSrc :: String
    rasterizeJsSrc =
      [string|
        "use strict";
          var page = require('webpage').create(),
              system = require('system'),
              address, output, size, pageWidth, pageHeight;

          if (system.args.length < 3 || system.args.length > 5) {
              console.log('Usage: rasterize.js URL filename [paperwidth*paperheight|paperformat] [zoom]');
              console.log('  paper (pdf output) examples: "5in*7.5in", "10cm*20cm", "A4", "Letter"');
              console.log('  image (png/jpg output) examples: "1920px" entire page, window width 1920px');
              console.log('                                   "800px*600px" window, clipped to 800x600');
              phantom.exit(1);
          } else {
              address = system.args[1];
              output = system.args[2];
              page.viewportSize = { width: 600, height: 600 };
              if (system.args.length > 3 && system.args[2].substr(-4) === ".pdf") {
                  size = system.args[3].split('*');
                  page.paperSize = size.length === 2 ? { width: size[0], height: size[1], margin: '0px' }
                                                     : { format: system.args[3], orientation: 'portrait', margin: '1cm' };
              } else if (system.args.length > 3 && system.args[3].substr(-2) === "px") {
                  size = system.args[3].split('*');
                  if (size.length === 2) {
                      pageWidth = parseInt(size[0], 10);
                      pageHeight = parseInt(size[1], 10);
                      page.viewportSize = { width: pageWidth, height: pageHeight };
                      page.clipRect = { top: 0, left: 0, width: pageWidth, height: pageHeight };
                  } else {
                      console.log("size:", system.args[3]);
                      pageWidth = parseInt(system.args[3], 10);
                      pageHeight = parseInt(pageWidth * 3/4, 10); // it's as good an assumption as any
                      console.log ("pageHeight:",pageHeight);
                      page.viewportSize = { width: pageWidth, height: pageHeight };
                  }
              }
              if (system.args.length > 4) {
                  page.zoomFactor = system.args[4];
              }
              page.open(address, function (status) {
                  if (status !== 'success') {
                      console.log('Unable to load the address!');
                      phantom.exit(1);
                  } else {
                      window.setTimeout(function () {
                          page.render(output);
                          phantom.exit();
                      }, 200);
                  }
              });
          }
      |]





drawingToSvgString :: RenderingOptions -> Styling -> Styled (Draft SVG) -> Str
drawingToSvgString drawOpts style finalD = Lubeck.Drawing.toSvgStr drawOpts $ Lubeck.Drawing.getDraft $ ($ style) $ Lubeck.DV.Styling.getStyled finalD

drawingToSvgStringUnstyled :: RenderingOptions -> Draft SVG -> Str
drawingToSvgStringUnstyled drawOpts finalD = Lubeck.Drawing.toSvgStr drawOpts $ Lubeck.Drawing.getDraft finalD

visualizeTest :: Show s => [s] -> Geometry SVG -> [Aesthetic s] -> Str
visualizeTest dat geom aess = drawingToSvgString mempty mempty
  $ drawPlot
  $ plot dat aess geom


{-|
Similar to StandardDrawingTests.DrawingTest, except the drawing is now
compiled to an SVG string (see renderDrawingTest)
-}
data RenderedDrawingTest = RenderedDrawingTest
  { ldt_name      :: !String
  , ldt_comment   :: !String
  , ldt_svgString :: !String }
  deriving (Show)

-- Return a map from name to SVG strings, iff the batch has no duplicate names.
testBatchToMap :: [RenderedDrawingTest] -> Maybe (Map String String)
testBatchToMap tests
  | hasDuplicates $ fmap ldt_name tests = Nothing
  | otherwise                           = Just $ Data.Map.fromList
                                          $ fmap (\x -> (ldt_name x, ldt_svgString x)) tests
  where
    hasDuplicates xs = length (nub xs) /= length xs


renderRenderedDrawingTestsToDir :: TestSuiteName -> FilePath -> [RenderedDrawingTest] -> IO ()
renderRenderedDrawingTestsToDir suiteName dir tests = do
  putStrLn $ msg suiteName dir
  renderRenderedDrawingTestsToDir1 dir tests
  where
    msg a b = "Rendering image test suite '" <> a <> "' to to directory '" <> b <> "'"

renderRenderedDrawingTestsToDir1 :: FilePath -> [RenderedDrawingTest] -> IO ()
renderRenderedDrawingTestsToDir1 dir tests = case testBatchToMap tests of
  Nothing -> error "renderRenderedDrawingTestsToFile: Duplicate names"
  Just nameToSvgStrMap -> do
    createDirectoryIfMissing True dir
    for_ (Data.Map.toList nameToSvgStrMap) $ \(name, svgStr) ->
      writeFile (dir <> "/" <> name <> ".svg") svgStr

    let content = concatMap (\name -> makeItem name)
          (Data.Map.keys nameToSvgStrMap)
    dateString <- formatTime defaultTimeLocale "<p>Generated at UTC %F %T</p>" <$> getCurrentTime
    writeFile (dir <> "/" <> "index.html") $ makeIndex dateString content
  where
    makeItem :: String -> String
    -- makeItem name = "<p><a href='" <> name <> ".svg'>" <> name <> "</a></p>\n"
    makeItem name = "<h2>" <> name <> "</h2><p><img src='" <> name <> ".svg' alt='" <> name <> "' /></p>"

    makeIndex :: String -> String -> String
    makeIndex dateString content = mconcat [s1, dateString, content, s2]
      where
        s1 = [string|
          <!DOCTYPE html>
          <html lang="en">
            <head>
              <meta charset="utf-8">
              <title>title</title>
              <link rel="stylesheet" href="style.css">
              <script src="script.js"></script>
            </head>
            <body> |]
        s2 = [string|
            </body>
          </html>
          |]


updateHashesDTs :: TestSuiteName -> FilePath -> [RenderedDrawingTest] -> IO ()
updateHashesDTs _ path tests = case testBatchToMap tests of
  Nothing -> error "updateHashesDTs: Duplicate names"
  Just nameToSvgStrMap -> updateHashes path nameToSvgStrMap

compareHashesDTs :: TestSuiteName -> FilePath -> [RenderedDrawingTest] -> IO Bool
compareHashesDTs suiteName path tests = case testBatchToMap tests of
  Nothing -> error "compareHashesDTs: Duplicate names"
  Just nameToSvgStrMap -> compareHashes suiteName path nameToSvgStrMap

drawingPlusDvTestBatch :: [RenderedDrawingTest]
drawingPlusDvTestBatch =
  renderDrawingTest <$> (Std.drawingTestBatch <> Std.dvTestBatch)

renderDrawingTest :: Std.DrawingTest SVG -> RenderedDrawingTest
renderDrawingTest (Std.DrawingTest n d dr) = RenderedDrawingTest n d (svgDraftToString dr)
  where
    svgDraftToString = unpackStr . drawingToSvgStringUnstyled mempty

{-

<test-name>
  Compares current code against 'lubeck-dv/hashes/test.json'
<test-name> --generate
  Regenerates 'lubeck-dv/hashes/test.json'
  If you change the test suite, you must commit the result, or your test won't pass
<test-name> --report <directory (default: /tmp/lubeck/dv/test/report)>
  Generates a directory with the current version of each test

-}
main :: IO ()
main = do
  args <- getArgs
  main' args

main' :: [String] -> IO ()
main' args = do
  case args of
    ["--generate"] -> do
      updateHashesDTs "Drawing+DV tests" "hashes/test.json" drawingPlusDvTestBatch

    ["--compare"] -> do
      ok <- compareHashesDTs "Drawing+DV tests" "hashes/test.json" drawingPlusDvTestBatch
      if ok then exitSuccess else exitFailure

    -- Same as --compare
    [] -> do
      -- cwd <- getCurrentDirectory
      -- print cwd

      ok <- compareHashesDTs "Drawing+DV tests" "hashes/test.json" drawingPlusDvTestBatch
      if ok then exitSuccess else exitFailure

    ["--report"] -> do
      renderRenderedDrawingTestsToDir "Drawing+DV tests" "/tmp/lubeck/dv/test/report" drawingPlusDvTestBatch


    _ -> error "Bad arguments"

  return ()
