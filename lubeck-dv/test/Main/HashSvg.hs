
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}

module Main.HashSvg where

import BasePrelude

import qualified System.Process as S
import NeatInterpolation (string)
import System.IO.Temp(withSystemTempDirectory, withSystemTempFile)
import Crypto.Hash(hashlazy, SHA256, Digest)
import qualified Data.ByteString.Lazy as LB
import Data.Aeson as A


-- Map String (FilePath -> IO (), FilePath -> IO String)
-- IO FilePath

-- | Name of a test image
type Name = String

-- | Test image represented as an SVG string.
type SvgString = String

{-|
Hash and store the given image set. Always succeeds.
The given file path should be commited intothe repo to allow other developers/tests to call compareHashes.
-}
updateHashes :: FilePath -> Map Name SvgString -> IO ()
updateHashes = undefined

{-|
Assure given image set is the same as the last call to updateHashes on this machine.
Assumes that somebody called updateHashes on this machine (or commited the result on a different machine).
-}
compareHashes :: FilePath -> Map Name SvgString -> IO ()
compareHashes = undefined

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
rasterizeAndHashSvgString contents = withSystemTempFile "" $ \filePath -> do
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
    runPhantomHashResult svgFilePath = withSystemTempDirectory "" $ \dir -> do
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
