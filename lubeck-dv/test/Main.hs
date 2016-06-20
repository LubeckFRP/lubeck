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
--
import Control.Lens(Getter, to)
import Control.Lens(_1, _2, _3, _4) -- TODO debug
import Control.Lens.Operators hiding ((<~))
import Data.Functor.Contravariant (Contravariant(..))
import Data.Time(UTCTime)
import Linear.Affine (Point(..))
import Linear.V1 (V1(..), _x)
import Linear.V2 (V2(..), _y)
import NeatInterpolation(string)
import qualified Data.Char
import qualified Data.List
import Data.Map(Map)
import qualified Data.Map
import Data.Colour(withOpacity)
import qualified Data.Colour.Names as Colors

import Lubeck.Str (Str, toStr, packStr, unpackStr)
import Lubeck.Drawing (Drawing, RenderingOptions(..), OriginPlacement(..)  )
import Lubeck.DV
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





drawingToSvgString :: RenderingOptions -> Styling -> Styled Drawing -> Str
drawingToSvgString drawOpts style finalD = Lubeck.Drawing.toSvgStr drawOpts $ ($ style) $ Lubeck.DV.Styling.getStyled finalD

drawingToSvgStringUnstyled :: RenderingOptions -> Drawing -> Str
drawingToSvgStringUnstyled drawOpts finalD = Lubeck.Drawing.toSvgStr drawOpts $ finalD

visualizeTest :: Show s => [s] -> Geometry -> [Aesthetic s] -> Str
visualizeTest dat geom aess = drawingToSvgString mempty mempty
  $ drawPlot
  $ plot dat aess geom


{-
Test a single image
-}
data DrawingTest = DrawingTest
  { dt_name      :: !String
  , dt_comment   :: !String
  , dt_svgString :: !String }
  deriving (Show)

-- Return a map from name to SVG strings, iff the batch has no duplicate names.
testBatchToMap :: [DrawingTest] -> Maybe (Map String String)
testBatchToMap tests
  | hasDuplicates $ fmap dt_name tests = Nothing
  | otherwise                          = Just $ Data.Map.fromList
                                          $ fmap (\x -> (dt_name x, dt_svgString x)) tests
  where
    hasDuplicates xs = length (nub xs) /= length xs


renderDrawingTestsToDir :: TestSuiteName -> FilePath -> [DrawingTest] -> IO ()
renderDrawingTestsToDir suiteName dir tests = do
  putStrLn $ msg suiteName dir
  renderDrawingTestsToDir1 dir tests
  where
    msg a b = "Rendering image test suite '" <> a <> "' to to directory '" <> b <> "'"

renderDrawingTestsToDir1 :: FilePath -> [DrawingTest] -> IO ()
renderDrawingTestsToDir1 dir tests = case testBatchToMap tests of
  Nothing -> error "renderDrawingTestsToFile: Duplicate names"
  Just nameToSvgStrMap -> do
    createDirectoryIfMissing True dir
    for_ (Data.Map.toList nameToSvgStrMap) $ \(name, svgStr) ->
      writeFile (dir <> "/" <> name <> ".svg") svgStr

    let content = concatMap (\name -> makeItem name)
          (Data.Map.keys nameToSvgStrMap)
    writeFile (dir <> "/" <> "index.html") $ makeIndex content
  where
    makeItem :: String -> String
    -- makeItem name = "<p><a href='" <> name <> ".svg'>" <> name <> "</a></p>\n"
    makeItem name = "<h2>" <> name <> "</h2><p><img src='" <> name <> ".svg' alt='" <> name <> "' /></p>"

    makeIndex :: String -> String
    makeIndex a = s1 <> a <> s2
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


updateHashesDTs :: TestSuiteName -> FilePath -> [DrawingTest] -> IO ()
updateHashesDTs _ path tests = case testBatchToMap tests of
  Nothing -> error "updateHashesDTs: Duplicate names"
  Just nameToSvgStrMap -> updateHashes path nameToSvgStrMap

compareHashesDTs :: TestSuiteName -> FilePath -> [DrawingTest] -> IO Bool
compareHashesDTs suiteName path tests = case testBatchToMap tests of
  Nothing -> error "compareHashesDTs: Duplicate names"
  Just nameToSvgStrMap -> compareHashes suiteName path nameToSvgStrMap



-- TEST

newtype Day = Day Int
  deriving (Eq, Ord, Show, Num, Real, HasScale)

data LikeType = ProjLike | RealLike
  deriving (Eq, Ord, Show)

instance HasScale LikeType where scale _ = categorical

data LikeCount = LikeCount { _time :: Day, _count :: Int, _likeType :: LikeType }
  deriving (Eq, Ord, Show)

$(makeLenses ''LikeCount)

likeCounts :: [LikeCount]
likeCounts =
  [ LikeCount 0 112000 RealLike
  , LikeCount 1 115000 RealLike
  , LikeCount 1 118000 RealLike
  , LikeCount 0 112000 ProjLike
  , LikeCount 1 113000 ProjLike
  , LikeCount 1 114000 ProjLike
  ]

test0 = visualizeTest likeCounts fill
  [ x     <~ time
  , y     <~ count
  , color <~ likeType
  ]


newtype Name = Name String deriving (Eq, Ord, Show, IsString)
data Gender = Female | Male deriving (Eq, Ord, Show)

instance HasScale Name where
  scale = const categorical

instance HasScale Gender where
  scale = const categorical

data Person = P1
  { personName :: Name
  , personAge :: Int
  , personHeight :: Double
  }
  deriving (Eq, Ord, Show)

data Person2 = P2
  { person2Name   :: Name
  , person2Age    :: Int
  , person2Height :: Double
  , person2Gender :: Gender
  }
  deriving (Eq, Ord, Show)

$(makeFields ''Person)
$(makeFields ''Person2)

males, females :: [Person]
males =
  [ P1 "Hans" 28 1.15
  , P1 "Sven" 25 1.15
  , P1 "Sven" 25 1.15
  , P1 "Sven" 25 1.15
  , P1 "Sven" 25 1.15
  , P1 "Sven" 25 1.15
  ]
females =
  [ P1 "Elin" 21 1.15
  , P1 "Alva" 19 1.15
  ]

people :: [Person2]
people = (males `cr` [Male]) <> (females `cr` [Female])
  where
    cr = crossWith (\p gender -> P2 (p^.name) (p^.age) (p^.height) gender)

test = DrawingTest "test1" "" $ unpackStr
  $ visualizeTest people (mconcat [pointG, line, fill])
  [ mempty
  , color <~ gender
  -- , shape <~ gender
  , x     <~ name
  , y     <~ age `withScale` linear
  ]
test2 = DrawingTest "test2" "" $ unpackStr
  $ visualizeTest ([(1,2), (3,4)] :: [(Int, Int)]) line
  [ mempty
  , x <~ _1
  , y <~ to snd
  ]
test3 = DrawingTest "test3" "" $ unpackStr
  $ visualizeTest ( [ ] :: [(UTCTime, Int)]) line
  [ mempty
  , x <~ to fst
  , y <~ to snd
  ]

test4 = DrawingTest "test4" "" $ unpackStr
  $ visualizeTest ("hello world" :: String) pointG [x <~ id, y <~ id]



data WD = Mon | Tues | Wed | Thurs | Fri | Sat | Sun deriving (Eq, Ord, Enum, Bounded) -- Show,
instance Show WD where
  show x = case x of
    Mon   -> "m"
    Tues  -> "t"
    Wed   -> "w"
    Thurs -> "tr"
    Fri   -> "f"
    Sat   -> "sa"
    Sun   -> "su"

instance HasScale WD where
  scale = const categoricalEnum

test5 = DrawingTest "test5" "" $ unpackStr
  $ visualizeTest [(Mon, 100 :: Int), (Sun, 400)] line [x <~ to fst, y <~ to snd]


test6 = DrawingTest "test6" "" $ unpackStr
  $ visualizeTest dat geom aes
 where
  dat =
    [ (Mon,   10)
    , (Tues,  30)
    , (Wed,   3)
    , (Thurs, 3)
    , (Fri,   12)
    , (Sat,   3)
    , (Sun,   10 :: Int)]
  aes =
    [ x <~ to fst
    , y <~ to snd
    ]
  geom = mconcat [pointG, line, fill]

test7 = DrawingTest "test7" "" $ unpackStr
  $ visualizeTest dat (mconcat [pointG, line, fill])
  [ mempty
  , x     <~ to (\(x,_,_) -> x)
  , y     <~ to (\(_,x,_) -> x)
  , color <~ to (\(_,_,x) -> x)
  ]
  where
    dat =
      [ (0::Int, 1::Int, True)
      , (1, 3, True)
      , (2, 0, True)
      , (3, 2, True)
      , (5, 9, True)

      , (0, 3, False)
      , (1, 2, False)
      , (2, 1, False)
      , (3, 0, False)
      , (5, 0, False)
      ]


-- test8
-- The same data plotted in 3 different ways:

-- Version I: Cross with True/False and plot 2 overlapping lines/areas
-- FIXME not working
test8a = DrawingTest "test8a" "" $ unpackStr
  $ visualizeTest dat2 (mconcat [line, fill])
  [ x     <~ _1
  , y     <~ _2
  , color <~ _3
  ]
  where
    dat2 = fmap (\a -> (a^._1, a^._2, False)) dat <> fmap (\a -> (a^._1, a^._3, True)) dat
    dat =
     [ (0, 3, 12)
     , (1, 1, 12)
     , (2, 1, 16)
     , (3, 5, 16)
     , (4, 16, 1) :: (Int, Int, Int)
     ]
-- Version II: Cross with True/False use area plot with bound aesthetic (lower/upper)
test8b = DrawingTest "test8b" "" $ unpackStr
  $ visualizeTest dat2 (mconcat [area2])
  [ x     <~ _1
  , y     <~ _2
  , bound <~ _3
  ]
  where
    dat2 :: [(Int,Int,Bool)]
    dat2 = fmap (\a -> (a^._1, a^._2, False)) dat <> fmap (\a -> (a^._1, a^._3, True)) dat
    dat =
     [ (0, 3, 12)
     , (1, 1, 12)
     , (2, 1, 16)
     , (3, 5, 16)
     , (4, 16, 1) :: (Int, Int, Int)
     ]
-- Version III: Use are a plot with "two y values" (questionable).
-- Note that y and yMin needs to have the same bounds for this to work (here [1..16])
test8c = DrawingTest "test8c" "" $ unpackStr
  $ visualizeTest dat (mconcat [area])
  [ x    <~ _1
  , yMin <~ _2
  , y    <~ _3
  ]
  where
    dat =
     [ (0, 3, 12)
     , (1, 1, 12)
     , (2, 1, 16)
     , (3, 5, 16)
     , (4, 16, 1) :: (Int, Int, Int)
     ]

-- Cross-lines
test9 = DrawingTest "test9" "" $ unpackStr
  $ visualizeTest dat (mconcat [pointG, xIntercept, yIntercept])
  [ x <~ _1 `withScale` categorical
  , y <~ _2 `withScale` linearIntegral
  , crossLineX <~ _3
  , crossLineY <~ _4
  ]
  where
    dat :: [(Int,Int,Bool,Bool)]
    dat = zip4
      [1..4] [1..4]
      [True,False,False,True] [False,False,True,True]

-- Labels and custom images
test10 = DrawingTest "test10" "" $ unpackStr
  $ visualizeTest dat (mconcat [labelG, pointG, imageG])
  [ x <~ _1 `withScale` categorical
  , y <~ _2 `withScale` linearIntegral
  , contramap (("value is "<>). toStr) label <~ _1
  , contramap (const customDr) image <~ _2
  ]
  where
    customDr :: Drawing
    customDr = Lubeck.Drawing.fillColor Colors.whitesmoke
      $ Lubeck.Drawing.scale 50 $ Lubeck.Drawing.square

    dat :: [(Int,Int)]
    dat = zip
      [1..4] [1..4]

-- Custom image.
test11 = DrawingTest "test11" "" $ unpackStr
  $ visualizeTest dat (mconcat [labelG, pointG, imageG])
  [ x <~ _1 `withScale` categorical
  , y <~ _2 `withScale` linearIntegral
  , contramap (const customDr) image <~ _2
  ]
  where
    customDr :: Drawing
    customDr = Lubeck.Drawing.fillColor Colors.turquoise
      $ Lubeck.Drawing.scale 50 $ Lubeck.Drawing.triangle

    dat :: [(Int,Int)]
    dat = zip
      [1..4] [1..4]

-- Custom image with size.
test12 = DrawingTest "test12" "" $ unpackStr
  $ visualizeTest dat (mconcat [labelG, pointG, imageG])
  [ x <~ _1 `withScale` categorical
  , y <~ _2 `withScale` linearIntegral
  , size <~ _1
  , contramap (const customDr) image <~ _2
  ]
  where
    customDr :: Drawing
    customDr = Lubeck.Drawing.fillColor Colors.turquoise
      $ Lubeck.Drawing.scale 50 $ Lubeck.Drawing.triangle

    dat :: [(Int,Int)]
    dat = zip
      [1..4] [1..4]

-- Custom image with size (linearly transformed).
test13 = DrawingTest "test13" "" $ unpackStr
  $ visualizeTest dat (mconcat [labelG, pointG, imageG])
  [ x <~ _1 `withScale` categorical
  , y <~ _2 `withScale` linearIntegral
  , contramap (\x -> -1*x + 1) size <~ _1
  , contramap (const customDr) image <~ _2
  ]
  where
    customDr :: Drawing
    customDr = Lubeck.Drawing.fillColor Colors.turquoise
      $ Lubeck.Drawing.scale 50 $ Lubeck.Drawing.triangle

    dat :: [(Int,Int)]
    dat = zip
      [1..4] [1..4]

-- Custom image with size (linearly transformed).
test14 = DrawingTest "test14" "" $ unpackStr
  $ visualizeTest dat (mconcat [labelG, pointG, imageG])
  [ x <~ _1 `withScale` categorical
  , y <~ _2 `withScale` linearIntegral
  , contramap (\x -> -1*x + 1) size <~ _1
  , contramap (const customDr) image <~ _2
  ]
  where
    customDr :: Drawing
    customDr = Lubeck.Drawing.fillColor Colors.turquoise
      $ Lubeck.Drawing.scale 50 $ dr

    dat :: [(Int,Int)]
    dat = zip
      [1..4] [1..4]

    Just dr =
      Lubeck.Drawing.addEmbeddedSVGFromStr $ packStr $ [string|
              <rect x="-0.5" y="-0.5" width="1" height="1" style="fill:blue">
                        <animateTransform attributeName="transform"
                              attributeType="XML"
                              type="rotate"
                              from="0 0 0"
                              to="360 0 0"
                              dur="10s"
                              repeatCount="indefinite"/>
              </rect>

      |]


-- Note: no test15-19


-- Multiple plots composed

test20 = DrawingTest "test20" "" $ unpackStr
  $ drawingToSvgString mempty mempty $ drawPlot $
  plot (zip "hans" "sven" :: [(Char,Char)]) [x<~_1,y<~_2] pointG
    <>
  plot (zip "hans" "svfn" :: [(Char,Char)]) [x<~_1,y<~_2] line


{-
  Same type/scale.
-}
test21 = DrawingTest "test21" "" $ unpackStr
  $ drawingToSvgString mempty mempty $ drawPlot $
  plot (zip
    ([1..10] :: [Int])
    ([2,5,1,2,5,-6,7,2,3,9] :: [Int])
    )
    [x<~_1,y<~_2]
    line
    <>
  plot (zip
    ([1..10] :: [Int])
    ([12,5,1,2,5,-6,7,2,13,15] :: [Int])
    )
    [x<~_1,y<~_2]
    fill

{-
  Different Y scales
-}
test22 = DrawingTest "test22" "" $ unpackStr
  $ drawingToSvgString mempty mempty $ drawPlot $
  plot (zip
    ([1..10] :: [Int])
    ([0,1,5,5,4,3,4,4,4,3] :: [Int])
    ) [x<~_1,y<~_2]
    line
    <>
  plot (zip
    ([1..10] :: [Int])
    ("AAABBBZQqz" :: [Char])
    )
    [x<~_1,y<~_2]
    fill


{-
  Different X scales
-}
test23 = DrawingTest "test23" "" $ unpackStr
  $ drawingToSvgString mempty mempty $ drawPlot $
  plot (zip
    ([1..10] :: [Int])
    ([0,1,5,5,4,3,4,4,4,3] :: [Int])
    ) [x<~_1,y<~_2]
    line
    <>
  plot (zip
    ([11..20] :: [Int])
    ("AAABBBZQqz" :: [Char])
    )
    [x<~_1,y<~_2]
    fill

{-
  Same type/scale.
-}
test24 = DrawingTest "test24" "" $ unpackStr
  $ drawingToSvgString mempty mempty $ drawPlot $ mconcat $ zipWith putTogether geoms dat
  where
    putTogether = \geom dat -> plot (zip [1..10::Int] dat) [x<~_1,y<~_2] geom
    geoms = [pointG, line, fill, pointG <> line]
    dat =
      [ [2,5,1,2,5,-6,7,2,3,9] :: [Int]
      , [1,1,1,1,2,2,2,2,3,3]
      , [-1,-2,-3,-3,-3,-3,-3,-3,-3,-3]
      ]


{-
  Bar plot.
-}
test25 = DrawingTest "test25" "" $ unpackStr
  $ drawingToSvgString mempty mempty $ drawPlot $
  plot (zip chars freq) [x <~ _1, y <~ _2] bars
  where
    chars :: [Char]
    freq :: [Int]
    chars = sortNub text
    freq = fmap (\c -> length $ filter (== c) text) chars
    text = filter Data.Char.isAlpha $ fmap Data.Char.toUpper $ [string|
      Statistics is the study of the collection, analysis, interpretation,
      presentation, and organization of data.[1] In applying statistics
      to, e.g., a scientific, industrial, or societal problem, it is
      conventional to begin with a statistical population or a statistical
      model process to be studied. Populations can be diverse topics such
      as "all people living in a country" or "every atom composing a
      crystal". Statistics deals with all aspects of data including the
      planning of data collection in terms of the design of surveys and
      experiments.
      |]
    sortNub = Data.List.nub . Data.List.sort


{-
  Bar plot (horizontal).
-}
test26 = DrawingTest "test26" "" $ unpackStr
  $ drawingToSvgString mempty (barPlotOrientation .~ Horizontal $ mempty) $ drawPlot $
  plot (zip chars freq) [x <~ _1, y <~ _2] bars
  where
    chars :: [Char]
    freq :: [Int]
    chars = sortNub text
    freq = fmap (\c -> length $ filter (== c) text) chars
    text = filter Data.Char.isAlpha $ fmap Data.Char.toUpper $ [string|
      Statistics is the study of the collection, analysis, interpretation,
      presentation, and organization of data.[1] In applying statistics
      to, e.g., a scientific, industrial, or societal problem, it is
      conventional to begin with a statistical population or a statistical
      model process to be studied. Populations can be diverse topics such
      as "all people living in a country" or "every atom composing a
      crystal". Statistics deals with all aspects of data including the
      planning of data collection in terms of the design of surveys and
      experiments.
      |]
    sortNub = Data.List.nub . Data.List.sort


-- TODO test30 and test31 should be the same
test30 = DrawingTest "test30" "" $ unpackStr
  $ drawingToSvgString
  -- (mempty { dimensions = P (V2 800 500), originPlacement = BottomLeft })
  -- (renderingRectangle .~ V2 800 500 $ mempty)
  mempty
  mempty
  $ drawPlot $ mconcat
    [ plot dat [x<~to (!! 0), y<~to (!! 1),       ((2::Double) >$ color)] line
    -- , plot dat [x<~to (!! 0), y<~to (!! 2),       ((1::Double) >$ color)] line
    -- , plot dat [x<~to (!! 0), (0.3::Double) >$ y, ((2::Double) >$ color)] line
    ]
  where
    dat = [ [x,cos x,sin x :: Double] | x <- [0,0.1..pi*2] ]

test31 = DrawingTest "test31" "" $ unpackStr
  $ drawingToSvgString
  -- (mempty { dimensions = P (V2 800 500), originPlacement = BottomLeft })
  -- (renderingRectangle .~ V2 800 500 $ mempty)
  mempty
  mempty
  $ drawPlot $ mconcat
    [ plot dat1 [x<~to (!! 0), y<~to (!! 1), ((0::Double) >$ color), ((0::Double) >$ lineType)] line
    , plot dat2 [x<~to (!! 0), y<~to (!! 1), ((1::Double) >$ color), ((1::Double) >$ lineType)] line
    , plot dat3 [x<~to (!! 0), y<~to (!! 1), ((2::Double) >$ color), ((2::Double) >$ lineType)] line
    ]
  where
    dat1 = [ [x,sin x] :: [Double] | x <- [0,0.1..pi*2] ]
    dat2 = [ [x,cos x] :: [Double] | x <- [0,0.1..pi*2] ]
    dat3 = [ [x,1    ] :: [Double] | x <- [0,0.1..pi*2] ]

test32 = DrawingTest "test32" "" $ unpackStr
  $ drawingToSvgString
  -- (mempty { dimensions = P (V2 800 500), originPlacement = BottomLeft })
  -- (renderingRectangle .~ V2 800 500 $ mempty)
  mempty
  mempty
  $ drawPlot $ mconcat
    [ plot dat1 [x<~to (!! 0), y<~to (!! 1), ((0::Double) >$ color)] pointG
    , plot dat2 [x<~to (!! 0), y<~to (!! 1), ((1::Double) >$ color)] pointG
    , plot dat3 [x<~to (!! 0), y<~to (!! 1), ((2::Double) >$ color)] pointG
    ]
  where
    dat1 = [ [x,sin x] :: [Double] | x <- [0,0.1..pi*2] ]
    dat2 = [ [x,cos x] :: [Double] | x <- [0,0.1..pi*2] ]
    dat3 = [ [x,1    ] :: [Double] | x <- [0,0.1..pi*2] ]

-- TODO
test33 = DrawingTest "test33" "" $ unpackStr
  $ drawingToSvgString
  -- (mempty { dimensions = P (V2 800 500), originPlacement = BottomLeft })
  -- (renderingRectangle .~ V2 800 500 $ mempty)
  mempty
  mempty
  $ drawPlot $ mconcat
    [ plot (dat1 <> dat2) [x<~to (!! 0), y<~to (!! 1)
      , bound <~ to (!! 2)
      , ((23::Double) >$ color)] area2
    -- , plot dat2 [x<~to (!! 0), y<~to (!! 1), ((1::Double) >$ color)] area2
    ]
  where
    dat1 = [ [x,sin x,0] :: [Double] | x <- [0,0.1..pi*2] ]
    dat2 = [ [x,cos x,1] :: [Double] | x <- [0,0.1..pi*2] ]
    dat3 = [ [x,1    ] :: [Double] | x <- [0,0.1..pi*2] ]


drTest1 = DrawingTest
  "drTest1"
  [string|
    A centered red circle, radius 10.
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ D.scale 50
  $ D.fillColor Colors.red
  $ D.circle

drTest2 = DrawingTest
  "drTest2"
  [string|
    A centered blue circle, radius 10.
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ D.scale 50
  $ D.fillColor Colors.blue
  $ D.circle

drTest3 = DrawingTest
  "drTest3"
  [string|
    A red circle on top of a blue square, both centered.
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ c <> s
  where
    c = D.scale 50 $ D.fillColor Colors.red D.circle
    s = D.scale 50 $ D.fillColor Colors.blue D.square

drTest4 = DrawingTest
  "drTest4"
  [string|
    A transparent image.
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ mempty

drTest5 = DrawingTest
  "drTest5"
  [string|
    A semi-transparent triangle.
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ D.scale 50
  $ D.fillColor Colors.blue
  $ D.triangle

drTest6 = DrawingTest
  "drTest6"
  [string|
    A square moved to the right.
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ (<> D.xyAxis)
  $ D.translateX 100
  $ D.scale 100
  $ D.fillColor Colors.blue
  $ D.square

drTest7 = DrawingTest
  "drTest7"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ (<> D.xyAxis)
  $ D.translateX 10
  $ D.scale 100
  $ D.strokeWidth 5
  $ D.strokeColor Colors.green
  $ D.verticalLine

drTest7b = DrawingTest
  "drTest7b"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ (<> D.xyAxis)
  $ D.translateX 10
  $ D.strokeWidth 5
  $ D.scale 100
  $ D.strokeColor Colors.green
  $ D.verticalLine

drTest8 = DrawingTest
  "drTest8"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ (<> D.xyAxis)
  $ D.translateY 10
  $ D.scale 100
  $ D.strokeWidth 5
  $ D.strokeColor Colors.green
  $ D.horizontalLine

drTest9 = DrawingTest
  "drTest9"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ (<> D.xyAxis)
  $ mconcat [
    (D.translate (V2 50 50)       $ D.text "1st")
  , (D.translate (V2 (-50) 50)    $ D.text "2nd")
  , (D.translate (V2 (-50) (-50)) $ D.text "3rd")
  , (D.translate (V2 50 (-50))    $ D.text "4th")
  ]

drTest10 = DrawingTest
  "drTest10"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ (<> D.xyAxis)
  $ mconcat [
    (D.translate (V2 21 63) $ D.strokeColor Colors.green $ D.scale 10 $ D.square)
  , (D.translate (V2 01 22) $ D.strokeColor Colors.green $ D.scale 11 $ D.square)
  , (D.translate (V2 31 51) $ D.strokeColor Colors.green $ D.scale 12 $ D.square)
  , (D.translate (V2 99 41) $ D.strokeColor Colors.green $ D.scale 13 $ D.square)
  , (D.translate (V2 71 17) $ D.strokeColor Colors.green $ D.scale 14 $ D.square)
  ]

drTest11 = DrawingTest
  "drTest11"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ (<> D.xyAxis)
  $ mconcat [
    D.rotate (D.turn/2) b
  , D.rotate (D.turn/3) b
  , D.rotate (D.turn/4) b
  , D.rotate (D.turn/5) b
  , D.rotate (D.turn/6) b
  , D.rotate (D.turn/7) b
  ]
  where
    b = (D.translate (V2 100 0) $ D.fillColor Colors.green $ D.scale 10 $ D.square)

drTest12 = DrawingTest
  "drTest12"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ (<> D.xyAxis)
  $ mconcat [
    D.rotate (0*D.turn/5) b
  , D.rotate (1*D.turn/5) b
  , D.rotate (2*D.turn/5) b
  , D.rotate (3*D.turn/5) b
  , D.rotate (4*D.turn/5) b
  ]
  where
    b = (D.translate (V2 100 0) $ D.fillColor Colors.green $ D.scale 10 $ D.square)

drTest13 = DrawingTest
  "drTest13"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ (<> D.xyAxis)
  $ mconcat [
    D.fitInsideRect r a
  , D.transform (D.rectToTransf r) b
  ]
  where
    r = D.rect 10 40 200 250
    a = (D.fillColorA (Colors.green `withOpacity` 0.2) $ D.scale 10 $ D.square)
    b = (D.fillColor Colors.green $ D.scale 0.5 $ D.square)

-- Angle/Point/Vector geometry
-- Directions
-- Rect/LineSet

-- All transformations
drTestTransf1 = DrawingTest
  "drTestTransf1"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty
drTestTransf2 = DrawingTest
  "drTestTransf2"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty
drTestTransf3 = DrawingTest
  "drTestTransf3"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty
drTestTransf4 = DrawingTest
  "drTestTransf4"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty
drTestTransf5 = DrawingTest
  "drTestTransf5"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty

-- Basic styles (which?)
drTestStyles1 = DrawingTest
  "drTestStyles1"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty
drTestStyles2 = DrawingTest
  "drTestStyles2"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty
drTestStyles3 = DrawingTest
  "drTestStyles3"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty
drTestStyles4 = DrawingTest
  "drTestStyles4"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty
drTestStyles5 = DrawingTest
  "drTestStyles5"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty
drTestStyles6 = DrawingTest
  "drTestStyles6"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty
drTestStyles7 = DrawingTest
  "drTestStyles7"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty

-- Text API
drTestText1 = DrawingTest
  "drTestText1"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty
drTestText2 = DrawingTest
  "drTestText2"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty
drTestText3 = DrawingTest
  "drTestText3"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty
drTestText4 = DrawingTest
  "drTestText4"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty
drTestText5 = DrawingTest
  "drTestText5"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty

-- Embedded SVG
drTestEmbed = DrawingTest
  "drTestEmbed"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty


-- Masking

drTestMasking1 = DrawingTest
  "drTestMasking1"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty

drTestMasking2 = DrawingTest
  "drTestMasking2"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty

-- Envelopes

drTestEnvelope1 = DrawingTest
  "drTestEnvelope1"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty

drTestEnvelope2 = DrawingTest
  "drTestEnvelope2"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty


-- Polygons
drTestPolygon1 = DrawingTest
  "drTestPolygon1"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty

drTestPolygon2 = DrawingTest
  "drTestPolygon2"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty


-- Rendering options

drTestRO1 = DrawingTest
  "drTestRO1"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty

drTestRO2 = DrawingTest
  "drTestRO2"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty

drTestRO3 = DrawingTest
  "drTestRO3"
  [string|
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  mempty

drTestU1 = DrawingTest
  "drTestU1"
  [string|
    Direction pointing to TR corner.
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ D.showDirection (D.dir $ V2 1 1)
drTestU2 = DrawingTest
  "drTestU2"
  [string|
    Direction pointing to BL corner.
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ D.showDirection (D.dir $ V2 (-1) (-1))
drTestU3 = DrawingTest
  "drTestU3"
  [string|
    Direction pointing to BR corner.
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ D.showDirection (D.dir $ V2 1 (-1))

drTestU4 = DrawingTest
  "drTestU4"
  [string|
    The points (100, 20) (-30,400)
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ D.showPoint (P $ V2 100 20) <> D.showPoint (P $ V2 (-30) 400)

drTestU5 = DrawingTest
  "drTestU5"
  [string|
    The points (100, 20) (-30,400)
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ mempty

drTestU6 = DrawingTest
  "drTestU6"
  [string|
    Envelope across the (1,1) diagonal for a square of size 100.
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ D.showEnvelope (D.posDiagonal)
  $ D.scale 100
  $ D.fillColor Colors.pink D.square

drTestU7 = DrawingTest
  "drTestU7"
  [string|
    The unit vector.
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ D.showUnitX



-- Template:

-- drTestU1 = DrawingTest
--   "drTestU1"
--   [string|
--   |]
--   $ unpackStr $ drawingToSvgStringUnstyled mempty
--   mempty





-- testRad = drawingToSvgString
--   (mempty { dimensions = P (V2 800 500), originPlacement = BottomLeft })
--   (renderingRectangle .~ V2 800 500 $ mempty) $ drawPlot $ mconcat
--     [ plot dat [x<~to (!! 0), y<~to (!! 1)] line
--     , plot dat [x<~to (!! 0), y<~to (!! 2)] line
--     ]
--   where
--     dat = dataset1
--
--
-- testRad2 = drawingToSvgString
--   -- (mempty { dimensions = P (V2 800 500), originPlacement = BottomLeft })
--   -- (renderingRectangle .~ V2 800 500 $ mempty)
--   mempty
--   mempty
--   $ drawPlot $ mconcat
--     [ plot dat [x<~to (!! 0), y<~to (!! 1)] line
--     , plot dat [x<~to (!! 0), y<~to (!! 2)] line
--     ]
--   where
--     dat = [ [x,cos x,sin x :: Double] | x <- [0,0.1..pi*2] ]
--




dataset1 :: [[Double]]
dataset1 =
      [
          [2,5.5,3.4  ],
          [3,7.9,9.4  ],
          [4,8.5,13  ],
          [5,4.9,3.2  ],
          [6,2.7,1.4  ],
          [7,5.7,1.6  ],
          [8,6.5,0.6  ],
          [9,6.8,0.4  ],
          [10,2.2,0  ],
          [11,1.4,0  ],
          [12,1.6,0  ],
          [13,-0.1,0  ],
          [14,1.7,3.4  ],
          [15,-1.1,0  ],
          [16,-1.2,0  ],
          [17,-1.4,0  ],
          [18,-3.8,0  ],
          [19,-2.6,0  ],
          [20,2.6,15.4  ],
          [21,5.5,3.2  ],
          [22,5.1,2.2  ],
          [23,5.5,2.7  ],
          [24,7.5,13  ],
          [25,6.8,12.8  ],
          [26,4,3.4  ],
          [27,2.5,0  ],
          [28,2.7,0.5  ],
          [29,5.4,11  ],
          [30,7.6,0.8  ],
          [31,6.8,1.8  ],
          [32,4.9,6.6  ],
          [33,4.9,0  ],
          [34,4.7,0  ],
          [35,4.1,0  ],
          [36,3.2,0  ],
          [37,4.8,0  ],
          [38,5.1,0  ],
          [39,6.4,5.6  ],
          [40,5.6,12.2  ],
          [41,8.1,3.6  ],
          [42,7.5,0.2  ],
          [43,6.4,2.8  ],
          [44,5.9,1  ],
          [45,4.1,0.4  ],
          [46,3.2,0  ],
          [47,4.2,5  ],
          [48,6.8,4.8  ],
          [49,4.3,20.6  ],
          [50,3.7,0  ],
          [51,4.4,2.7  ],
          [52,8.3,1.2  ],
          [53,7.1,0  ],
          [54,4.2,0.2  ],
          [55,5.2,28  ],
          [56,6,0.8  ],
          [57,0.8,0.2  ],
          [58,0.2,0  ],
          [59,2.4,0.2  ],
          [60,2.2,6.4  ],
          [61,3.9,0.4  ],
          [62,3.6,2.6  ],
          [63,9.3,0.2  ],
          [64,8.5,0.6  ],
          [65,4.4,8.7  ],
          [66,3,0  ],
          [67,2.1,0  ],
          [68,4.6,0  ],
          [69,5.1,7.6  ],
          [70,6.9,7.4  ],
          [71,6.2,0.2  ],
          [72,4.6,6.6  ],
          [73,3,0.2  ],
          [74,4.4,2.8  ],
          [75,8.4,14.2  ],
          [76,6.6,2  ],
          [77,7.2,0.4  ],
          [78,3.9,0.6  ],
          [79,2.4,0  ],
          [80,4.2,6.7  ],
          [81,4,0.2  ],
          [82,3.1,0  ],
          [83,5.1,0  ],
          [84,5.7,0  ],
          [85,8,0  ],
          [86,6.7,2.4  ],
          [87,9.3,0.4  ],
          [88,8.9,2.6  ],
          [89,7.4,8.6  ],
          [90,5.8,11.2  ],
          [91,3.7,2.6  ],
          [92,6.8,4.5  ],
          [93,9.9,0  ],
          [94,6.7,1.8  ],
          [95,4.3,0  ],
          [96,6.2,0  ],
          [97,4.8,0  ],
          [98,6.2,0  ],
          [99,8.7,0  ],
          [100,11.5,0  ],
          [101,11.6,0  ],
          [102,8.9,4.2  ],
          [103,8.4,1.2  ],
          [104,8.2,0  ],
          [105,7.9,0  ],
          [106,10.1,0  ],
          [107,9.5,8.4  ],
          [108,5.7,1  ],
          [109,10.2,1  ],
          [110,7.5,8.2  ],
          [111,9.3,0.6  ],
          [112,8.8,0  ],
          [113,13,0  ],
          [114,13.1,0  ],
          [115,11,3.6  ],
          [116,11.9,4  ],
          [117,11.1,0.2  ],
          [118,8,1.6  ],
          [119,10.3,0.2  ],
          [120,11.6,0.9  ],
          [121,10.7,5.1  ],
          [122,9,0  ],
          [123,7.4,0  ],
          [124,9.1,7.8  ],
          [125,10.5,0  ],
          [126,8.4,0  ],
          [127,9.7,0  ],
          [128,12.1,0  ],
          [129,13.7,0  ],
          [130,9.8,0  ],
          [131,8.6,0  ],
          [132,9.3,0  ],
          [133,12.8,0  ],
          [134,14.6,0  ],
          [135,15.9,0  ],
          [136,12.9,0  ],
          [137,12.4,0  ],
          [138,10.5,0  ],
          [139,9.3,0  ],
          [140,10.6,0  ],
          [141,12.4,0  ],
          [142,13.8,7  ],
          [143,11.1,0.3  ],
          [144,11.4,0.3  ],
          [145,10.9,0.2  ],
          [146,15,0  ],
          [147,16.6,0  ],
          [148,12.8,0  ],
          [149,13,0  ],
          [150,11.8,0  ],
          [151,12.1,8  ],
          [152,12.4,1.2  ],
          [153,15.8,4.4  ],
          [154,13.5,1  ],
          [155,11.5,0  ],
          [156,9.9,0.5  ],
          [157,13.3,0  ],
          [158,10.2,0.2  ],
          [159,11.6,12.2  ],
          [160,11.1,2.2  ],
          [161,12.2,0  ],
          [162,12.7,0  ],
          [163,13.8,0  ],
          [164,14.5,0  ],
          [165,14.2,0  ],
          [166,11.8,0  ],
          [167,12.1,0  ],
          [168,15.3,0.6  ],
          [169,15,0.4  ],
          [170,13.9,1.5  ],
          [171,14.4,0  ],
          [172,14.8,0  ],
          [173,16.3,0  ],
          [174,13.1,10.8  ],
          [175,13.8,5  ],
          [176,12,0  ],
          [177,14,0  ],
          [178,16.5,0  ],
          [179,13,0  ],
          [180,13.3,1.8  ],
          [181,16.7,0.2  ],
          [182,15.5,3.2  ],
          [183,16.2,0  ],
          [184,13,4.3  ],
          [185,14.3,8.9  ],
          [186,12.9,0  ],
          [187,15,0  ],
          [188,16.3,0  ],
          [189,17.5,0  ],
          [190,18.3,0  ],
          [191,17.3,0  ],
          [192,15.4,0  ],
          [193,16.8,0  ],
          [194,17.8,0  ],
          [195,18,3  ],
          [196,18.3,0  ],
          [197,15,1.8  ],
          [198,18.1,0  ],
          [199,18.5,0  ],
          [200,15.6,0  ],
          [201,17.8,0  ],
          [202,16.4,2.4  ],
          [203,17,0  ],
          [204,16.8,0  ],
          [205,17.8,0.2  ],
          [206,17.4,0  ],
          [207,17.4,0  ],
          [208,17.4,0  ],
          [209,15.8,0  ],
          [210,15.2,0  ],
          [211,16.4,0  ],
          [212,17,0  ],
          [213,17.1,0  ],
          [214,17,0  ],
          [215,17.2,0  ],
          [216,18.8,0  ],
          [217,21.6,0  ],
          [218,22.3,0  ],
          [219,17.2,0  ],
          [220,16.7,1.4  ],
          [221,15.7,0  ],
          [222,17.9,0  ],
          [223,17.2,0  ],
          [224,18.3,0  ],
          [225,20.6,0  ],
          [226,17.6,0  ],
          [227,18.4,0  ],
          [228,22.6,0  ],
          [229,22.1,0  ],
          [230,21.2,0  ],
          [231,16.2,0  ],
          [232,16.3,0  ],
          [233,17.7,0  ],
          [234,17.1,0  ],
          [235,17,0  ],
          [236,16.2,0  ],
          [237,15,0  ],
          [238,16.5,0  ],
          [239,14.2,0.2  ],
          [240,14.9,0  ],
          [241,16.5,0  ],
          [242,16,0  ],
          [243,15.1,0  ],
          [244,14.7,0  ],
          [245,14.9,0  ],
          [246,14.5,0  ],
          [247,15.2,0  ],
          [248,15.6,0  ],
          [249,15.6,0  ],
          [250,19.4,0  ],
          [251,18.6,0  ],
          [252,18.5,0  ],
          [253,14.2,0  ],
          [254,13.3,0.2  ],
          [255,10.9,0  ],
          [256,12.8,0  ],
          [257,14.9,0  ],
          [258,15.2,0  ],
          [259,14.9,0  ],
          [260,16.2,0  ],
          [261,17.3,0  ],
          [262,16.7,0  ],
          [263,14.6,0  ],
          [264,14.6,0  ],
          [265,11.5,0  ],
          [266,13.9,0  ],
          [267,13.3,0  ],
          [268,13.1,0  ],
          [269,12.9,0  ],
          [270,12.3,0  ],
          [271,14,0  ],
          [272,14.1,2  ],
          [273,12.2,0  ],
          [274,12.2,0  ],
          [275,15.3,0  ],
          [276,10.5,0  ],
          [277,11.1,0  ],
          [278,10.7,0  ],
          [279,11.5,0  ],
          [280,12.6,0  ],
          [281,13.7,0  ],
          [282,12.4,0  ],
          [283,9.3,0  ],
          [284,8.8,0  ],
          [285,10.4,0  ],
          [286,10.7,3.6  ],
          [287,13.6,12.8  ],
          [288,13.6,17.2  ],
          [289,10.2,10.1  ],
          [290,10.7,0  ],
          [291,8,0  ],
          [292,9.3,13.8  ],
          [293,10.7,0.4  ],
          [294,6.1,0  ],
          [295,6.3,0  ],
          [296,7.5,3.8  ],
          [297,7.7,4.4  ],
          [298,7.1,3.8  ],
          [299,7.9,0  ],
          [300,7.6,6.2  ],
          [301,8.7,16.2  ],
          [302,11.2,0.8  ],
          [303,12.3,15.5  ],
          [304,11.2,29.8  ],
          [305,10.5,14.8  ],
          [306,12.1,1.4  ],
          [307,11.6,3  ],
          [308,12.1,9  ],
          [309,12.8,7.4  ],
          [310,10.3,0  ],
          [311,9.1,5.3  ],
          [312,7.8,0.6  ],
          [313,6.9,0  ],
          [314,3.8,0  ],
          [315,2.1,0  ],
          [316,2.4,23.8  ],
          [317,7.3,13.6  ],
          [318,7.8,0.2  ],
          [319,5.9,0  ],
          [320,5.2,0  ],
          [321,6.2,0.2  ],
          [322,8.5,2.2  ],
          [323,7.5,8.2  ],
          [324,7,25.8  ],
          [325,7.9,7.8  ],
          [326,4.3,15  ],
          [327,4.3,0.4  ],
          [328,7.5,10.4  ],
          [329,4.7,0  ],
          [330,3.9,0  ],
          [331,3.1,0  ],
          [332,3.5,0  ],
          [333,3.4,6.4  ],
          [334,9.1,0.8  ],
          [335,9.5,14.6  ],
          [336,8.2,21  ],
          [337,8.6,10.4  ],
          [338,7.6,12.3  ],
          [339,9.6,9.6  ],
          [340,5.6,5.8  ],
          [341,4.1,2.2  ],
          [342,3.6,6.2  ],
          [343,3.5,0  ],
          [344,3.4,5.4  ],
          [345,3.6,0  ],
          [346,4.9,2.8  ],
          [347,5.3,0  ],
          [348,5.4,5.2  ],
          [349,3.4,3.4  ],
          [350,3.6,0.8  ],
          [351,4.1,22.2  ],
          [352,3.3,10.5  ],
          [353,2.5,0.4  ],
          [354,4.5,26.8  ],
          [355,3.7,7.4  ],
          [356,5.2,0.4  ],
          [357,4.4,3.8  ],
          [358,5,0.4  ],
          [359,3.9,4.5  ],
          [360,4.3,27.1  ],
          [361,5,7.8  ],
          [362,4.3,0.4  ],
          [363,5,2.2  ],
          [364,4.8,2.8  ],
          [365,2.7,0  ],
          [366,2.9,4.3  ]
          ]

drawingTestBatck = [
    drTest1
  , drTest2
  , drTest3
  , drTest4
  , drTest5
  , drTest6
  , drTest7
  , drTest7b
  , drTest8
  , drTest9
  , drTest10
  , drTest11
  , drTest12
  , drTest13

  , drTestRO1
  , drTestRO2
  , drTestRO3
  , drTestU1
  , drTestU2
  , drTestU3
  , drTestU4
  , drTestU5
  , drTestU6
  , drTestU7

  , drTestTransf1
  --
  , drTestStyles1
  --
  , drTestText1
  --
  , drTestMasking1
  , drTestEmbed
  , drTestEnvelope1
  --
  -- TODO etc
  ]
dvTestBatch = [
  test
  , test2
  , test3
  , test4
  , test5
  , test6
  , test7
  , test8a
  , test8b
  , test8c
  , test9
  , test10
  , test11
  , test12
  , test13

  -- Cant' reliably hash an *animation*
  -- , test14

  , test20
  , test21
  , test22
  , test23
  , test24
  , test25

  , test30
  , test31
  , test32
  ]

-- TODO separate these, see #126
drawingPlusDvTestBatch = drawingTestBatck <> dvTestBatch


--
--   print "Rendered all test plots"


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
      renderDrawingTestsToDir "Drawing+DV tests" "/tmp/lubeck/dv/test/report" drawingPlusDvTestBatch


    _ -> error "Bad arguments"

  return ()











test :: DrawingTest
test2 :: DrawingTest
test3 :: DrawingTest
test4 :: DrawingTest
test5 :: DrawingTest
test6 :: DrawingTest
test7 :: DrawingTest
test8a :: DrawingTest
test8b :: DrawingTest
test8c :: DrawingTest
test9 :: DrawingTest
test10 :: DrawingTest
test11 :: DrawingTest
test12 :: DrawingTest
test13 :: DrawingTest
test14 :: DrawingTest
test20 :: DrawingTest
test21 :: DrawingTest
test22 :: DrawingTest
test23 :: DrawingTest
test24 :: DrawingTest
test25 :: DrawingTest
test26 :: DrawingTest
test30 :: DrawingTest
test31 :: DrawingTest
test32 :: DrawingTest
test33 :: DrawingTest
