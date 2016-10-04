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
  , shape <~ gender
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
    customDr :: Draft SVG
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
    customDr :: Draft SVG
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
    customDr :: Draft SVG
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
    customDr :: Draft SVG
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
    customDr :: Draft SVG
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



{-
TODO
Bar plot with highlights (vertical):
  - 2nd element hovered
  - 3rd and 5th element selected
-}
test27 = DrawingTest "test27" "" $ unpackStr
  $ drawingToSvgString mempty (barPlotOrientation .~ Vertical $ hoverSelectStates .~ hss $ mempty) $ drawPlot $
  plot (zip chars freq) [x <~ _1, y <~ _2 `withScale` linearWithOptions IntegerLR UseZero] bars
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
    hss = Data.IntMap.fromList [(pred 2,Hovering), (pred 3,Selected),(pred 5,Selected)]

{-
TODO
Bar plot with highlights (horizontal):
  - 2nd element hovered
  - 3rd and 5th element selected
-}
test28 = DrawingTest "test28" "" $ unpackStr
  $ drawingToSvgString mempty (barPlotOrientation .~ Horizontal $ hoverSelectStates .~ hss $ mempty) $ drawPlot $
  plot (zip chars freq) [x <~ _1, y <~ _2 `withScale` linearWithOptions IntegerLR UseZero] bars
  where
    chars :: [Char]
    freq :: [Int]
    chars = sortNub text
    freq = fmap (\c -> length $ filter (== c) text) chars
    text = filter Data.Char.isAlpha $ fmap Data.Char.toUpper $ [string|
      Foo bar baz.
      |]
    sortNub = Data.List.nub . Data.List.sort
    hss = Data.IntMap.fromList [(pred 2,Hovering), (pred 3,Selected),(pred 5,Selected)]




{-
  Pie/circular plot.
-}
test29a = DrawingTest "test29a" "" $ unpackStr
  $ drawingToSvgString mempty (noXY .~ Any True $ mempty) $ drawPlot $
  plot (zip chars freq) [x <~ _1, y <~ _2] circular
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
  Pie/circular plot.
-}
test29b = DrawingTest "test29b" "" $ unpackStr
  $ drawingToSvgString mempty (noXY .~ Any True $ mempty) $ drawPlot $
  plot dat [x <~ _1, y <~ _2 `withScale` linear' mempty] circular
  where
    -- No need for Y values to add up to anything
    dat = [(Male, 20), (Female, 60::Int)]
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

test34 = DrawingTest
  "test34"
  [string|
    Scatter plot with various colors, GG-style.
  |]
  $ unpackStr $ drawingToSvgString mempty mempty
  $ drawPlot $ plot persons [x <~ to height33,
                             y <~ to weight,
                             color <~ to is_male] pointG
  where
    persons =
        [ Person33 {height33 = 165.7360491771233, weight = 76.03799460579975, is_male = False, heart_attack = False}
        , Person33 {height33 = 175.12193932322913, weight = 65.25275937074434, is_male = False, heart_attack = False}
        , Person33 {height33 = 167.7922983463412, weight = 67.5555097845642, is_male = False, heart_attack = True}
        , Person33 {height33 = 207.27006559896898, weight = 103.98944174820292, is_male = True, heart_attack = True}
        , Person33 {height33 = 172.7052026387999, weight = 65.14306580066584, is_male = False, heart_attack = False}
        , Person33 {height33 = 175.28393694345976, weight = 83.71385106124151, is_male = False, heart_attack = False}
        , Person33 {height33 = 182.894317016853, weight = 74.80525732560123, is_male = True, heart_attack = False}
        , Person33 {height33 = 153.49345812200315, weight = 61.51298500649473, is_male = True, heart_attack = False}
        , Person33 {height33 = 143.77358608461873, weight = 43.97494290530609, is_male = False, heart_attack = False}
        , Person33 {height33 = 143.01580698995238, weight = 52.91724210008227, is_male = True, heart_attack = True}
        , Person33 {height33 = 169.9527441504168, weight = 67.42385983097182, is_male = False, heart_attack = False}
        , Person33 {height33 = 168.18366078277452, weight = 45.27022580745364, is_male = False, heart_attack = False}
        , Person33 {height33 = 174.13957282626728, weight = 45.84929758563176, is_male = False, heart_attack = False}
        , Person33 {height33 = 206.11505956041583, weight = 76.72414574085815, is_male = True, heart_attack = False}
        , Person33 {height33 = 151.82726364867673, weight = 69.40687007977549, is_male = False, heart_attack = False}
        , Person33 {height33 = 172.0123851019946, weight = 71.33650394073732, is_male = True, heart_attack = False}
        , Person33 {height33 = 189.21507200328045, weight = 72.89958051622676, is_male = True, heart_attack = True}
        , Person33 {height33 = 184.77110958562676, weight = 64.89459630555031, is_male = True, heart_attack = False}
        , Person33 {height33 = 199.01350781906189, weight = 71.72674160898778, is_male = True, heart_attack = False}
        , Person33 {height33 = 163.75707191336923, weight = 81.666482664923, is_male = True, heart_attack = False}
        , Person33 {height33 = 173.88257149211242, weight = 65.46667120205059, is_male = False, heart_attack = False}
        , Person33 {height33 = 182.5680008738076, weight = 55.284679398245196, is_male = True, heart_attack = True}
        , Person33 {height33 = 171.96214339495825, weight = 81.49072035455242, is_male = True, heart_attack = True}
        , Person33 {height33 = 170.43165032290557, weight = 76.96430321586323, is_male = True, heart_attack = True}
        , Person33 {height33 = 196.94388723587585, weight = 82.61400121306723, is_male = True, heart_attack = False}
        , Person33 {height33 = 143.41321064584798, weight = 69.36088022700234, is_male = False, heart_attack = False}
        , Person33 {height33 = 154.63867930841468, weight = 58.34811447430104, is_male = False, heart_attack = True}
        , Person33 {height33 = 190.97131160977506, weight = 61.52556015970118, is_male = True, heart_attack = False}
        , Person33 {height33 = 180.53267762627934, weight = 76.52010849484034, is_male = False, heart_attack = False}
        , Person33 {height33 = 208.89983685523458, weight = 72.60304408690126, is_male = True, heart_attack = True}
        , Person33 {height33 = 136.75601295831578, weight = 51.09542205184846, is_male = False, heart_attack = False}
        , Person33 {height33 = 179.98472036130877, weight = 54.74798743766594, is_male = False, heart_attack = True}
        , Person33 {height33 = 214.46567802416826, weight = 86.85193172799728, is_male = True, heart_attack = True}
        , Person33 {height33 = 131.95913283620402, weight = 49.88661920169436, is_male = False, heart_attack = False}
        , Person33 {height33 = 169.92809718718337, weight = 66.60618647551475, is_male = False, heart_attack = False}
        , Person33 {height33 = 195.87848540631023, weight = 75.52644021142304, is_male = True, heart_attack = False}
        , Person33 {height33 = 175.38693731767734, weight = 69.80460199892678, is_male = False, heart_attack = False}
        , Person33 {height33 = 192.88051291105833, weight = 80.49399916337688, is_male = True, heart_attack = False}
        , Person33 {height33 = 145.48409755913838, weight = 39.57766825848976, is_male = False, heart_attack = False}
        , Person33 {height33 = 181.9614002944488, weight = 76.22363513792163, is_male = True, heart_attack = False}
        , Person33 {height33 = 159.8764263559022, weight = 68.65001135360272, is_male = True, heart_attack = False}
        , Person33 {height33 = 206.21647427857067, weight = 81.51927873902375, is_male = False, heart_attack = True}
        , Person33 {height33 = 151.48341434326196, weight = 76.26485838805596, is_male = False, heart_attack = False}
        , Person33 {height33 = 147.3011984188401, weight = 51.833082792649805, is_male = False, heart_attack = False}
        , Person33 {height33 = 153.87643066940038, weight = 61.91454510318069, is_male = False, heart_attack = True}
        , Person33 {height33 = 175.71618868380574, weight = 70.44070554104657, is_male = False, heart_attack = True}
        , Person33 {height33 = 188.70346257993054, weight = 74.0383256289079, is_male = True, heart_attack = True}
        , Person33 {height33 = 179.78702726884129, weight = 69.17839718808246, is_male = True, heart_attack = True}
        , Person33 {height33 = 189.84341440906817, weight = 68.45823389669266, is_male = True, heart_attack = False}
        ]


{-
  Tick text at 45 degrees counter-clockwise (X and Y).
-}
test35 = DrawingTest "test35" "" $ unpackStr
  $ drawingToSvgString mempty st $ drawPlot $
  plot (zip
    ([1..10] :: [Int])
    ([0,1,5,5,4,3,4,4,4,3] :: [Int])
    ) [x<~_1,y<~_2]
    line
  where
    st = tickTextTurn .~ (D.turn*1/8, D.turn*1/8) $ mempty

data Person33 = Person33 { height33 :: Double, weight :: Double, is_male :: Bool, heart_attack :: Bool }

drTest1 = DrawingTest
  "drTest1"
  [string|
    A centered red circle, radius 50.
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ D.scale 50
  $ D.fillColor Colors.red
  $ D.circle

drTest2 = DrawingTest
  "drTest2"
  [string|
    A centered blue circle, radius 50.
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ D.scale 50
  $ D.fillColor Colors.blue
  $ D.circle

drTest3 = DrawingTest
  "drTest3"
  [string|
    A red circle (radius 50) on top of a blue square (side length 50), both centered.
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
    A semi-transparent triangle (fillColorA).
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ D.scale 50
  $ D.fillColorA (Colors.blue `withOpacity` 0.5)
  $ D.triangle

drTest5b = DrawingTest
  "drTest5b"
  [string|
    A path looking like two triangles connecte at the origin.
    Semi-transparent blue fill, red border (default line width).
    Below it a slightly larger square.
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ (<> D.xyAxis)
  $ D.scale 50
  $ (<> (D.fillColor (Colors.lightgrey) $ D.scale 2.1 $ D.square))
  $ D.strokeColor Colors.red
  $ D.fillColorA (Colors.blue `withOpacity` 0.5)
  $ D.polygon $ D.betweenPoints $
    [ P (V2 0      0)
    , P (V2 1      1)
    , P (V2 1    (-1))
    , P (V2 (-1)   1)
    , P (V2 (-1) (-1))
    ]

drTest6 = DrawingTest
  "drTest6"
  [string|
    A square moved to the right over a co-ordinate system.
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
    A stroked vertical line with stroke with 5, height 100, moved 10 to the right,
    over a co-ordinate system.
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
    Exactly the same as drTest7 (transformations distribute over styles and vice-versa).
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
    A stroked horizonal line with stroke with 5, width 100, moved 10 upwards,
    over a co-ordinate system.
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
    Text naming the four quadrants (text, translation and orientation) over a co-ordinate system.
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
    Five squares of various size and position (all in the 1st quadrant).
    Stroked green with stroke width 1, not filled (default stroke and fill styles, no extranou fills).
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
    Six small squares filled green, not stroked, translated 100 to the right and then rotated
    variously (colors, transformations, rotation direction).
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
    Five squares translated and rotated, distributed evenly around local origin.
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
    Two squares on top, one fit inside a rectangle.
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

-- All transformations
drTestTransf1 = DrawingTest
  "drTestTransf1"
  [string|
    Translation.
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ D.translate (V2 50 (-50)) sq1 <> sq2
    where
      sq1 = D.fillColor Colors.red  $ D.scale 150 $ D.square
      sq2 = D.fillColor Colors.blue $ D.scale 150 $ D.square
drTestTransf2 = DrawingTest
  "drTestTransf2"
  [string|
    Scaling.
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ D.scaleX 1.2 sq1 <> D.scaleY 1.2 sq2
    where
      sq1 = D.fillColor Colors.red  $ D.scale 150 $ D.square
      sq2 = D.fillColor Colors.blue $ D.scale 150 $ D.square
drTestTransf3 = DrawingTest
  "drTestTransf3"
  [string|
    Rotation.
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ D.rotate (D.turn/3) sq1 <> sq2
    where
      sq1 = D.fillColor Colors.red  $ D.scale 150 $ D.square
      sq2 = D.fillColor Colors.blue $ D.scale 150 $ D.square
drTestTransf4 = DrawingTest
  "drTestTransf4"
  [string|
    Shearing.
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ D.shearX 1.1 sq1 <> sq2
    where
      sq1 = D.fillColor Colors.red  $ D.scale 150 $ D.square
      sq2 = D.fillColor Colors.blue $ D.scale 150 $ D.square
drTestTransf5 = DrawingTest
  "drTestTransf5"
  [string|
    Linear/affine combo.
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ D.scaleX (-1) (D.translate 50 sq1) <> sq2
    where
      sq1 = D.fillColor Colors.red  $ D.scale 150 $ D.square
      sq2 = D.fillColor Colors.blue $ D.scale 150 $ D.square

-- Basic styles (which?)
-- TODO abstraction leaks through styleNamed
-- Let's just do the exported ones for now (dashing/strokeWidth/strokeColor/fillColor)

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
    Embedding an SVG in a drawing.
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ em
  where
    em = maybe (error "Testing SVG embeds: Can't parse SVG string")id  $ D.addEmbeddedSVGFromStr $ toStr [string|
        <svg
           xmlns:dc="http://purl.org/dc/elements/1.1/"
           xmlns:cc="http://web.resource.org/cc/"
           xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
           xmlns:svg="http://www.w3.org/2000/svg"
           xmlns="http://www.w3.org/2000/svg"
           xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"
           xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"
           width="600"
           height="600"
           id="svg2"
           sodipodi:version="0.32"
           inkscape:version="0.45.1"
           sodipodi:docname="Example.svg"
           inkscape:output_extension="org.inkscape.output.svg.inkscape"
           sodipodi:docbase="/home/gmaxwell"
           version="1.0">
          <metadata
             id="metadata9">
            <rdf:RDF>
              <cc:Work
                 rdf:about="">
                <dc:format>image/svg+xml</dc:format>
                <dc:type
                   rdf:resource="http://purl.org/dc/dcmitype/StillImage" />
              </cc:Work>
            </rdf:RDF>
          </metadata>
          <sodipodi:namedview
             inkscape:window-height="620"
             inkscape:window-width="814"
             inkscape:pageshadow="2"
             inkscape:pageopacity="0.0"
             guidetolerance="10.0"
             gridtolerance="10.0"
             objecttolerance="10.0"
             borderopacity="1.0"
             bordercolor="#666666"
             pagecolor="#ffffff"
             id="base"
             width="600px"
             height="600px"
             inkscape:zoom="0.35974058"
             inkscape:cx="50"
             inkscape:cy="519.04966"
             inkscape:window-x="483"
             inkscape:window-y="101"
             inkscape:current-layer="svg2" />
          <defs
             id="defs16" />
          <g
             id="g2161"
             transform="matrix(6.3951354,0,0,6.3951354,-22.626246,-7.1082509)">
            <path
               nodetypes="ccccccccccccccccccccccccccccccccccccccc"
               id="flowRoot1882"
               d="M 36.009766,9.2505083 C 37.739295,9.4211273 38.305879,11.470697 38.052581,12.935049 C 37.346266,16.153899 36.316821,19.51466 35.445405,22.717701 C 36.091666,24.812224 31.712284,24.008877 33.219932,22.315459 C 34.817041,18.411202 36.011404,13.498336 36.009766,9.2505083 z M 36.009766,2.9926958 C 38.210311,1.2242088 40.996268,9.172757 33.911571,6.1534847 C 33.884619,5.7603019 36.096289,3.3869447 36.009766,2.9926958 z M 41.371094,15.871601 C 41.371094,13.66457 41.371094,11.457539 41.371094,9.250508 C 43.180621,9.4257387 43.963014,11.704559 43.286137,13.215517 C 42.859084,15.059792 42.939241,17.3996 44.601487,18.625335 C 46.710544,19.683477 49.38774,17.353112 48.803268,15.118437 C 48.93196,13.406538 48.236292,11.613848 48.968862,9.9690415 C 51.055097,9.6500357 51.500677,12.487155 50.544985,13.844747 C 50.070023,15.309708 50.857452,16.781898 50.672344,18.239432 C 50.279615,19.94056 48.418404,20.00023 47.0225,20.071868 C 45.478489,20.38194 43.516835,20.791368 42.361947,19.38874 C 41.522514,18.444089 41.211274,17.107671 41.371094,15.871601 z M 61.224609,9.5727739 C 60.41978,11.557552 58.100804,10.235616 56.62767,10.571551 C 53.836862,14.393611 60.920038,13.513667 61.8085,17.011648 C 61.85613,18.933747 60.028359,20.587389 58.129091,20.443312 C 56.904487,20.607229 54.609204,20.982393 54.417879,19.267622 C 55.280609,17.508269 57.336359,19.528803 58.633111,18.8463 C 60.403141,17.99081 59.402232,15.555325 57.728781,15.321475 C 56.550115,14.98135 55.091813,15.225439 54.254747,14.112764 C 53.017669,12.881167 53.392132,10.733148 54.736719,9.7413252 C 56.619172,8.3307396 59.170326,8.9535067 61.224609,9.5727739 z M 66.458984,6.1450396 C 65.368126,7.6333334 67.348936,9.9531574 68.987229,9.0948979 C 69.978133,11.042503 66.524641,10.777931 66.473495,12.430992 C 64.443605,16.101814 68.48273,18.623426 67.571657,20.417528 C 65.440858,20.26155 64.324307,17.844452 64.577433,15.919357 C 64.70847,14.408586 65.055107,12.79361 64.322961,11.373941 C 63.786422,9.5475192 64.150419,7.1452655 65.954233,6.1552477 L 66.206043,6.1203323 L 66.458984,6.1450396 L 66.458984,6.1450396 z " />
            <path
               nodetypes="ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
               id="flowRoot1890"
               d="M 10.867188,44.877953 C 6.2812618,42.124849 5.2205914,52.366268 10.409215,49.892431 C 12.389385,49.295568 14.988045,43.912658 10.867188,44.877953 z M 15.167969,43.987328 C 14.919826,46.33724 16.617756,52.554202 12.298734,50.536918 C 9.8041142,52.312916 6.0580855,52.958674 4.5023123,49.583386 C 2.6350454,45.257322 7.3033103,42.298712 11.046443,43.361059 C 15.247185,41.320786 9.4930286,38.338264 7.1068792,40.322138 C 3.4374421,40.01388 7.406407,37.201407 9.3495087,37.962912 C 12.44212,37.877788 15.556534,40.380131 15.171751,43.648912 L 15.169638,43.83797 L 15.167969,43.987328 z M 30.53125,43.553734 C 29.638794,45.911558 32.49467,50.463872 28.779999,51.070944 C 26.888088,47.702306 30.931621,41.190257 25.58365,40.046147 C 20.73931,40.312798 21.252194,45.910871 22.001439,49.154066 C 21.84253,51.828309 18.790577,51.39256 19.585585,48.673738 C 19.851829,45.693864 18.285332,39.630301 20.986983,38.702911 C 23.508461,40.80889 25.761847,35.731906 28.452459,38.686226 C 29.921454,39.793194 30.827618,41.709992 30.53125,43.553734 z M 38.807,49.770223 C 42.369034,50.768974 44.523344,46.328688 43.700521,43.358983 C 40.402775,35.546453 32.491199,44.344131 38.807,49.770223 z M 39.941406,38.034203 C 52.085872,39.705642 43.204854,59.098342 34.688722,48.642968 C 32.591886,44.778031 34.383109,38.440132 39.291369,38.051827 L 39.941406,38.034203 L 39.941406,38.034203 z M 51.660156,34.624046 C 49.815978,37.850583 54.789459,38.666222 55.83437,39.23566 C 54.140746,40.715733 50.093061,40.12158 51.562461,43.76212 C 51.004096,46.980523 52.486847,50.037723 55.670614,50.54595 C 53.547788,53.782616 48.41793,50.035495 49.349973,46.519692 C 50.339877,43.686471 48.78131,40.671166 48.467256,38.48357 C 51.099926,37.413599 47.886512,33.32283 51.660156,34.624046 z M 69.859375,43.553734 C 68.966918,45.911557 71.822794,50.463872 68.108124,51.070944 C 66.216214,47.702306 70.259746,41.190256 64.911775,40.046145 C 60.222418,40.285904 60.439194,45.757728 61.367942,48.953683 C 60.705448,53.064855 57.788626,49.900134 58.838379,47.289738 C 58.969709,43.381174 59.006437,39.455087 58.607404,35.565714 C 59.356025,31.632413 62.368269,34.68013 61.01352,37.194316 C 60.38417,39.302538 61.469087,40.653476 62.996248,38.474829 C 66.202089,36.826154 70.863269,39.826451 69.859375,43.553734 z M 85.410156,44.374046 C 83.244849,47.905533 76.447085,42.456344 75.976013,47.444052 C 76.913541,51.724548 83.275324,48.726196 84.393639,50.133773 C 82.109855,53.525123 76.421339,51.860111 74.285335,49.01336 C 71.258247,44.729984 74.614013,37.166516 80.254289,37.96756 C 83.286958,38.284495 85.833914,41.310745 85.410156,44.374046 z M 83.253906,43.741234 C 84.431319,39.039614 74.594812,38.687325 76.291886,43.335226 C 78.284783,44.796048 81.032856,43.090943 83.253906,43.741234 z M 96.554688,40.366234 C 93.290612,38.6882 90.622217,42.519635 90.728522,45.492665 C 90.881925,47.333676 92.330286,52.144465 89.028751,50.905988 C 88.95673,46.763963 88.353312,42.447207 89.31721,38.336643 C 91.040471,38.503437 92.207514,40.668181 93.421468,38.208298 C 94.902478,37.44973 97.690944,38.263668 96.554688,40.366234 z " />
            <path
               style="fill:#ff0000"
               nodetypes="ccccccccccccccccccccccccccccccccccccccccccccccccccccc"
               id="flowRoot1898"
               d="M 17.026327,63.789847 C 0.7506376,64.058469 13.88279,66.387154 13.113883,69.323258 C 8.0472417,70.287093 3.5936285,63.565714 6.8090451,59.370548 C 8.7591553,55.717791 15.269922,55.198361 16.902068,59.393261 C 17.532581,60.758947 17.628237,62.396589 17.026327,63.789847 z M 15.306463,62.656109 C 18.852566,58.713773 7.6543584,56.609143 10.765803,61.304742 C 12.124789,62.217715 13.961359,61.705342 15.306463,62.656109 z M 31.307931,62.391383 C 27.130518,63.524026 24.669863,68.663004 27.470717,72.229472 C 25.946657,74.052316 24.253697,71.076237 24.857281,69.636909 C 23.737444,67.038428 17.399862,72.254246 19.386636,68.888657 C 23.159719,67.551193 22.398496,63.711301 22.06067,60.848671 C 24.064085,60.375294 24.370376,65.772689 27.167918,63.326048 C 28.350126,62.546369 29.927362,61.067531 31.307931,62.391383 z M 37.66875,70.598623 C 33.467314,66.62264 32.517064,77.972723 37.30626,74.466636 C 38.742523,73.853608 40.55904,70.38932 37.66875,70.598623 z M 41.677321,70.973131 C 42.340669,75.308182 36.926157,78.361257 33.331921,76.223155 C 29.43435,74.893988 30.618698,67.677232 35.003806,68.567885 C 37.137393,70.592854 42.140265,67.002221 37.656192,66.290007 C 35.242233,65.914214 35.166503,62.640757 38.036954,63.926404 C 40.847923,64.744926 43.227838,68.124735 41.677321,70.973131 z M 62.379099,76.647079 C 62.007404,78.560417 61.161437,84.034535 58.890565,82.010019 C 59.769679,79.039958 62.536382,72.229115 56.947899,72.765789 C 53.790416,73.570863 54.908257,80.968388 51.529286,79.496859 C 51.707831,76.559817 55.858125,71.896837 50.8321,70.678504 C 45.898113,69.907818 47.485944,75.735824 45.286883,78.034703 C 42.916393,76.333396 45.470823,71.647155 46.624124,69.414735 C 50.919507,67.906486 63.618534,70.878704 62.379099,76.647079 z M 66.426447,83.84905 C 67.616398,85.777591 62.114624,94.492698 62.351124,90.31711 C 63.791684,86.581961 65.730376,78.000636 67.391891,74.85575 C 71.027815,73.781175 76.383067,75.350289 76.591972,79.751898 C 77.048545,83.793048 73.066803,88.429945 68.842187,86.765936 C 67.624386,86.282034 66.56741,85.195132 66.426447,83.84905 z M 74.086569,81.803435 C 76.851893,78.050524 69.264402,74.310256 67.560734,78.378191 C 65.893402,80.594099 67.255719,83.775746 69.700555,84.718558 C 72.028708,85.902224 73.688639,83.888662 74.086569,81.803435 z M 82.318799,73.124577 C 84.30523,75.487059 81.655015,88.448086 78.247183,87.275736 C 78.991935,82.387828 81.291029,77.949394 82.318799,73.124577 z M 95.001985,87.684695 C 78.726298,87.953319 91.858449,90.281999 91.089542,93.218107 C 86.0229,94.18194 81.569287,87.460562 84.784701,83.265394 C 86.734814,79.612637 93.245582,79.09321 94.877729,83.28811 C 95.508245,84.653796 95.603892,86.291438 95.001985,87.684695 z M 93.282122,86.550957 C 96.828223,82.608621 85.630017,80.503993 88.741461,85.199592 C 90.100447,86.112565 91.937018,85.600192 93.282122,86.550957 z " />
          </g>
        </svg>
      |]


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
    A circle above a square.
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ D.scale 70 D.circle D.=== D.scale 80 D.square

drTestEnvelope2 = DrawingTest
  "drTestEnvelope2"
  [string|
    A circle to the left of a square, and to the left and above of them a triangle.
  |]
  $ unpackStr $ drawingToSvgStringUnstyled mempty
  $ D.scale 110 D.triangle D./// (D.scale 70 D.circle D.||| D.scale 80 D.square)


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
  , drTest5b
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
  , drTestTransf2
  , drTestTransf3
  , drTestTransf4
  , drTestTransf5
  --
  , drTestStyles1
  , drTestStyles2
  , drTestStyles3
  , drTestStyles4
  , drTestStyles5
  , drTestStyles6
  , drTestStyles7
  --
  , drTestText1
  , drTestText2
  , drTestText3
  , drTestText4
  , drTestText5
  --
  , drTestMasking1
  , drTestMasking2
  , drTestEmbed
  , drTestEnvelope1
  , drTestEnvelope2
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
  , test26

  , test27
  , test28
  , test29a
  , test29b


  , test30
  , test31
  , test32

  , test34
  , test35
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
