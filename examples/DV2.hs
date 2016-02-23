
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import Data.Monoid ((<>))

import GHCJS.Types(JSString, jsval)
import qualified Web.VirtualDom.Html as H
import qualified Data.JSString

import qualified Data.List
import qualified Data.Ord
import System.Random (mkStdGen, randoms, split)

import Control.Lens (view)
import Control.Lens.Operators


-- TODO move
import Data.Time (UTCTime(..), DiffTime, Day(..))


-- TODO Debug
import Control.Concurrent(forkIO, threadDelay)
import Control.Monad(forever)

import Lubeck.FRP
import Lubeck.DV.Drawing
import Lubeck.DV.SimpleNormalized
import Lubeck.App (Html, runAppReactive)
import Lubeck.Forms
import Lubeck.Forms.Select
  -- (Widget, Widget', component, bothWidget)
import Lubeck.Forms.Basic
import Lubeck.Drawing
import Lubeck.Util(showJS, parseDateAndTimeToUTC)

import Linear.Vector
import Linear.Affine
import Linear.V0
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4

import qualified Lubeck.Drawing

import Data.Colour (withOpacity)
import qualified Data.Colour.Names as Colors


-- MAIN


chooseDrawing :: [Drawing] -> IO (Signal Html)
chooseDrawing ds = do
  (view, intE) <- componentEvent 0 (rangeWidget 0 (length ds - 1) 1) mempty
  drawingS <- stepperS mempty (fmap (ds !!) intE)
  return $ mconcat [view, (fmap (toSvg rendOpts) drawingS)]
  where
    rendOpts  = mempty
                { originPlacement = Center
                , dimensions      = P $ V2 1000 500
                }
    -- backgroundGrid = scale 600 xyCoords

hcat :: [Drawing] -> Drawing
hcat = foldt (|||) mempty

vcat :: [Drawing] -> Drawing
vcat = foldt (===) mempty

-- TODO does this make it faster?
-- Not really...
foldt            :: (a -> a -> a) -> a -> [a] -> a
foldt f z []     = z
foldt f z [x]    = x
foldt f z xs     = foldt f z (pairs f xs)
  where
    pairs f (x:y:t)  = f x y : pairs f t
    pairs f t        = t

-- TODO align (V2 a -> a -> enveloped -> enveloped)
{-
Then derive:

align BL  = posDiagonal 0
align TR  = posDiagonal 1
align TL  = negDiagonal 0
align BR  = negDiagonal 0
align L   = unitX 0
align R   = unitX 1
align T   = unitY 1
align B   = unitY 0
-}

main :: IO ()
main = do
  dS <- chooseDrawing $ fmap (scale 10 . (<> scale 10 xyCoords)) $
    [ (translateX 2 blueRect ||| blueCircle)

    , scale 0.5 legend
    , scale 0.1 legend

    , showEnvelope unitX $ showEnvelope unitY $ redRect
    , showEnvelope unitX $ showEnvelope unitY $ blueRect
    , showEnvelope unitX $ showEnvelope unitY $ scale 1.2 blueRect
    , showEnvelope unitX $ showEnvelope unitY $ shear 0.8 1 blueRect
    , showEnvelope unitX $ showEnvelope unitY $ shear 1 0.8 blueRect

    , showEnvelope unitX $ showEnvelope unitY $ shear 0.2 1 redCircle
    , showEnvelope unitX $ showEnvelope unitY $ shear 1 0.2 redCircle
    , showEnvelope unitX $ showEnvelope unitY $ translateX 0.2 $ shear 0.2 1 redCircle
    , showEnvelope unitX $ showEnvelope unitY $ translateX 0.2 $ translateY 1 $ shear 1 0.2 redCircle

    , showEnvelope unitX   $ scale 1 blueCircle
    , showEnvelope unitX   $ scale 2 blueCircle
    , showEnvelope unitX   $ translateX 4 $ scale 1 blueCircle
    , showEnvelope unitX   $ translateX (-5) $ scale 1 blueCircle
    , showEnvelope unitX   $ translateX (-4.5) $ scale 1 blueCircle
    , showEnvelope unitX   $ rotate (4*turn/13) $ translateX 5 $ scale 3 blueCircle

    , showEnvelope unitX $ showEnvelope unitY $ scale 10 $ strokeWidth 1 $ strokeColor Colors.blue $ segments (take 5 randVectors)
    , showEnvelope unitX $ showEnvelope unitY $ scale 10 $ strokeWidth 1 $ strokeColor Colors.blue $ segments (drop 2 $ take 3 randVectors)
    , showEnvelope unitX $ showEnvelope unitY $ scale 10 $ strokeWidth 1 $ strokeColor Colors.blue $ segments (drop 20 $ take 15 randVectors)

    , showEnvelope (V2 1 0.3)   $ scale 1 blueCircle
    , showEnvelope (V2 1 0.3)   $ scale 2 blueCircle
    , showEnvelope (V2 1 0.3)   $ translateX 4 $ scale 1 blueCircle

    , showEnvelope (V2 1 0.3)   $ shear 0.2 0.1 $ scale 1 blueCircle
    , showEnvelope (V2 1 0.3)   $ shear 0.2 0.1 $ scale 2 blueCircle
    , showEnvelope (V2 1 0.3)   $ shear 0.2 0.1 $ translateX 4 $ scale 1 blueCircle

    , showEnvelope (negated $ V2 1 0.3)   $ shear 0.2 0.1 $ scale 1 blueCircle
    , showEnvelope (negated $ V2 1 0.3)   $ shear 0.2 0.1 $ scale 2 blueCircle
    , showEnvelope (negated $ V2 1 0.3)   $ shear 0.2 0.1 $ translateX 4 $ scale 1 blueCircle

    , showDirection (dir $ V2 1    0)
    , showDirection (dir $ V2 0    1)
    , showDirection (dir $ V2 (-1) 0)
    , showDirection (dir $ V2 0    (-1))

    , showDirection2 (dir $ V2 1    0)
    , showDirection2 (dir $ V2 0    1)
    , showDirection2 (dir $ V2 (-1) 0)
    , showDirection2 (dir $ V2 0    (-1))

    , showUnitX
    , rotate (turn*1/3) $ showUnitX
    , rotate (turn*3/4) $ showUnitX
    , (redCircle ||| blueRect ||| blueCircle)
    , (redCircle ||| fillH ||| blueRect ||| fillH ||| blueCircle)
    , (redCircle ||| fillV ||| blueRect ||| fillV ||| blueCircle)
    , (redCircle === blueCircle)

    , (scale 3 redCircle ||| blueCircle)
    , (redCircle ||| scale 2 redRect ||| scale 0.5 blueCircle)
    , (redCircle === scale 2 blueCircle)

    , (redCircle === (scale 2 blueCircle ||| greenCircle))
    , (redCircle <> juxtapose (V2 0.5 0.7) redCircle blueCircle) === hcat (replicate 300 $ rotate (turn/12) $ scaleX 0.1 $ scale 0.1 greenCircle)
    , (redCircle === (scale 2 blueCircle ||| greenCircle))
    , (redCircle <> juxtapose (V2 0.5 0.7) redCircle blueCircle) === mconcat (replicate 300 $ rotate (turn/12) $ scaleX 0.1 $ scale 0.1 greenCircle)
    ]
  runAppReactive $ fmap (H.text "Please choose a graph:" <>) dS
  where
    legend = vcat
      [ redRect  ||| scale 0.3 (textLeftMiddle "Red")
      , blueRect ||| scale 0.3 (textLeftMiddle "Blue")
      , scale 0.3 (textMiddleMiddle "Red")  ||| redRect
      , scale 0.3 (textMiddleMiddle "Blue") ||| blueRect
      , scale 0.3 (textRightMiddle "Red")   ||| redRect
      , scale 0.3 (textRightMiddle "Blue")  ||| blueRect

      , redRect  ||| scale 0.3 (textLeftMiddle "RedRedRedRedRed")
      , blueRect ||| scale 0.3 (textLeftMiddle "BlueBlueBlueBlue")
      , scale 0.3 (textMiddleMiddle "RedRedRedRedRed")  ||| redRect
      , scale 0.3 (textMiddleMiddle "BlueBlueBlueBlue") ||| blueRect
      , scale 0.3 (textRightMiddle "RedRedRedRedRed")   ||| redRect
      , scale 0.3 (textRightMiddle "BlueBlueBlueBlue")  ||| blueRect
      ]

    fillH = scale 10 horizontalLine
    fillV = scale 10 verticalLine

    redCircle   = scale 10 $ fillColorA (Colors.red `withOpacity` 0.4) circle
    blueCircle  = scale 10 $ fillColorA (Colors.blue `withOpacity` 0.4) circle
    greenCircle = scale 10 $ fillColorA (Colors.green `withOpacity` 0.4) circle

    redRect   = scale 10 $ fillColorA (Colors.red `withOpacity` 0.4) square
    greenRect = scale 10 $ fillColorA (Colors.green `withOpacity` 0.4) square
    blueRect  = scale 10 $ fillColorA (Colors.blue `withOpacity` 0.4) square
    redRectX  = scale 10 $ scaleX 1.2 $ rotate (turn/13) $ redRect

    plotStyle = id
      $ renderingRectangle  .~ V2 500 550
      $ linePlotStrokeColor .~ (Colors.blue  `withOpacity` 0.5)
      $ barPlotBarColors    .~ cycle [Colors.purple `withOpacity` 0.5]
      $ mempty

    ps           = zipWith _p rand1 rand2

    -- combine [f, g...] x = mconcat [f x, g x...]
    combine fs x = mconcat $ fmap ($ x) fs

    headOnly xs = if null xs then [] else [head xs]
    lastOnly xs = if null xs then [] else [last xs]
    _p x y = P (V2 x y)

    rotateVector a = transformVector (rotation a)


-- Some random series for testing

randPoints, ordRandPoints :: [P2 Double]
ordRandPoints = (Data.List.sortBy (Data.Ord.comparing (view _x)) $ take 10 randPoints)
randPoints    = zipWith (\x y -> P (V2 x y)) rand1 rand2
randVectors   = zipWith (\x y -> (V2 x y)) rand1 rand2

rand1, rand2 :: [Double]
rand1 = randoms $ fst $ split randG
rand2 = randoms $ snd $ split randG

randG = (mkStdGen 8712261455)
-- randG = (mkStdGen 123456789)
-- randG = (mkStdGen 3141599999)
