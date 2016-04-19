
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import Data.Monoid ((<>))

import GHCJS.Types(JSString, jsval)
import qualified Web.VirtualDom as VD
import qualified Web.VirtualDom.Html as H
import qualified Web.VirtualDom.Html.Attributes as H
import qualified Web.VirtualDom.Html.Events as H
import qualified Web.VirtualDom.Svg.Events as SvgEv
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
import Lubeck.DV.ColorPalette
import Lubeck.DV.Styling
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

main :: IO ()
main = do
  dS <- chooseDrawing $ fmap ((`getStyled` plotStyle) . (<> plotRectangle))
    -- All based on simpleLinePlot (auto-scaling and axis)
    [ mempty
    -- , pure testSimple1
    -- , pure testSimple2
    -- , pure testSimple3

    -- Line, scatter and combinations
    , lineData     ordRandPoints
    , scatterData  ordRandPoints
    , scatterDataX ordRandPoints
    , scatterDataY ordRandPoints

    , combine [scatterDataX, scatterData]  ordRandPoints
    , combine [scatterDataY, scatterData]  ordRandPoints
    , combine [scatterDataX, scatterDataY] ordRandPoints

    -- TODO overlay multiple line graphs etc w local style
    , combine [lineData, scatterData]      ordRandPoints
    , mconcat [lineData ordRandPoints, scatterData (lastOnly ordRandPoints)]
    , mconcat [lineData ordRandPoints, scatterData (headOnly ordRandPoints)]


    , mconcat [linearData 1 0, scatterData ordRandPoints]
    , mconcat [linearData (-1) 0.5, scatterData ordRandPoints]
    , mconcat [linearData (-1) 1, scatterData ordRandPoints]

    , lineData [_p 0 0, _p 1 1]
    , lineData [_p 0 0, _p 0 0, _p 1 1]
    , lineData [_p 0.5 0.5, _p 1 1]
    , stepData (_p 0 0.5)
      [ V2 0 0, V2 0.2 0, V2 0 0.3
      , V2 0.5 0, V2 0 (-0.2), V2 0.3 0, V2 0 0.2]

    , barData (fmap realToFrac $ take 10 rand1)
    , barData (fmap realToFrac $ take 3 rand2)
    , barData $ fmap realToFrac [1,1.1..1]

    , ratioData (realToFrac $ rand1 !! 0)
    , ratioData (realToFrac $ rand1 !! 1)
    , ratioData (realToFrac $ rand1 !! 2)

    , ratioDataWithColor $ _p (rand1 !! 0) 0
    , ratioDataWithColor $ _p (rand1 !! 1) 0.2
    , ratioDataWithColor $ _p (rand1 !! 2) 0.7

    ]
  runAppReactive $ fmap (H.text "Please choose a graph:" <>) dS
  where
    plotStyle = id
      $ renderingRectangle  .~ V2 500 250
      $ linePlotStrokeColor .~ singleColour (Colors.blue  `withOpacity` 0.5)
      $ barPlotBarColors    .~ singleColour (Colors.purple `withOpacity` 0.5)
      $ mempty

    ps           = zipWith _p rand1 rand2

    -- combine [f, g...] x = mconcat [f x, g x...]
    combine fs x = mconcat $ fmap ($ x) fs

    headOnly xs = if null xs then [] else [head xs]
    lastOnly xs = if null xs then [] else [last xs]
    _p x y = P (V2 x y)


-- Some random series for testing

randPoints, ordRandPoints :: [P2 Double]
ordRandPoints = (Data.List.sortBy (Data.Ord.comparing (view _x)) $ take 10 randPoints)
randPoints    = zipWith (\x y -> P (V2 x y)) rand1 rand2

rand1, rand2 :: [Double]
rand1 = randoms $ fst $ split randG
rand2 = randoms $ snd $ split randG

randG = (mkStdGen 8712261455)
-- randG = (mkStdGen 123456789)
-- randG = (mkStdGen 3141599999)
