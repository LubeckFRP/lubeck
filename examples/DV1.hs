
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

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

import Data.VectorSpace
import Data.AffineSpace
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
    rendOpts  = defaultRenderingOptions
                { origoPlacement = BottomLeft
                , dimensions = Point 800 400
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

    , lineData [Point 0 0, Point 1 1]
    , lineData [Point 0 0, Point 0 0, Point 1 1]
    , lineData [Point 0.5 0.5, Point 1 1]
    , stepData (Point 0 0.5)
      [ Vector 0 0, Vector 0.2 0, Vector 0 0.3
      , Vector 0.5 0, Vector 0 (-0.2), Vector 0.3 0, Vector 0 0.2]

    , barData (take 10 rand1)
    , barData (take 3 rand2)

    , ratioData (rand1 !! 0)
    , ratioData (rand1 !! 1)
    , ratioData (rand1 !! 2)

    , ratioDataWithColor $ Point (rand1 !! 0) 0
    , ratioDataWithColor $ Point (rand1 !! 1) 0.2
    , ratioDataWithColor $ Point (rand1 !! 2) 0.7

    ]
  runAppReactive $ fmap (H.text "Please choose a graph:" <>) dS
  where
    plotStyle = id
      $ renderingRectangle  .~ Vector 800 400
      $ linePlotStrokeColor .~ (Colors.blue  `withOpacity` 0.5)
      $ barPlotBarColors    .~ cycle [Colors.purple `withOpacity` 0.5]
      $ mempty

    ps           = zipWith Point rand1 rand2

    -- combine [f, g...] x = mconcat [f x, g x...]
    combine fs x = mconcat $ fmap ($ x) fs

    headOnly xs = if null xs then [] else [head xs]
    lastOnly xs = if null xs then [] else [last xs]


randPoints, ordRandPoints :: [Point]
ordRandPoints = (Data.List.sortBy (Data.Ord.comparing x) $ take 10 randPoints)
randPoints    = zipWith Point rand1 rand2
rand1, rand2 :: [Double]
rand1 = randoms $ fst $ split randG
rand2 = randoms $ snd $ split randG

randG = (mkStdGen 8712261455)
-- randG = (mkStdGen 123456789)
-- randG = (mkStdGen 3141599999)
