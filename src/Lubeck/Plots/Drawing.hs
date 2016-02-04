
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Lubeck.Plots.Drawing
    ( scatterData
    , lineData
    , boxData
    , ticks
    , labeledAxis
    ) where

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

-- TODO move
import Data.Time (UTCTime(..), DiffTime, Day(..))


-- TODO Debug
import Control.Concurrent(forkIO, threadDelay)
import Control.Monad(forever)

import Lubeck.FRP
import Lubeck.App (Html, runAppReactive)
import Lubeck.Forms
  -- (Widget, Widget', component, bothWidget)
import Lubeck.Forms.Basic
import Lubeck.Drawing
import Lubeck.Util(showJS)

import Data.VectorSpace
import Data.AffineSpace
import qualified Lubeck.Drawing

import Data.Colour (withOpacity)
import qualified Data.Colour.Names as Colors

-- Line overlays, box plots, heat maps
-- Stacking and graphing box plots
-- Generating legends
-- Generating (proper axises)
-- Visualize pairs, lists, ordered sets, maps, trees, directed graphs
-- Overlay lines (i.e. for interaction browser)

-- We will require basic envelopes
-- Optimally, also textual envelopes

-- $normalizeInputScalar
-- Input should be normalized so that for each point @x@ in input, x ∈ [0,1].

-- $normalizeInputPoint
-- Input should be normalized so that for each point @Point x y@ in input, x ∈ [0,1], y ∈ [0,1].


-- | Draw a scatter plot.
--   Can be combined with 'lineData'.
--   $normalizeInputPoint
scatterData :: [Point] -> Drawing
scatterData ps = scale 300 $ mconcat $ fmap (\p -> translate (p .-. origin) base) ps
  where
    base = fillColorA (Colors.red `withOpacity` 0.6) $ scale (10/300) circle
    origin = Point 0 0
    -- scaling a b = Transformation (a,0,0,b,0,0)

-- | Draw a line plot.
--   Can be combined with 'scatterData'.
--   $normalizeInputPoint
lineData :: [Point] -> Drawing
lineData []     = mempty
lineData [_]    = mempty
lineData (p:ps) = scale 300 $ translate (p .-. origin) $ lineStyle $ segments $ betweenPoints $ (p:ps)
  where
    lineStyle = strokeColorA (Colors.red `withOpacity` 0.6) . fillColorA (Colors.black `withOpacity` 0) . strokeWidth 1.3
    -- translation a b = Transformation (0,0,0,0,a,b)
    -- scaling a b = Transformation (a,0,0,b,0,0)
    origin = Point 0 0

-- | Draw a box plot.
--   $normalizeInputScalar
boxData :: [Double] -> Drawing
boxData ps = scale 300 $ mconcat $
    fmap (\p -> scaleX (1/fromIntegral (length ps)) $ scaleY p $ base) ps
  where
    -- TODO horizontal stacking (nicer with proper envelopes!)
    base = fillColorA (Colors.blue `withOpacity` 0.6) $ square

-- | Draw ticks.
-- Each argument is a list of tick positions (normalized to [0,1]) and an optional tick label.
ticks
  :: [(Double, JSString)] -- ^ X axis ticks.
  -> [(Double, JSString)] -- ^ Y axis ticks.
  -> Drawing
ticks xt yt = mconcat [xTicks, yTicks]
  where
    xTicks = mconcat $ flip fmap xt $
      \(pos,str) -> translateX (pos * 300) $
        (scale kBasicTickLength $ strokeColor Colors.black $ translateY (-0.5) verticalLine) <> (translateY (kBasicTickLength * (-1.5)) .rotate (turn*1/4)) (textEnd str)
    yTicks = mconcat $ flip fmap yt $
      \(pos,str) -> translateY (pos * 300) $
        (scale kBasicTickLength $ strokeColor Colors.black $ translateX (-0.5) horizontalLine) <> (translateX (kBasicTickLength * (-1.5)) .rotate (turn*0/4)) (textEnd str)

    kBasicTickLength = 10
    -- kPositionTickRelAxis = (-0.5) -- (-0.5) for outside axis, 0 for centered around axis, 0.5 for inside
    -- kPositionLabelRelAxis = (-0.8) -- (kPositionTickRelAxis-0) to make label touch tick, (kPositionTickRelAxis-1) to offset by length of tick

-- | Draw X and Y axis.
labeledAxis
  :: JSString -- ^ X axis label.
  -> JSString -- ^ Y axis label.
  -> Drawing
labeledAxis labelX labelY = mconcat
  [ scale 300 $ axis
  , translateY (300/2) $ translateX (-20) $ rotate (turn/4) $ textMiddle labelY
  , translateX (300/2) $ translateY (-20) $ textMiddle labelX]

axis = mconcat [axisY, axisX]
axisY = strokeWidth 2 $ strokeColor Colors.black $ translateY 0.5 verticalLine
axisX = strokeWidth 2 $ strokeColor Colors.black $ translateX 0.5 horizontalLine
