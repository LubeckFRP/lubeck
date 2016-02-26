
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections,
  TemplateHaskell, CPP #-}

-- |
-- Basics for drawing plots.
--
-- TODO nice docs
--
-- Essentially:
--
-- * This modlules draws basic static, animated and interactive graphics
--   For static graphics, use 'Styled', for animatedinteractive, use 'StyledT Behavior' or similar.
--
-- * Looks are provided by the 'Styled' monad. Use lens API to modify styling attributes.
--
-- * TODO substyling
--
-- * Data is generally recieved in R^n. Do normalization and axes  elsewhere.
--
-- This module should NOT handle:
--
--  * Data normalization (it expects everything in the unit hypercube)
--
--  * Fancy overloading (all data is in concrete types: arguments to functions below)
--
--  * Styling (everything parameterized on the style record)
--
--    ** Whatever the value of this the data will be rendered "correctly" (if not "intelligibly")
module Lubeck.DV.Drawing
  (
  -- * Drawing data

  -- $normalizeInputPoint
  -- $normalizeInputScalar

  -- ** Scatter
    scatterData
  , scatterDataX
  , scatterDataY

  -- ** Lines
  , lineData
  , stepData
  , linearData

  -- ** Bars and sizes
  , barData
  , barData2
  , barData3
  , barData4

  , barDataWithColor
  , barDataWithColor2
  , barDataWithColor3
  , barDataWithColor4

  , circleData
  , circleDataWithColor
  , pieChartData

  -- | Ratios
  , ratioData
  , ratioDataWithColor

  -- ** Discrete data and counts
  , discreteData
  , intData
  , discreteHeatMap

  , treeMapGraph
  , treeMapGraphWithColor


  -- * Drawing axes
  , ticks
  , ticksNoFilter
  , labeledAxis
  -- , crossLineX
  -- , crossLineY
  , plotRectangle



  -- * Drawing legends

  -- * Drawing titles

  -- * Drawing overlays/explanatories

  -- * Styling

  -- ** Building a style
  , Styling
  -- TODO exort all lenses here
  , renderingRectangle
  , linePlotStrokeColor
  , linePlotStrokeWidth
  , linePlotStrokeType
  , linePlotFillColor

  , scatterPlotStrokeColor
  , scatterPlotFillColor
  , scatterPlotShape

  , barPlotBarColors
  , barPlotWidth
  , barPlotUngroupedOffset
  , barPlotGroupedOffset
  , barPlotSpaceUsed

  -- ** Running a style
  , Styled
  , getStyled
  , withDefaultStyle

  , StyledT
  , getStyledT
  , withDefaultStyleT
  ) where

import Prelude hiding (div)
import qualified Prelude

import Control.Applicative
import Control.Lens ()
import Control.Lens.Operators
import Control.Lens.TH (makeLenses)
import Control.Monad.Identity
import Control.Monad.Reader
-- import Data.AffineSpace
import Data.Colour (Colour, AlphaColour, withOpacity, blend)
import Data.Monoid ((<>), First(..))
-- import Data.VectorSpace
import GHCJS.Types(JSString, jsval)
import qualified Data.Colour.Names as Colors
import qualified Data.JSString
import qualified Data.VectorSpace as VS

import Linear.Vector
import Linear.Affine
-- import Linear.Matrix hiding (translation)
-- import Linear.Metric -- Needed?
import Linear.V0
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4

import Lubeck.Drawing
import Lubeck.Util(showJS)
import qualified Lubeck.Drawing

data VerticalHorizontal = Vertical | Horizontal
-- How to display a bar plot with more than two dimensions.§
data BarPlotType = Grouped | Stacked | TwoSides

data Styling = Styling
  { _dummy                            :: ()

  -- ^ Rectangle in which the plot will be rendered (default @300 x 300@)
  , _renderingRectangle               :: V2 Double

  -- Line plots
  , _linePlotStrokeColor              :: AlphaColour Double
  , _linePlotStrokeWidth              :: Double
  , _linePlotStrokeType               :: () -- TODO
  , _linePlotFillColor                :: AlphaColour Double

  -- Scatter plots
  , _scatterPlotStrokeColor           :: AlphaColour Double
  , _scatterPlotStrokeWidth           :: Double
  , _scatterPlotFillColor             :: AlphaColour Double
  , _scatterPlotSize                  :: Double
  , _scatterPlotShape                 :: ()

  -- Bar plots
  -- Infinite list of bar colours:
  , _barPlotBarColors                 :: [AlphaColour Double]
  , _barPlotWidth                     :: V2 Double
  , _barPlotUngroupedOffset           :: V2 Double
  , _barPlotGroupedOffset             :: V2 Double
  , _barPlotStackedOffset             :: V2 Double
  -- Percentage of horizontal dim taken up by plots, in [0..1] (default 1)
  -- I.e. https://infogr.am/average_temperature_of_6_major_deserts
  , _barPlotSpaceUsed                 :: Double

  , _ratioPlotBackgroundColor         :: AlphaColour Double
  , _ratioPlotForegroundColor         :: AlphaColour Double

  -- Color allocator
    -- TODO idea: to allocate colors to categories/dimensions
    -- This could work very well with Control.Monad.Reader.local.

    -- I.e. there should be a function (subStyle 1 4 :: Styling -> Styling) that transforms the styling
    -- (in this case 1 out of 4), by recursively applying over all styles that support this (i.e. the bar colour space).

    -- NOTE there are two ways of using color with this API: through styling (appropriate for
    -- bar groups etc where the color is not strictly bound to the data), or through an extra R dimension ("withColor",
    -- appropriate for heat maps etc)

  -- Axis/ticks
    -- X,Y axis name
      -- NOTE: Standard styles: left/below centered, at end

    -- X Axis standard (usually left) line (strokeWith, strokeColorA)
    -- X Axis opposite                line (strokeWith, strokeColorA)
    -- Y Axis standard                line (strokeWith, strokeColorA)
    -- Y Axis opposite                line (strokeWith, strokeColorA)

    -- Axis arrow end?

    -- NOTE: strike-through/background ticks are rarely used together
    -- X,Y strike-through/background ticks
    -- X,Y tick (length, width, pos rel axis (see below), strokeColorA)
    -- Text position relative tick
    -- Text rotation
      -- NOTE: common(x,y) is (turn/4,0), (turn/8,0), (0,0)

  -- Heat maps and related
  , _heatMapColour1                   :: AlphaColour Double
  , _heatMapColour2                   :: AlphaColour Double

  }
  deriving (Show)

makeLenses ''Styling

instance Monoid Styling where
  mempty = Styling
    { _dummy                        = mempty
    , _renderingRectangle           = V2 300 300

    , _linePlotStrokeColor          = Colors.red `withOpacity` 0.6
    , _linePlotStrokeWidth          = 2.5
    , _linePlotStrokeType           = mempty
    , _linePlotFillColor            = Colors.black `withOpacity` 0

    , _scatterPlotStrokeColor       = Colors.red `withOpacity` 0.6
    , _scatterPlotStrokeWidth       = 1
    , _scatterPlotFillColor         = Colors.red `withOpacity` 0.6
    , _scatterPlotSize              = 10 -- TODO should really be a ratio of rendering rectangle (x or y?)
    , _scatterPlotShape             = mempty

    , _barPlotBarColors             = fmap (`withOpacity` 0.6) $ cycle
                                      [ Colors.red
                                      , Colors.green
                                      , Colors.blue
                                      , Colors.pink
                                      , Colors.orange
                                      , Colors.purple
                                      ]
    , _barPlotWidth                 = V2 1   0 -- TODO not actually used as other values are relative this anyway
    , _barPlotUngroupedOffset       = V2 0.5 0
    , _barPlotGroupedOffset         = V2 0   0
    , _barPlotStackedOffset         = V2 0   0.1
    , _barPlotSpaceUsed             = 9/10

    , _ratioPlotBackgroundColor     = Colors.whitesmoke `withOpacity` 0.9
    , _ratioPlotForegroundColor     = Colors.red        `withOpacity` 0.6

    , _heatMapColour1               = Colors.red        `withOpacity` 1
    , _heatMapColour2               = Colors.purple     `withOpacity` 1
    }
  mappend = const

type Styled = StyledT Identity

newtype StyledT m a = Styled { _getStyled :: ReaderT Styling m a }
  deriving (Functor, Applicative, Monad, MonadReader Styling)

instance (Monad m, Monoid a) => Monoid (StyledT m a) where
  mempty = pure mempty
  mappend = liftA2 mappend

-- | Extract a 'Styled' value.
getStyled :: Styled a -> Styling -> a
getStyled = runReader . _getStyled

-- | Extract a 'Styled' value.
getStyledT :: StyledT m a -> Styling -> m a
getStyledT = runReaderT . _getStyled

withDefaultStyle :: Styled a -> a
withDefaultStyle x = getStyled x mempty

withDefaultStyleT :: StyledT m a -> m a
withDefaultStyleT x = getStyledT x mempty

-- TODO consolidate origin, intoRect

-- | Draw data for a scatter plot.
scatterData :: Monad m => [P2 Double] -> StyledT m Drawing
scatterData ps = do
  style <- ask
  let base  = id
            $ fillColorA (style^.scatterPlotFillColor)
            $ strokeColorA (style^.scatterPlotStrokeColor)
            $ scale (style^.scatterPlotSize) circle
  let origin = P $ V2 0 0
  let intoRect = transformPoint (scalingX (style^.renderingRectangle._x) <> scalingY (style^.renderingRectangle._y))
  return $ mconcat $ fmap (\p -> translate (p .-. origin) base) (fmap intoRect ps)

-- | Draw data for a scatter plot ignoring Y values.
scatterDataX :: Monad m => [P2 Double] -> StyledT m Drawing
scatterDataX ps = do
  style <- ask
  let base = strokeColorA (style^.scatterPlotStrokeColor) $ strokeWidth 1.5 $ translateY 0.5 $ verticalLine
  let origin = P $ V2 0 0
  let intoRect = transformPoint (scalingX (style^.renderingRectangle._x) <> scalingY (style^.renderingRectangle._y))
  return $ mconcat $ fmap (\p -> scaleY (style^.renderingRectangle._y) $ translateX (p^._x) base) (fmap intoRect ps)

-- | Draw data for a scatter plot ignoring X values.
scatterDataY :: Monad m => [P2 Double] ->  StyledT m Drawing
scatterDataY ps = do
  style <- ask
  let base = strokeColorA (style^.scatterPlotStrokeColor) $ strokeWidth 1.5 $ translateX 0.5 $ horizontalLine
  let origin = P $ V2 0 0
  let intoRect = transformPoint (scalingX (style^.renderingRectangle._x) <> scalingY (style^.renderingRectangle._y))
  return $ mconcat $ fmap (\p -> scaleX (style^.renderingRectangle._x) $ translateY (p^._y) base) (fmap intoRect ps)

-- | Draw data for a line plot.
lineData :: [P2 Double] -> Styled Drawing
lineData []     = mempty
lineData [_]    = mempty
lineData (p:ps) = do
  style <- ask
  let lineStyle = id
                . strokeColorA  (style^.linePlotStrokeColor)
                . fillColorA    (style^.linePlotFillColor)
                . strokeWidth   (style^.linePlotStrokeWidth)
  let origin = P $ V2 0 0
  let intoRect = transformPoint (scalingX (style^.renderingRectangle._x) <> scalingY (style^.renderingRectangle._y))
  return $ translate (intoRect p .-. origin) $ lineStyle $ segments $ betweenPoints $ fmap intoRect (p:ps)

-- | Step chart
--
-- See Visualize this p 124
stepData :: P2 Double -> [V2 Double] -> Styled Drawing
stepData z vs = lineData (offsetVectors z vs)

-- | Draw a linear function @ax + b@. Renders the function in the [0..1] domain,
--   i.e to get a line intersecting the outer ends of the X and Y axis use @linearData (-1) 1@.
linearData :: Double -> Double -> Styled Drawing
linearData a b = lineData $ fmap (\x -> P $ x `V2` f x) [0,1]
  where
    f x = a*x + b

-- TODO bar graphs can be transposed (x/y) (how?)

-- | Draw a bar graph.
barData :: [P1 Double] -> Styled Drawing
barData ps = do
  style <- ask
  let barWidth = 1/fromIntegral (length ps + 1)
  let barFullOffset = barWidth + barWidth * (style^.barPlotUngroupedOffset._x)
  let base = alignB $ fillColorA ((style^.barPlotBarColors) !! 0) $ square
  return $ scaleX (2/3) $ scaleRR style $ mconcat $ zipWith (\n -> translateX (n * barFullOffset)) [1..] $
    fmap (\(P (V1 v)) -> scaleX barWidth $ scaleY v $ base) ps
  where
    alignB = translate (V2 0 0.5)
    scaleRR = transform . scalingRR
    scalingRR style = let r = style^.renderingRectangle in scaling (r^._x) (r^._y)

-- | Draw
barData2 :: [P2 Double] -> Styled Drawing
-- | Draw
barData3 :: [P3 Double] -> Styled Drawing
-- | Draw
barData4 :: [P4 Double] -> Styled Drawing
[barData2, barData3, barData4] = undefined

-- rawBarData4 :: [[R]] -> Styled Drawing


-- | Draw
barDataWithColor  :: [P2 Double] -> Styled Drawing
-- | Draw
barDataWithColor2 :: [P3 Double] -> Styled Drawing
-- | Draw
barDataWithColor3 :: [P4 Double] -> Styled Drawing
-- | Draw
-- barDataWithColor4 :: [R5] -> Styled Drawing
[barDataWithColor, barDataWithColor2, barDataWithColor3, barDataWithColor4] = undefined

-- | Visualizes a count
-- See "Visualize this" pXXII (Godfather example)
discreteData :: Enum a => [(a, Int)] -> Styled Drawing
discreteData = undefined

-- TODO calendar map, see Visualize this p70

-- | Discrete 2D heat map
-- See "Visualize this, p 233"
-- heatDiscrete2D :: (Enum a, Enum b) => (a -> b -> Double)

-- | Visualizes a ratio. Essentially a 1-category bar graph.
-- a la http://webbddatascience.demo.aspnetzero.com/Application#/tenant/dashboard
intData :: Int -> Styled Drawing
intData = undefined

-- | Visualizes a ratio. Essentially a 1-category bar graph.
-- a la http://webbddatascience.demo.aspnetzero.com/Application#/tenant/dashboard
ratioData :: P1 Double -> Styled Drawing
ratioData (P (V1 v)) = do
  style <- ask
  let fg = style^.ratioPlotForegroundColor
  let bg = style^.ratioPlotBackgroundColor
  return $ transform (scalingRR style) (fillColorA fg (scaleY v (alignBL square)) <> fillColorA bg (alignBL square))
  where
    -- TODO move
    alignBL = translate (V2 0.5 0.5)
    scalingRR style = let r = style^.renderingRectangle in scaling (r^._x) (r^._y)


-- | Visualizes ration with colour.
-- a la http://webbddatascience.demo.aspnetzero.com/Application#/tenant/dashboard
ratioDataWithColor :: P2 Double -> Styled Drawing
ratioDataWithColor (P (V2 v1 v2)) = do
  style <- ask
  let bg  = style^.ratioPlotBackgroundColor
  let fg1 = style^.heatMapColour1
  let fg2 = style^.heatMapColour2
  let fg  = blend v2 fg1 fg2
  return $ transform (scalingRR style) (fillColorA fg (scaleY v1 (alignBL square)) <> fillColorA bg (alignBL square))
  where
    -- TODO move
    alignBL = translate (V2 0.5 0.5)
    scalingRR style = let r = style^.renderingRectangle in scaling (r^._x) (r^._y)

-- TODO consolidate ratioData, ratioDataWithColor


-- | Visualizes the plotting rectangle. Useful for deugging.
plotRectangle :: Styled Drawing
plotRectangle = do
  style <- ask
  return $ transform (scalingRR style) (scale 2 xyCoords)
  where
    scalingRR style = let r = style^.renderingRectangle in scaling (r^._x) (r^._y)

-- | Draw
-- TODO use a ratio/percantage type wrapper
-- TODO use area not radius
circleData :: [Double] -> Styled Drawing
circleData = undefined

-- | Draw
pieChartData :: [Double] -> Styled Drawing
pieChartData = undefined
-- See https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Clipping_and_masking


-- | Draw
circleDataWithColor :: [P2 Double] -> Styled Drawing
circleDataWithColor = undefined

-- circleDataWithColor = do
--   s <- getStyling
--   sizedData (baseCircleFromStyling c)
--   where
--     baseCircleFromStyling = ...

-- Higher order bar graphs.
-- Can render these by
-- color mapping + one of
--   stacking, grouping, alternating (same as grouping with no spacing), above/below (2 dimensions only)

-- | Draw a bar graph.
-- barData2 :: [P2 Double] -> Styled Drawing
-- barData2 :: [P3 Double] -> Styled Drawing
-- barData2 :: [P4 Double] -> Styled Drawing

-- | A size graph: scales the given objets and places them side by side.
-- sizedData :: [R] -> Styled Drawing -> Styled Drawing

-- | Draw a tree map.
-- TODO use a ratio/percantage type wrapper
treeMapGraph :: [Double] -> Styled Drawing
treeMapGraph = undefined
{-
Tree map like bottom one here:
https://infogr.am/link-building-strategies-from-the-experts
See also "Visualize this, p 157"
:
  Split horizontally, put 2 largest values to the left (split vertically), rest of values to the right by
  Split vertically, put 2 largest values at the top (split horizontally), rest of values at the bottom by
  etc

  (What to do if given an odd number of elements?)
-}


-- | Like 'treeMapGraph', mapping the last dimension to colour.
treeMapGraphWithColor :: [P2 Double] -> Styled Drawing
treeMapGraphWithColor = undefined

-- | Draw a discrete heat map.
-- TODO use a ratio/percantage type wrapper
discreteHeatMap :: (Int -> Int -> Double) -> Styled Drawing
discreteHeatMap = undefined




-- TODO Visualize pairs, lists, ordered sets, maps, trees, directed graphs
-- TODO Pie charts





-- TODO alternating tick size (i.e. every 50 year, 100 year etc)

-- TODO we should generalize this not to assume 2 axes
-- As far as we are concerned here there might be up to 4 axes (there may be more by overlaying)

-- TODO some creative tick positioning here
-- https://knowledge.infogr.am/featured

-- | Draw ticks.
-- Each argument is a list of tick positions (normalized to [0,1]) and an optional tick label.
-- Positions outside the normalized range are discarded.
ticks
  :: [(Double, JSString)] -- ^ X axis ticks.
  -> [(Double, JSString)] -- ^ Y axis ticks.
  -> Styled Drawing
ticks xt yt = ticksNoFilter (filterTicks xt) (filterTicks yt)
  where
    filterTicks = filter (withinNormRange . fst)
    withinNormRange x = 0 <= x && x <= 1

-- | Draw ticks.
-- Each argument is a list of tick positions (normalized to [0,1]) and an optional tick label.
-- Contrary to 'ticks', 'ticksNoFilter' accept ticks at arbitrary positions.
ticksNoFilter
  :: [(Double, JSString)] -- ^ X axis ticks.
  -> [(Double, JSString)] -- ^ Y axis ticks.
  -> Styled Drawing
ticksNoFilter xt yt = return $ mconcat [xTicks, yTicks]
  where
    xTicks = mconcat $ flip fmap xt $
      \(pos,str) -> translateX (pos * 300) $
        (scale kBasicTickLength $ strokeColor Colors.black $ strokeWidth 1.5 $ translateY (-0.5) verticalLine)
          <> (translateY (kBasicTickLength * (-1.5)) .rotate (turn*1/8)) (textEnd str)
    yTicks = mconcat $ flip fmap yt $
      \(pos,str) -> translateY (pos * 300) $
        (scale kBasicTickLength $ strokeColor Colors.black $ strokeWidth 1.5 $ translateX (-0.5) horizontalLine)
          <> (translateX (kBasicTickLength * (-1.5)) .rotate (turn*0.00001/8)) (textEnd str)

    kBasicTickLength = 10
    -- Note: Add infinitesimal slant to non-slanted text to get same anti-aliasing behavior
    -- kPositionTickRelAxis = (-0.5) -- (-0.5) for outside axis, 0 for centered around axis, 0.5 for inside
    -- kPositionLabelRelAxis = (-0.8) -- (kPositionTickRelAxis-0) to make label touch tick, (kPositionTickRelAxis-1) to offset by length of tick

-- | Draw X and Y axis.
labeledAxis
  :: JSString -- ^ X axis label.
  -> JSString -- ^ Y axis label.
  -> Styled Drawing
labeledAxis labelX labelY = return $ mconcat
  [ scale 300 $ axis
  , translateY (300/2) $ translateX (-20) $ rotate (turn/4) $ textMiddle labelY
  , translateX (300/2) $ translateY (-20) $ textMiddle labelX]

axis, axisX, axisY :: Drawing
axis = mconcat [axisY, axisX]
axisX = strokeWidth 1.5 $ strokeColor Colors.black $ translateX 0.5 horizontalLine
axisY = strokeWidth 1.5 $ strokeColor Colors.black $ translateY 0.5 verticalLine

crossLineX, crossLineY :: Double -> Drawing
crossLineX n = translateX (n * 300) $ strokeWidth 2 $ strokeColor Colors.lightblue $ axisY
crossLineY n = translateY (n * 300) $ strokeWidth 2 $ strokeColor Colors.lightblue $ axisX





-- These are all low-level drawing functions.
--
-- Conventions:
--
--  * All data is normalized to fall inside the /unit hypercube/, meaning that each point in the data
--    set can be expressed as a linear combination of scalars in the range @[0..1]@.
--
--  * The position of each data points is mapped from the hypercube into the plotting rectangle
--    (currently hardcoded as @[(0,0)..(300,300])@).
--
--  * Returns data in the 'Styled' monad. Basic idea is that exctracting values from 'Styled'
--    may affect display (sometimes dramatically), but never the basic semantics of the data.
--
--  Consequently:
--
--  * Data must be normalized and labels, and the labels normalized in the same withOpacity
--    as the data before using this module.
--
--  * Plots generated by this module can be overlayd using the 'Drawing' monoid instance.
--
-- $normalizeInputScalar
-- Input should be normalized so that for each point @x@ in input, x ∈ [0,1].
--
-- $normalizeInputPoint
-- Input should be normalized so that for each point @Point x y@ in input, x ∈ [0,1], y ∈ [0,1].
--
--
