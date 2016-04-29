
{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
  , QuasiQuotes
  , OverloadedStrings
  , TupleSections
  , TemplateHaskell
  , ConstraintKinds
  #-}

module Lubeck.DV.Internal.Render
  where

import Prelude hiding (div)
import qualified Prelude

import Control.Applicative
import Control.Lens (to)
import Control.Lens.Operators
import Control.Lens.TH (makeLenses)
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Colour (Colour, AlphaColour, withOpacity, blend, alphaChannel)
import Data.Monoid
import Data.Map(Map)
import qualified Data.Colour.Names as Colors

import Linear.Vector
import Linear.Affine
import Linear.V0
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4

import Lubeck.Str
import Lubeck.Drawing
import Lubeck.DV.Styling
import qualified Lubeck.Drawing
import Lubeck.DV.ColorPalette
  ( Palette
  , singleColour
  , paletteFromList
  , getColorFromPalette
  , paletteToColor
  )
import Lubeck.DV.LineStyles
  ( extractLineStyle )

-- Util
transformIntoRect :: Styling -> P2 Double -> P2 Double
transformIntoRect style = transformPoint (scalingX (zoo^._x * rect^._x) <> scalingY (zoo^._y * rect^._y))
  where
    zoo   = style^.zoom
    rect  = style^.renderingRectangle

-- Util
relOrigin :: (Num n, Num (v n), Additive v) => Point v n -> v n
relOrigin p = p .-. 0
--
-- -- TODO more general pattern here
-- -- Capture with TFs?
-- addV1_2 :: V1 n -> V2 n -> V3 n
-- addV1_1 (V1 x)   (V1 y)   = V2 x y
-- addV2_1 (V2 x y) (V1 z)   = V3 x y z
-- addV1_2 (V1 x)   (V2 y z) = V3 x y z
-- addV2_2 (V2 a b) (V2 c d) = V4 a b c d
--
-- addP1_2 :: P1 n -> P2 n -> P3 n
-- addP1_2 (P a) (P b) = P $ addV1_2 a b


{-
TODO
Support fixed colors in point, line, fill, area2
scatterData, lineData, fillData, areaData'

TODO
  scatter plot with different colors on elements
-}

data ScatterData = ScatterData
  { scatterDataColor :: Double
  }


scatterData :: (Monad m) => ScatterData -> [P2 Double] -> StyledT m Drawing
scatterData (ScatterData colorN) ps = do
  style <- ask
  let base  = id
            $ fillColorA (style^.scatterPlotFillColor.to (`getColorFromPalette` colorN))
            $ strokeWidth (style^.scatterPlotStrokeWidth)
            $ strokeColorA (style^.scatterPlotStrokeColor.to (`getColorFromPalette` colorN))
            $ scale (style^.scatterPlotSize) circle
  return $ mconcat $ fmap (\p -> translate (relOrigin p) base) $ fmap (transformIntoRect style) ps

scatterDataX :: (Monad m) => [P2 Double] -> StyledT m Drawing
scatterDataX ps = do
  style <- ask
  let base = strokeColorA (style^.scatterPlotStrokeColor.to paletteToColor) $ strokeWidth (style^.scatterPlotStrokeWidth) $ translateY 0.5 $ verticalLine
  return $ mconcat $ fmap (\p -> scaleY (style^.renderingRectangle._y) $ translateX (p^._x) base) $ fmap (transformIntoRect style) ps

scatterDataY :: (Monad m) => [P2 Double] ->  StyledT m Drawing
scatterDataY ps = do
  style <- ask
  let base = strokeColorA (style^.scatterPlotStrokeColor.to paletteToColor) $ strokeWidth (style^.scatterPlotStrokeWidth) $ translateX 0.5 $ horizontalLine
  return $ mconcat $ fmap (\p -> scaleX (style^.renderingRectangle._x) $ translateY (p^._y) base) $ fmap (transformIntoRect style) ps

data LineData = LineData
  { lineDataColor :: Double
  , lineDataShape :: Double
  }
defLineData = LineData 0 0

lineData :: (Monad m) => LineData -> [P2 Double] -> StyledT m Drawing
lineData _ []     = mempty
lineData _ [_]    = mempty
lineData (LineData colorN dashN) (p:ps) = do
  style <- ask
  let lineStyle = id
                . strokeColorA  (style^.linePlotStrokeColor.to (`getColorFromPalette` colorN))
                . fillColorA    (Colors.black `withOpacity` 0) -- transparent
                . strokeWidth   (style^.linePlotStrokeWidth)
                . dash          (style^.linePlotStroke. to (`extractLineStyle` dashN))
  return $ translate (relOrigin (transformIntoRect style p)) $ lineStyle $ segments $ betweenPoints $ fmap (transformIntoRect style) (p:ps)

data AreaData = AreaData
  { areaDataColor :: Double
  }

fillData :: (Monad m) => AreaData -> [P2 Double] -> StyledT m Drawing
fillData _ []     = mempty
fillData _ [_]    = mempty
fillData (AreaData colorN) (p:ps) = do
  style <- ask
  let lineStyle = id
                -- . strokeColorA  (style^.linePlotStrokeColor)
                . fillColorA    (style^.linePlotFillColor.to (`getColorFromPalette` colorN))
                -- . strokeWidth   (style^.linePlotStrokeWidth)
  return $ translate (relOrigin (transformIntoRect style pProjX)) $ lineStyle $ segments $ betweenPoints $ fmap (transformIntoRect style) $ addExtraPoints (p:ps)

  where
    -- Because of projection (below!), ignore y value for 1st point
    pProjX = P (V2 firstPointX 0) where P (V2 firstPointX _) = p

    -- Add points from first and last projected on the X axis to make sure space below line is completely filled.
    addExtraPoints ps = [proj $ head ps] ++ ps ++ [proj $ last ps]
      where
        proj (P (V2 x _)) = P (V2 x 0)

areaData :: (Monad m) => AreaData -> [P3 Double] -> StyledT m Drawing
areaData i ps = areaData' i $
  fmap (\p -> P $ V2 (p^._x) (p^._z)) ps
    <>
  fmap (\p -> P $ V2 (p^._x) (p^._y)) (reverse ps)

areaData' :: (Monad m) => AreaData -> [P2 Double] -> StyledT m Drawing
areaData' _ []     = mempty
areaData' _ [_]    = mempty
areaData' (AreaData colorN) (p:ps) = do
  style <- ask
  let lineStyle = fillColorA (style^.linePlotFillColor.to (`getColorFromPalette` colorN))
  return $ translate (relOrigin (transformIntoRect style p)) $ lineStyle $ segments $ betweenPoints $ fmap (transformIntoRect style) (p:ps)








-- | Draw a step chart.
--
-- Similar to 'lineData', except it renders a series of changes (vectors) relative a starting point.
--
-- Can be combined with `scatterData`, `scatterDataX` etc.
--
-- See /Visualize this/, p. 124
stepData :: (Monad m) => P2 Double -> [V2 Double] -> StyledT m Drawing
stepData z vs = lineData (LineData 1 1) (offsetVectors z vs)

-- | Draw a linear function @ax + b@. Renders the function in the [0..1] domain,
--   i.e to get a line intersecting the outer ends of the X and Y axis use @linearData (-1) 1@.
--
--   Can be combined with `scatterData`, `scatterDataX` etc.
linearData :: (Monad m) => Double -> Double -> StyledT m Drawing
linearData a b = lineData (LineData 1 1) $ fmap (\x -> P $ x `V2` f x) [0,1]
  where
    f x = a*x + b

-- TODO bar graphs can be transposed (x/y) (how?)

-- | Draw a one-dimensional bar graph.
--
-- For grouped/stacked charts, see `barData2`, `barData3` etc.
barData :: (Monad m) => [P1 Double] -> StyledT m Drawing
barData ps = do
  style <- ask
  let barWidth = 1/fromIntegral (length ps + 1)
  let barFullOffset = barWidth + barWidth * (style^.barPlotUngroupedOffset._x)
  let base = alignB $ fillColorA (style^.barPlotBarColors.to paletteToColor) $ square
  return $ scaleX (2/3) $ scaleRR style $ mconcat $ zipWith (\n -> translateX (n * barFullOffset)) [1..] $
    fmap (\(P (V1 v)) -> scaleX barWidth $ scaleY v $ base) ps
  where
    alignB = translate (V2 0 0.5)
    scaleRR = transform . scalingRR
    scalingRR style = let r = style^.renderingRectangle in scalingX (r^._x) <> scalingY (r^._y)

-- | Draw a two-dimensional bar graph.
--
--  Every dimension in the given data set maps to a seprate bar.
--
-- Bars can be redered as grouped (default), stacked, or on opposite sides of the axis, depending on styling.
barData2 :: (Monad m) => [P2 Double] -> StyledT m Drawing

-- | Draw a three-dimensional bar graph.
--
--  Every dimension in the given data set maps to a seprate bar.
--
-- Bars can be redered as grouped (default) or stacked depending on styling.
barData3 :: (Monad m) => [P3 Double] -> StyledT m Drawing

-- | Draw a four-dimensional bar graph.
--
--  Every dimension in the given data set maps to a seprate bar.
--
-- Bars can be redered as grouped (default) or stacked depending on styling.
barData4 :: (Monad m) => [P4 Double] -> StyledT m Drawing
[barData2, barData3, barData4] = undefined

-- rawBarData4 :: [[R]] -> StyledT m Drawing


-- | Draw a bar graph.
barDataWithColor  :: (Monad m) => [P2 Double] -> StyledT m Drawing
-- | Draw a bar graph.
barDataWithColor2 :: (Monad m) => [P3 Double] -> StyledT m Drawing
-- | Draw a bar graph.
barDataWithColor3 :: (Monad m) => [P4 Double] -> StyledT m Drawing
-- | Draw
-- barDataWithColor4 :: [R5] -> StyledT m Drawing
[barDataWithColor, barDataWithColor2, barDataWithColor3] = undefined

{-
  Full insternal spec of a bar graph:
    - Stylings
      stack/dodge (behavior when number of dimensions > 1)
      transpose (transposed or not)
    - Data
      height
      color
-}

barDataWithColorN  :: (Monad m) => [[P2 Double]] -> StyledT m Drawing
barDataWithColorN pss = do
  -- TODO draw all dimensions
  let ps = head pss

  style <- ask
  let barWidth = 1/fromIntegral (length ps + 1)
  let barFullOffset = barWidth + barWidth * (style^.barPlotUngroupedOffset._x)
  -- TODO derive color from value below
  -- The color we recieve is *always* scaled to be in [0..1]
  -- Do we interpolate this over the palette, or is there a better way?
  --  Do we need to generalize geoms so that they also have access to *unscaled data*?
  let base = alignB $ fillColorA (style^.barPlotBarColors.to paletteToColor) $ square
  return $ scaleX (2/3) $ scaleRR style $ mconcat $ zipWith (\n -> translateX (n * barFullOffset)) [1..] $
    fmap (\(P (V2 v color)) -> scaleX barWidth $ scaleY v $ base) ps
  where
    alignB = translate (V2 0 0.5)
    scaleRR = transform . scalingRR
    scalingRR style = let r = style^.renderingRectangle in scalingX (r^._x) <> scalingY (r^._y)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~






-- | Visualizes a discrete count.
--
-- By default, this renders as a set of squares, coloured and grouped by the given element count.
--
-- See /Visualize this/ p XXII (Godfather example)
discreteData :: (Enum a, Monad m) => [(a, Int)] -> StyledT m Drawing
discreteData = undefined

-- TODO calendar map, see Visualize this p70

-- | Discrete 2D heat map
-- See "Visualize this, p 233"
-- heatDiscrete2D :: (Enum a, Enum b) => (a -> b -> Double)

-- | Visualizes an integer.
-- intData :: (Monad m) => P1 Int -> StyledT m Drawing
-- intData (P (V1 n)) = discreteDataGroup (replicate n ())

-- | Visualizes a ratio. Similar to 'barData', but only shows a single element.
--
-- See http://webbddatascience.demo.aspnetzero.com/Application#/tenant/dashboard
ratioData :: (Monad m) => P1 Double -> StyledT m Drawing
ratioData (P (V1 v)) = do
  style <- ask
  let fg = style^.ratioPlotForegroundColor.to paletteToColor
  let bg = style^.ratioPlotBackgroundColor.to paletteToColor
  return $ transform (scalingRR style) (fillColorA fg (scaleY v (alignBL square)) <> fillColorA bg (alignBL square))
  where
    -- TODO move
    alignBL = translate (V2 0.5 0.5)
    scalingRR style = let r = style^.renderingRectangle in scalingX (r^._x) <> scalingY (r^._y)


-- | Visualizes ratio with colour.
--
-- a la http://webbddatascience.demo.aspnetzero.com/Application#/tenant/dashboard
ratioDataWithColor :: (Monad m) => P2 Double -> StyledT m Drawing
ratioDataWithColor (P (V2 v1 v2)) = do
  style <- ask
  let bg  = style^.ratioPlotBackgroundColor. to paletteToColor
  -- let fg1 = style^.heatMapColour1
  -- let fg2 = style^.heatMapColour2
  -- TODO new styling here
  let fg1 = style^.ratioPlotForegroundColor.to paletteToColor
  let fg2 = style^.ratioPlotBackgroundColor.to paletteToColor
  let fg  = blend v2 fg1 fg2
  return $ transform (scalingRR style) (fillColorA fg (scaleY v1 (alignBL square)) <> fillColorA bg (alignBL square))
  where
    -- TODO move
    alignBL = translate (V2 0.5 0.5)
    scalingRR style = let r = style^.renderingRectangle in scalingX (r^._x) <> scalingY (r^._y)

-- TODO consolidate ratioData, ratioDataWithColor


-- | Draws the plotting rectangle.
--
-- Useful for deugging.
plotRectangle :: (Monad m) => StyledT m Drawing
plotRectangle = do
  style <- ask
  return $ transform (scalingRR style) (scale 2 xyCoords)
  where
    scalingRR style = let r = style^.renderingRectangle in scalingX (r^._x) <> scalingY (r^._y)

-- -- | Draw a circle size plot.
-- circleData :: (Monad m) => [P1 Double] -> StyledT m Drawing
-- circleData = undefined
-- -- TODO use a ratio/percantage type wrapper
-- -- TODO use area not radius
--
-- -- | Draw a pie chart.
-- pieChartData :: (Monad m) => [P1 Double] -> StyledT m Drawing
-- pieChartData = undefined
-- -- See https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Clipping_and_masking
--
-- -- | Draw
-- circleDataWithColor :: (Monad m) => [P2 Double] -> StyledT m Drawing
-- circleDataWithColor = undefined

-- circleDataWithColor = do
--   s <- getStyling
--   sizedData (baseCircleFromStyling c)
--   where
--     baseCircleFromStyling = ...

-- Higher order bar graphs.
-- Can render these by
-- color mapping + one of
--   stacking, grouping, alternating (same as grouping with no spacing), above/below (2 dimensions only)

--  Draw a bar graph.
-- barData2 :: [P2 Double] -> Styled Drawing
-- barData2 :: [P3 Double] -> Styled Drawing
-- barData2 :: [P4 Double] -> Styled Drawing

--  A size graph: scales the given objets and places them side by side.
-- sizedData :: [R] -> Styled Drawing -> Styled Drawing

-- | Draw a tree map.
-- treeMapGraph :: (Monad m) => [P1 Double] -> StyledT m Drawing
-- treeMapGraph = undefined
-- TODO use a ratio/percantage type wrapper


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
-- treeMapGraphWithColor :: (Monad m) => [P2 Double] -> StyledT m Drawing
-- treeMapGraphWithColor = undefined

-- | Draw a discrete heat map.
--
-- Example http://bokeh.pydata.org/en/latest/docs/gallery/les_mis.html
-- discreteHeatMap :: (Monad m) => [a] -> [b] -> (a -> b -> Double) -> StyledT m Drawing
-- discreteHeatMap = undefined
-- TODO use a ratio/percantage type wrapper




-- TODO Visualize pairs, lists, ordered sets, maps, trees, directed graphs
-- TODO Pie charts

-- | Basic type of legend.
-- Every string is matched to a single color.
--
-- >>> quantLegend ["Male", "Female"]
quantLegend :: [Str] -> StyledT m Drawing
quantLegend = undefined

-- | Similar to 'quantLegend', except strings indicate endpoint of a region.
-- The number of visualized
--
-- >>> quantLegend ["0", "50", "100"]
intervalLegend :: [Str] -> StyledT m Drawing
intervalLegend = undefined

--  | A legend where colours are interpolated in a continous space (i.e. for heat maps).
--
-- >>> scaleLegend (packStr . show) (0, 100)
scaleLegend :: (a -> Str) -> (a, a) -> StyledT m Drawing
scaleLegend = undefined

-- | Draw a title.
title :: Str -> StyledT m Drawing
title = undefined


-- TODO alternating tick size (i.e. every 50 year, 100 year etc)

-- TODO we should generalize this not to assume 2 axes
-- As far as we are concerned here there might be up to 4 axes (there may be more by overlaying)

-- TODO some creative tick positioning here
-- https://knowledge.infogr.am/featured

-- | Draw ticks.
--
-- Same as 'ticksNoFilter' with a sanity check to remove ticks outside of quadrant.
ticks
  :: Monad m
  => [(Double, Maybe Str)] -- ^ X axis ticks.
  -> [(Double, Maybe Str)] -- ^ Y axis ticks.
  -> StyledT m Drawing
ticks xt yt = ticksNoFilter (filterTicks xt) (filterTicks yt)
  where
    filterTicks = filter (withinNormRange . fst)
    withinNormRange x = 0 <= x && x <= 1

-- | Draw ticks.
--
-- Each argument is a list of tick positions (normalized to [0,1]) and label.
-- If the label is @Nothing@ this is rendered as a minor tick, which often
-- implies a less pronounced styling compared to major ticks (typically:
-- shorter tick lines, lighter colours).
--
-- To render a major tick without label, use @Just mempty@.
--
-- Contrary to 'ticks', 'ticksNoFilter' accept ticks at arbitrary positions.
ticksNoFilter
  :: Monad m
  => [(Double, Maybe Str)] -- ^ X axis ticks.
  -> [(Double, Maybe Str)] -- ^ Y axis ticks.
  -> StyledT m Drawing
ticksNoFilter xt yt = do
  style <- ask

  let x = (style^.zoom._x) * (style^.renderingRectangle._x)
  let y = (style^.zoom._y) * (style^.renderingRectangle._y)

  let (xTickTurn, yTickTurn) = style^.tickTextTurn -- (1/8, 0)

  let tl         = style^.basicTickLength
  let widthFgB   = style^.basicTickStrokeWidth
  let widthBgX   = style^.backgroundTickStrokeWidthX
  let widthBgY   = style^.backgroundTickStrokeWidthY
  let colFgB     = style^.basicTickColor
  let colBgX     = style^.backgroundTickStrokeColorX
  let colBgY     = style^.backgroundTickStrokeColorY
  let drawBgX    = not $ isTransparent colBgX
  let drawBgY    = not $ isTransparent colBgY

  -- TODO derive properly
  -- let tlMin      = style^.basicTickLength
  -- let widthFgBMin   = style^.basicTickStrokeWidth
  -- let widthBgXMin   = style^.backgroundTickStrokeWidthX
  -- let widthBgYMin   = style^.backgroundTickStrokeWidthY
  -- let colFgBMin     = style^.basicTickColor
  -- let colBgXMin     = style^.backgroundTickStrokeColorX
  -- let colBgYMin     = style^.backgroundTickStrokeColorY

  let xTicks = mconcat $ flip fmap xt $
          \(pos,str) -> translateX (pos * x) $ mconcat
            [ mempty
            -- Inside quadrant (background) grid
            , if not drawBgX then mempty else
                strokeWidth widthBgX $ strokeColorA colBgX $ scale y $ translateY (0.5) verticalLine
            -- Outside quadrant tick
            , strokeWidth widthFgB $ strokeColorA colFgB $ scale tl $ translateY (-0.5) verticalLine
            -- Text
            , maybe mempty id $ fmap (\str -> translateY (tl * (-1.5)) . rotate (turn*xTickTurn) $ textX style str) $ str
            ]
  let yTicks = mconcat $ flip fmap yt $
          \(pos,str) -> translateY (pos * y) $ mconcat
            [ mempty
            -- Inside quadrant (background) grid
            , if not drawBgY then mempty else
                strokeWidth widthBgY $ strokeColorA colBgY $ scale x $ translateX (0.5) horizontalLine
            -- Outside quadrant tick
            , strokeWidth widthFgB $ strokeColorA colFgB $ scale tl $ translateX (-0.5) horizontalLine
            -- Text
            , maybe mempty id $ fmap (\str -> translateX (tl * (-1.5)) . rotate (turn*yTickTurn) $ textY style str) $ str
            ]
  return $ mconcat [xTicks, yTicks]
  where
    -- kBasicTickLength = 10

    -- Note: Add infinitesimal slant to non-slanted text to get same anti-aliasing behavior
    -- kPositionTickRelAxis = (-0.5) -- (-0.5) for outside axis, 0 for centered around axis, 0.5 for inside
    -- kPositionLabelRelAxis = (-0.8) -- (kPositionTickRelAxis-0) to make label touch tick, (kPositionTickRelAxis-1) to offset by length of tick

    textX = text_ fst fst
    textY = text_ snd snd
    text_ which which2 style = textWithOptions $ mempty
      { textAnchor        = style^.tickTextAnchor.to which
      , alignmentBaseline = style^.tickTextAlignmentBaseline.to which2
      -- TODO read family from style
      , fontFamily = style^.tickTextFontFamily
      , fontStyle  = style^.tickTextFontStyle
      , fontSize   = First $ Just $ (toStr $ style^.tickTextFontSizePx) <> "px"
      , fontWeight = style^.tickTextFontWeight
      }
    isTransparent color = abs (alphaChannel color) < 0.001

-- | Draw X and Y axis.
labeledAxis
  :: Monad m
  => Str -- ^ X axis label.
  -> Str -- ^ Y axis label.
  -> StyledT m Drawing
labeledAxis labelX labelY = do
  style <- ask
  let x = style^.renderingRectangle._x
  let y = style^.renderingRectangle._y

  let axisX = strokeWidth (style^.axisStrokeWidth.to fst) $ strokeColorA (style^.axisStrokeColor.to fst) $ translateX 0.5 horizontalLine
  let axisY = strokeWidth (style^.axisStrokeWidth.to snd) $ strokeColorA (style^.axisStrokeColor.to snd) $ translateY 0.5 verticalLine
  let axis = mconcat [axisY, axisX]

  return $ mconcat
    [ scaleX x $ scaleY y $ axis
    , translateX (x/2) $ translateY (-50*x/300) $ text_ style labelX
    , translateY (y/2) $ translateX (-50*y/300) $ rotate (turn/4) $ text_ style labelY
    ]
  where
    text_ style= textWithOptions $ mempty
      { textAnchor = TextAnchorMiddle
      , fontFamily = style^.axisTextFontFamily
      , fontWeight = style^.axisTextFontWeight
      , fontStyle  = style^.axisTextFontStyle
      , fontSize   = First $ Just $ (toStr $ style^.axisTextFontSizePx) <> "px"
      }
