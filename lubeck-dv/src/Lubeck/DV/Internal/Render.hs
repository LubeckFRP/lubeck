
{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
  , QuasiQuotes
  , OverloadedStrings
  , TupleSections
  , TemplateHaskell
  , ConstraintKinds
  , FlexibleContexts
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

transformIntoRect :: Styling -> P2 Double -> P2 Double
transformIntoRect style = transformPoint $
     scalingXY rect
  where
    -- zoo   = style^.zoom
    rect  = style^.renderingRectangle

-- Util
relOrigin :: (Num n, Num (v n), Additive v) => Point v n -> v n
relOrigin p = p .-. 0


{-
Take a point in the UHQ and transform it into its rendering position.
If the resulting position is outside the RR, return (Left p), otherwise return (Right p).

This is accomplished as follows:
  - Run the zoom affine transformation (if default zoom, this is the identity)
  - Optionally filter points that now falls outside the UHQ (i.e. when zooming in)
  - Run the linear transformation that defines the rendering rectangle (always a scaling)
-}
getRenderingPosition :: Styling -> P2 Double -> Either (P2 Double) (P2 Double)
getRenderingPosition styling x = fmap (transformPoint (scalingXY $ styling^.renderingRectangle)) $ Right $ transformPoint (styling^.zoom) x

  where

    -- TODO actually use filtering
    filterTicks :: [(Double, a)] -> [(Double, a)]
    filterTicks = filter (withinNormRange . fst)

    -- | Is a number within the normalized (UHQ) range?
    withinNormRange :: Double -> Bool
    withinNormRange x = 0 <= x && x <= 1


data ScatterData = ScatterData
  { scatterDataColor :: Double
  }

mapFilterEither :: (Monad m, Alternative m) => (a -> Either t b) -> m a -> m b
mapFilterEither f = (=<<) (g . f)
  where
    g (Left _)  = empty
    g (Right x) = pure x

-- TODO zoom OK
scatterData :: (Monad m, MonadReader Styling m, Monoid (m Drawing)) => ScatterData -> [P2 Double] -> m Drawing
scatterData (ScatterData colorN) ps = do
  style <- ask
  let base  = id
            $ fillColorA (style^.scatterPlotFillColor.to (`getColorFromPalette` colorN))
            $ strokeWidth (style^.scatterPlotStrokeWidth)
            $ strokeColorA (style^.scatterPlotStrokeColor.to (`getColorFromPalette` colorN))
            $ scale (style^.scatterPlotSize) circle
  return $ mconcat $ fmap (\p -> translate (relOrigin p) base) $ mapFilterEither (getRenderingPosition style) ps

-- TODO zoom
scatterDataX :: (Monad m, MonadReader Styling m, Monoid (m Drawing)) => [P2 Double] -> m Drawing
scatterDataX ps = do
  style <- ask
  let base = strokeColorA (style^.scatterPlotStrokeColor.to paletteToColor) $ strokeWidth (style^.scatterPlotStrokeWidth) $ translateY 0.5 $ verticalLine
  return $ mconcat $ fmap (\p -> scaleY (style^.renderingRectangle._y) $ translateX (p^._x) base) $ fmap (transformIntoRect style) ps

-- TODO zoom
scatterDataY :: (Monad m, MonadReader Styling m, Monoid (m Drawing)) => [P2 Double] ->  m Drawing
scatterDataY ps = do
  style <- ask
  let base = strokeColorA (style^.scatterPlotStrokeColor.to paletteToColor) $ strokeWidth (style^.scatterPlotStrokeWidth) $ translateX 0.5 $ horizontalLine
  return $ mconcat $ fmap (\p -> scaleX (style^.renderingRectangle._x) $ translateY (p^._y) base) $ fmap (transformIntoRect style) ps

data LineData = LineData
  { lineDataColor :: Double
  , lineDataShape :: Double
  }
defLineData = LineData 0 0

-- TODO zoom OK
lineData :: (Monad m, MonadReader Styling m, Monoid (m Drawing)) => LineData -> [P2 Double] -> m Drawing
lineData _ []     = mempty
lineData _ [_]    = mempty
lineData (LineData colorN dashN) (p:ps) = do
  style <- ask
  let lineStyle = id
                . strokeColorA  (style^.linePlotStrokeColor.to (`getColorFromPalette` colorN))
                . fillColorA    (Colors.black `withOpacity` 0) -- transparent
                . strokeWidth   (style^.linePlotStrokeWidth)
                . dash          (style^.linePlotStroke. to (`extractLineStyle` dashN))
  return $ (either (const mempty) translate $ fmap relOrigin $ getRenderingPosition style p) $ lineStyle $ segments $ betweenPoints $ mapFilterEither (getRenderingPosition style) (p:ps)

data AreaData = AreaData
  { areaDataColor :: Double
  }

-- TODO zoom OK
fillData :: (Monad m, MonadReader Styling m, Monoid (m Drawing)) => AreaData -> [P2 Double] -> m Drawing
fillData _ []     = mempty
fillData _ [_]    = mempty
fillData (AreaData colorN) (p:ps) = do
  style <- ask
  let lineStyle = id
                . fillColorA    (style^.linePlotFillColor.to (`getColorFromPalette` colorN))
  -- return $ (either id translate $ relOrigin $ getRenderingPosition style pProjX) $ lineStyle $ fmap segments $ fmap betweenPoints $ mapFilterEither (getRenderingPosition style) $ addExtraPoints (p:ps)
  return mempty
  where
    -- Because of projection (below!), ignore y value for 1st point
    pProjX = P (V2 firstPointX 0) where P (V2 firstPointX _) = p

    -- Add points from first and last projected on the X axis to make sure space below line is completely filled.
    addExtraPoints ps = [proj $ head ps] ++ ps ++ [proj $ last ps]
      where
        proj (P (V2 x _)) = P (V2 x 0)

areaData :: (Monad m, MonadReader Styling m, Monoid (m Drawing)) => AreaData -> [P3 Double] -> m Drawing
areaData i ps = areaData' i $
  fmap (\p -> P $ V2 (p^._x) (p^._z)) ps
    <>
  fmap (\p -> P $ V2 (p^._x) (p^._y)) (reverse ps)

-- TODO zoom
areaData' :: (Monad m, MonadReader Styling m, Monoid (m Drawing)) => AreaData -> [P2 Double] -> m Drawing
areaData' _ []     = mempty
areaData' _ [_]    = mempty
areaData' (AreaData colorN) (p:ps) = do
  style <- ask
  let lineStyle = fillColorA (style^.linePlotFillColor.to (`getColorFromPalette` colorN))
  return $ translate (relOrigin (transformIntoRect style p)) $ lineStyle $ segments $ betweenPoints $ fmap (transformIntoRect style) (p:ps)

-- TODO zoom
-- | Draw a one-dimensional bar graph.
--
-- For grouped/stacked charts, see `barData2`, `barData3` etc.
barData :: (Monad m, MonadReader Styling m) => [P1 Double] -> m Drawing
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


-- TODO zoom
baseImage :: (Monad m, MonadReader Styling m) => Drawing -> Double -> Double -> Maybe Double -> m Drawing
baseImage dr x y Nothing     = baseImage dr x y (Just 1)
baseImage dr x y (Just size) = do
  style <- ask
  return $ Lubeck.Drawing.translateX (x * style^.renderingRectangle._x)
    $ Lubeck.Drawing.translateY (y * style^.renderingRectangle._y)
    $ Lubeck.Drawing.scale size
    $ dr

-- TODO zoom
baseLabel :: MonadReader Styling m => Double -> Double -> Str -> m Drawing
baseLabel x y str = do
    style <- ask
    return $ Lubeck.Drawing.translateX (x * {-style^.zoom._x *-} style^.renderingRectangle._x)
      $ Lubeck.Drawing.translateY (y * {-style^.zoom._y *-} style^.renderingRectangle._y)
      -- TODO font
      $ text_ style str

  where
    text_ style = fmap (Lubeck.Drawing.translate absOffset) $ Lubeck.Drawing.textWithOptions $ mempty
      {
      Lubeck.Drawing.textAnchor       = style^.labelTextAnchor
      -- TODO read family from style
      , Lubeck.Drawing.fontFamily     = style^.labelTextFontFamily
      , Lubeck.Drawing.fontStyle      = style^.labelTextFontStyle
      , Lubeck.Drawing.fontSize       = First $ Just $ (toStr $ style^.labelTextFontSizePx) <> "px"
      , Lubeck.Drawing.fontWeight     = style^.labelTextFontWeight
      , Lubeck.Drawing.textSelectable = All False
      }
      where
        absOffset = style^.labelTextAbsOffset




-- | Draw ticks.
--
-- Same as 'ticksNoFilter' with a sanity check to remove ticks outside of quadrant.
ticks
  :: (Monad m, MonadReader Styling m)
  => [(Double, Maybe Str)] -- ^ X axis ticks.
  -> [(Double, Maybe Str)] -- ^ Y axis ticks.
  -> m Drawing
ticks xt yt = ticksNoFilter (filterTicks xt) (filterTicks yt)
  where
    filterTicks :: [(Double, a)] -> [(Double, a)]
    filterTicks = filter (withinNormRange . fst)

    -- | Is a number within the normalized (UHQ) range?
    withinNormRange :: Double -> Bool
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
      :: (Monad m, MonadReader Styling m)
      => [(Double, Maybe Str)] -- ^ X axis ticks.
      -> [(Double, Maybe Str)] -- ^ Y axis ticks.
      -> m Drawing
    ticksNoFilter xTickList yTickList = do
      style <- ask

      -- TODO zoom: derive from zoom , with filtering
      let xPos = {-(style^.zoom._x) *-} (style^.renderingRectangle._x)
      let yPos = {-(style^.zoom._y) *-} (style^.renderingRectangle._y)

      -- Flipped!
      let xInsideLength = {-(style^.zoom._y) *-} (style^.renderingRectangle._y)
      let yInsideLength = {-(style^.zoom._x) *-} (style^.renderingRectangle._x)

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

      let xTicks = mconcat $ flip fmap xTickList $
              \(pos,str) -> translateX (pos * xPos) $ mconcat
                [ mempty
                -- Text
                , maybe mempty id $ fmap (translateY (tl * (-1.5)) . rotate (turn*xTickTurn) . textX style) $ str
                -- Outside quadrant (main) tick
                , strokeWidth widthFgB $ strokeColorA colFgB $ scale tl $ translateY (-0.5) verticalLine
                -- Inside quadrant (background) grid
                , if not drawBgX then mempty else
                    strokeWidth widthBgX $ strokeColorA colBgX $ scale xInsideLength $ translateY (0.5) verticalLine
                ]
      let yTicks = mconcat $ flip fmap yTickList $
              \(pos,str) -> translateY (pos * yPos) $ mconcat
                [ mempty
                -- Text
                , maybe mempty id $ fmap (translateX (tl * (-1.5)) . rotate (turn*yTickTurn) . textY style) $ str
                -- Outside quadrant (main) tick
                , strokeWidth widthFgB $ strokeColorA colFgB $ scale tl $ translateX (-0.5) horizontalLine
                -- Inside quadrant (background) grid
                , if not drawBgY then mempty else
                    strokeWidth widthBgY $ strokeColorA colBgY $ scale yInsideLength $ translateX (0.5) horizontalLine
                ]
      return $ mconcat [xTicks, yTicks]
      where
        textX = text_ fst fst
        textY = text_ snd snd
        text_ which which2 style = textWithOptions $ mempty
          { textAnchor        = style^.tickTextAnchor.to which
          , alignmentBaseline = style^.tickTextAlignmentBaseline.to which2
          -- TODO read family from style
          , fontFamily        = style^.tickTextFontFamily
          , fontStyle         = style^.tickTextFontStyle
          , fontSize          = First $ Just $ (toStr $ style^.tickTextFontSizePx) <> "px"
          , fontWeight        = style^.tickTextFontWeight
          , textSelectable    = All False
          }
        isTransparent color = abs (alphaChannel color) < 0.001

-- TODO zoom: OK, axis not affected by zoom
-- | Draw X and Y axis.
labeledAxis
  :: (Monad m, MonadReader Styling m)
  => Str -- ^ X axis label.
  -> Str -- ^ Y axis label.
  -> m Drawing
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
      { textAnchor     = TextAnchorMiddle
      , fontFamily     = style^.axisTextFontFamily
      , fontWeight     = style^.axisTextFontWeight
      , fontStyle      = style^.axisTextFontStyle
      , fontSize       = First $ Just $ (toStr $ style^.axisTextFontSizePx) <> "px"
      , textSelectable = All False
      }
