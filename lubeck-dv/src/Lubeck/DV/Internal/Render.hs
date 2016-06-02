
{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
  , QuasiQuotes
  , OverloadedStrings
  , TupleSections
  , TemplateHaskell
  , ConstraintKinds
  , FlexibleContexts
  , ScopedTypeVariables
  , BangPatterns
  #-}

module Lubeck.DV.Internal.Render
  where

import Prelude hiding (div)
import qualified Prelude
import Data.Bifunctor(bimap)

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
import Lubeck.Drawing.Transformation
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


{-
Take a point in the UHQ and transform it into its rendering position.
If the resulting position is outside the RR, return (Left p), otherwise return (Right p).

This is accomplished as follows:
  - Run the zoom affine transformation (if default zoom, this is the identity)
  - Optionally filter points that now falls outside the UHQ (i.e. when zooming in)
  - Run the linear transformation that defines the rendering rectangle (always a scaling)
-}
getRenderingPosition :: Styling -> P2 Double -> Either (P2 Double) (P2 Double)
getRenderingPosition styling x = bimapSame (transformPoint (scalingXY $ styling^.renderingRectangle)) $
  (\p@(P (V2 x y)) -> if withinNormRange x && withinNormRange y then Right p else Left p) $ transformPoint (styling^.zoom) x
  where
    bimapSame f = bimap f f
{-
Like getRenderingPosition for drawings.
Carries out transformation directly without filtering. This is usually not what we want.

TODO prove/assure that this is equal to getRenderingPosition/getRenderingPositionT modulo filtering
-}
getRenderingPositionD :: Styling -> Drawing -> Drawing
getRenderingPositionD styling x = transform (scalingXY $ styling^.renderingRectangle) $ transform (styling^.zoom) x

  where

    -- TODO actually use filtering
    filterTicks :: [(Double, a)] -> [(Double, a)]
    filterTicks = filter (withinNormRange . fst)

    -- | Is a number within the normalized (UHQ) range?
    withinNormRange :: Double -> Bool
    withinNormRange x = 0 <= x && x <= 1

{-|
Like getRenderingPositionD, but don't actually scale the image.

Effectively, the given image is treated as an image which "anchor point" at
(origin +^ v). The anchor is transformed into the rendering space (taking zoom
and rendering rectangle into account), and the image is then translated so that
its origin aligns with the transformed anchor.
-}
getRenderingPositionRel :: V2 Double -> Styling -> Drawing -> Drawing
getRenderingPositionRel v styling dr = case getRenderingPosition styling (origin .+^ v) of
  Left _ -> mempty
  Right p -> translate (p .-. origin) dr
  where
    origin = P $ V2 0 0

data ScatterData = ScatterData
  { scatterDataColor :: Double
  }


mapFilterEitherBoth :: (Monad m, Alternative m) => (a -> Either b b) -> m a -> m b
mapFilterEitherBoth f = fmap (g . f)
  where
    g (Left x)  = x
    g (Right x) = x

mapFilterEither :: (Monad m, Alternative m) => (a -> Either t b) -> m a -> m b
mapFilterEither f = (=<<) (g . f)
  where
    g (Left _)  = empty
    g (Right x) = pure x

scatterData :: (Monad m, MonadReader Styling m, Monoid (m Drawing)) => ScatterData -> [P2 Double] -> m Drawing
scatterData (ScatterData colorN) ps = do
  style <- ask
  let base  = id
            $ fillColorA (style^.scatterPlotFillColor.to (`getColorFromPalette` colorN))
            $ strokeWidth (style^.scatterPlotStrokeWidth)
            $ strokeColorA (style^.scatterPlotStrokeColor.to (`getColorFromPalette` colorN))
            $ scale (style^.scatterPlotSize) circle
  return $ mconcat $ fmap (\p -> translate (relOrigin p) base) $ mapFilterEither (getRenderingPosition style) ps

scatterDataX :: (Monad m, MonadReader Styling m, Monoid (m Drawing)) => [P2 Double] -> m Drawing
scatterDataX ps = do
  style <- ask
  let base = strokeColorA (style^.scatterPlotStrokeColor.to paletteToColor) $ strokeWidth (style^.scatterPlotStrokeWidth) $ translateY 0.5 $ verticalLine
  return $ mconcat $ fmap (\p -> scaleY (style^.renderingRectangle._y) $ translateX (p^._x) base) $ mapFilterEither (getRenderingPosition style) ps

scatterDataY :: (Monad m, MonadReader Styling m, Monoid (m Drawing)) => [P2 Double] ->  m Drawing
scatterDataY ps = do
  style <- ask
  let base = strokeColorA (style^.scatterPlotStrokeColor.to paletteToColor) $ strokeWidth (style^.scatterPlotStrokeWidth) $ translateX 0.5 $ horizontalLine
  return $ mconcat $ fmap (\p -> scaleX (style^.renderingRectangle._x) $ translateY (p^._y) base) $ mapFilterEither (getRenderingPosition style) ps

data LineData = LineData
  { lineDataColor :: Double
  , lineDataShape :: Double
  }
defLineData = LineData 0 0

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

  return $ lineStyle $ ((either (translate . relOrigin) (translate . relOrigin) $ getRenderingPosition style p) :: Drawing -> Drawing) $
    ((segments $ betweenPoints $ mapFilterEitherBoth (getRenderingPosition style) (p:ps)) :: Drawing)

data AreaData = AreaData
  { areaDataColor :: Double
  }

fillData :: (Monad m, MonadReader Styling m, Monoid (m Drawing)) => AreaData -> [P2 Double] -> m Drawing
fillData _ []     = mempty
fillData _ [_]    = mempty
fillData (AreaData colorN) (p:ps) = do
  style <- ask
  let lineStyle = id
                . fillColorA    (style^.linePlotFillColor.to (`getColorFromPalette` colorN))
  return $ (either (translate . relOrigin) (translate . relOrigin) $ getRenderingPosition style pProjX)
    $ lineStyle $ segments $ betweenPoints $ mapFilterEitherBoth (getRenderingPosition style) $ addExtraPoints (p:ps)
  where
    -- Because of projection (below!), ignore y value for 1st point
    pProjX :: P2 Double
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

areaData' :: (Monad m, MonadReader Styling m, Monoid (m Drawing)) => AreaData -> [P2 Double] -> m Drawing
areaData' _ []     = mempty
areaData' _ [_]    = mempty
areaData' (AreaData colorN) (p:ps) = do
  style <- ask
  let lineStyle = fillColorA (style^.linePlotFillColor.to (`getColorFromPalette` colorN))
  return $ (either (translate . relOrigin) (translate . relOrigin) $ getRenderingPosition style p)
    $ lineStyle $ segments $ betweenPoints $ mapFilterEitherBoth (getRenderingPosition style) (p:ps)

-- | Draw a one-dimensional bar graph.
--
-- For grouped/stacked charts, see `barData2`, `barData3` etc.
barData :: (Monad m, MonadReader Styling m) => [P1 Double] -> m Drawing
barData xs = do
  style <- ask
  case style^.barPlotOrientation of
    Vertical   -> barDataV xs
    Horizontal -> barDataH xs


barDataV :: (Monad m, MonadReader Styling m) => [P1 Double] -> m Drawing
barDataV ps = do
  style <- ask
  let barWidth = 1/fromIntegral (length ps + 1)
  let barFullOffset = barWidth + barWidth * (style^.barPlotUngroupedOffset._x)
  let base = alignB $ fillColorA (style^.barPlotBarColors.to paletteToColor) $ square
  return $ scaleX (2/3) $ scaleRR style $ mconcat $ zipWith (\n -> translateX (n * barFullOffset)) [1..] $
    fmap (\(P (V1 v)) -> scaleX barWidth $ scaleY v $ base) ps
  where
    alignB = translate (V2 0 0.5)
    scaleRR :: Styling -> Drawing -> Drawing
    scaleRR = getRenderingPositionD

barDataH :: (Monad m, MonadReader Styling m) => [P1 Double] -> m Drawing
barDataH ps = do
  style <- ask
  let barWidth = 1/fromIntegral (length ps + 1)
  let barFullOffset = barWidth + barWidth * (style^.barPlotUngroupedOffset._x)
  let base = alignL $ fillColorA ({-style^.barPlotBarColors.to paletteToColor-}Colors.green `withOpacity` 1) $ square
  return $ scaleY (2/3) $ scaleRR style $ mconcat $ zipWith (\n -> translateY (n * barFullOffset)) [1..] $
    fmap (\(P (V1 v)) -> scaleY barWidth $ scaleX v $ base) ps
  where
    alignL = translate (V2 0.5 0)
    scaleRR :: Styling -> Drawing -> Drawing
    scaleRR = getRenderingPositionD


{-
TODO unify/generalize bar pltos
-}

baseImage :: (Monad m, MonadReader Styling m) => Drawing -> Double -> Double -> Maybe Double -> m Drawing
baseImage dr x y Nothing     = baseImage dr x y (Just 1)
baseImage dr x y (Just size) = do
  style <- ask
  return $ getRenderingPositionRel (V2 x y) style
    $ Lubeck.Drawing.scale size
    $ dr

baseLabel :: MonadReader Styling m => Double -> Double -> Str -> m Drawing
baseLabel x y str = do
  style <- ask
  return $ getRenderingPositionRel (V2 x y) style
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
ticks xTickList1 yTickList1 = do
  style <- ask
  -- TODO general orientation

  let !xTickList = case style^.barPlotOrientation of
            Vertical   -> xTickList1
            Horizontal -> yTickList1
  let !yTickList = case style^.barPlotOrientation of
            Vertical   -> yTickList1
            Horizontal -> xTickList1

  -- TODO zoom: derive from zoom , with filtering
  -- let (xPos :: Double) = {-(style^.zoom._x) *-} (style^.renderingRectangle._x)
  -- let (yPos :: Double) = {-(style^.zoom._y) *-} (style^.renderingRectangle._y)

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
          \(pos,str) ->
            maybe mempty translate (getRenderingPositionX style pos) $ mconcat
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
          \(pos,str) ->
            maybe mempty translate (getRenderingPositionY style pos) $ mconcat
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

    getRenderingPositionX, getRenderingPositionY :: Styling -> Double -> Maybe (V2 Double)
    fooX styling = fmap (transformPoint (scalingXY $ styling^.renderingRectangle)) . filterPointOutsideUHQ . transformPoint (xComponent $ styling^.zoom)
    fooY styling = fmap (transformPoint (scalingXY $ styling^.renderingRectangle)) . filterPointOutsideUHQ . transformPoint (yComponent $ styling^.zoom)
    getRenderingPositionX st x = (.-. origin) <$> fooX st (P $ V2 x 0)
    getRenderingPositionY st y = (.-. origin) <$> fooY st (P $ V2 0 y)

    xComponent :: Num a => T2 a -> T2 a
    xComponent = rectToTransf . (_bottom .~ 0) . (_top .~ 1) . transfToRect

    yComponent :: Num a => T2 a -> T2 a
    yComponent = rectToTransf . (_left .~ 0) . (_top .~ 1) . transfToRect

    filterPointOutsideUHQ p@(P (V2 x y))
      | (0 <= x && x <= 1) && (0 <= y && y <= 1) = Just p
      | otherwise = Nothing

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


{-
TODO finish/fix filtering
- Tick fitlering needs a new version of getRenderingPositionX
- Axes are OK
- Data could probably be handled by an SVG clip, see https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Clipping_and_masking
  - Lines and areas need mask, otherwise we're OK
-}

relOrigin :: (Num n, Num (v n), Additive v) => Point v n -> v n
relOrigin p = p .-. 0
{-# INLINE relOrigin #-}

-- filterTicks :: [(Double, a)] -> [(Double, a)]
-- filterTicks = filter (withinNormRange . fst)

-- | Is a number within the normalized (UHQ) range?
withinNormRange :: Double -> Bool
withinNormRange x = (0-0.001) <= x && x <= (1+0.001)
