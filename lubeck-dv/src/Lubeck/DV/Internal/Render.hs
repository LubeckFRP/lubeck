
{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
  , QuasiQuotes
  , OverloadedStrings
  , TupleSections
  , ConstraintKinds
  , FlexibleContexts
  , ScopedTypeVariables
  , RankNTypes
  , TypeFamilies
  , BangPatterns
  #-}


{-# OPTIONS_GHC
  -fno-warn-name-shadowing
  -fwarn-incomplete-patterns
  -Werror
  #-}

module Lubeck.DV.Internal.Render
  where

import Prelude hiding (div)
import qualified Prelude
import Data.Bifunctor(bimap)
import Debug.Trace

import Control.Applicative
import Control.Lens (to, Lens', view, over, set, lens)
import Control.Lens.Operators
import Control.Lens.TH (makeLenses)
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Colour (Colour, AlphaColour, withOpacity, blend, alphaChannel)
import Data.Monoid
import Data.Map(Map)
import Data.IntMap(IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Colour.Names as Colors

import Linear.Vector
import Linear.Affine hiding (Diff)
import Linear.V0
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4

import Lubeck.Str
import Lubeck.FRP
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

data Shape = Circle | Triangle | Square | Cross | XSquare | Star
  deriving (Enum, Bounded, Eq, Ord, Show)

data ScatterData2 = ScatterData2
  { scatterDataColor2 :: Double
  , scatterDataStrokeColor2 :: Double
  , scatterDataFillColor2 :: Double
  , scatterDataPoint2 :: P2 Double
  , scatterDataSize2 :: Double
  , scatterDataShape2 :: Shape
  }
scatterData2Point :: Lens' ScatterData2 (P2 Double)
scatterData2Point = lens scatterDataPoint2 (\s b -> s {scatterDataPoint2 = b})

scatterData2 :: (HasColors a, HasLines a) => (Monad m, MonadReader Styling m, Monoid (m (Draft a))) => [ScatterData2] -> m (Draft a)
scatterData2 ps = do
  style <- ask
  let base pp = id
            $ scale (style^.scatterPlotSize * scatterDataSize2 pp)
            $ shape pp
  return $ mconcat $ fmap
    (\pp ->
      translate (relOrigin (scatterDataPoint2 pp))
      (addStyling pp style (base pp))
      )
    $ mapFilterEither (getRenderingPosition2 scatterData2Point style) ps
  where
    shape pp = case scatterDataShape2 pp of
      Circle    -> circle
      Triangle  -> triangle
      Square    -> square
      Cross     -> rotate (-1/8) verticalLine <> rotate (1/8) verticalLine
      XSquare   -> rotate (-1/8) verticalLine <> rotate (1/8) verticalLine <> square
      Star      -> mconcat $ fmap (\r -> rotate r verticalLine) [0,1/8..7/8]
    addStyling pp style x = id
      $ fillColorA (style^.scatterPlotFillColor.to (`getColorFromPalette` (scatterDataFillColor2 pp)))
      $ strokeColorA (style^.scatterPlotStrokeColor.to (`getColorFromPalette` (scatterDataStrokeColor2 pp)))
      $ strokeWidth (style^.scatterPlotStrokeWidth)
      $ x

scatterDataX :: (HasColors a, HasLines a, Monad m, MonadReader Styling m, Monoid (m (Draft a))) => [P2 Double] -> m (Draft a)
scatterDataX ps = do
  style <- ask
  -- let base = strokeColorA (style^.scatterPlotStrokeColor.to paletteToColor) $ strokeWidth (style^.scatterPlotStrokeWidth) $ translateY 0.5 $ verticalLine
  let base  = id
            $ addStyling style
            $ translateY 0.5
            $ verticalLine
  return $ mconcat $ fmap (\p -> scaleY (style^.renderingRectangle._y) $ translateX (p^._x) base) $ mapFilterEither (getRenderingPosition style) ps
  where
    addStyling style x = id
      $ strokeColorA (style^.scatterPlotStrokeColor.to paletteToColor)
      $ strokeWidth (style^.scatterPlotStrokeWidth)
      $ x

scatterDataY :: (HasColors a, HasLines a, Monad m, MonadReader Styling m, Monoid (m (Draft a))) => [P2 Double] ->  m (Draft a)
scatterDataY ps = do
  style <- ask
  let base = strokeColorA (style^.scatterPlotStrokeColor.to paletteToColor) $ strokeWidth (style^.scatterPlotStrokeWidth) $ translateX 0.5 $ horizontalLine
  return $ mconcat $ fmap (\p -> scaleX (style^.renderingRectangle._x) $ translateY (p^._y) base) $ mapFilterEither (getRenderingPosition style) ps

data LineData = LineData
  { lineDataColor :: Double
  , lineDataShape :: Double
  }
defLineData = LineData 0 0

lineData :: (HasColors a, HasLines a, Monad m, MonadReader Styling m, Monoid (m (Draft a))) => LineData -> [P2 Double] -> m (Draft a)
lineData _ []     = mempty
lineData _ [_]    = mempty
lineData (LineData colorN dashN) (p:ps) = do
  style <- ask
  let mPrec = style^.precision
  let lineStyle = id
                . strokeColorA  (style^.linePlotStrokeColor.to (`getColorFromPalette` colorN))
                . fillColorA    (Colors.black `withOpacity` 0) -- transparent
                . strokeWidth   (style^.linePlotStrokeWidth)
                . dash          (style^.linePlotStroke. to (`extractLineStyle` dashN))

  return $ maskRenderingRectangle style $ lineStyle $ ((either (translate . relOrigin) (translate . relOrigin) $ getRenderingPosition style p) {-:: (Draft a) -> (Draft a) -}) $
    ((segments $ betweenPoints $ mapFilterEitherBoth (getRenderingPosition style) (p:mFilterToPrecision mPrec ps)) {- :: (Draft a) -} )
  -- TODO this should really filter out all points outside data set (except the 2 closest to RR, to get correct slope), and *then* apply precision

mFilterToPrecision :: Maybe Int -> [a] -> [a]
mFilterToPrecision Nothing xs  = xs
mFilterToPrecision (Just n) xs = filterToPrecision n xs

filterToPrecision :: Int -> [a] -> [a]
filterToPrecision n xs = takeEvery n' xs
  where
    -- takeEvery 2 [1..] => [1,3,5..]
    takeEvery :: Int -> [a] -> [a]
    takeEvery _ []     = []
    takeEvery n (x:xs) = x : drop (n-1) (takeEvery n xs)

    n' = ceiling $ toDouble (length xs) / toDouble n

    toDouble :: Int -> Double
    toDouble = fromIntegral


data AreaData = AreaData
  { areaDataColor :: Double
  }

fillData :: (HasColors a, Monad m, MonadReader Styling m, Monoid (m (Draft a))) => AreaData -> [P2 Double] -> m (Draft a)
fillData _ []     = mempty
fillData _ [_]    = mempty
fillData (AreaData colorN) (p:ps) = do
  style <- ask
  let lineStyle = id
                . fillColorA    (style^.linePlotFillColor.to (`getColorFromPalette` colorN))

  return $ maskRenderingRectangle style $ (either (translate . relOrigin) (translate . relOrigin) $ getRenderingPosition style pProjX)
    $ lineStyle $ segments $ betweenPoints $ mapFilterEitherBoth (getRenderingPosition style) $ addExtraPoints (p:ps)
  where
    -- Because of projection (below!), ignore y value for 1st point
    pProjX :: P2 Double
    pProjX = P (V2 firstPointX 0) where P (V2 firstPointX _) = p

    -- Add points from first and last projected on the X axis to make sure space below line is completely filled.
    addExtraPoints ps = [proj $ head ps] ++ ps ++ [proj $ last ps]
      where
        proj (P (V2 x _)) = P (V2 x 0)

areaData :: (HasColors a, HasLines a, Monad m, MonadReader Styling m, Monoid (m (Draft a))) => AreaData -> [P3 Double] -> m (Draft a)
areaData i ps = areaData' i $
  fmap (\p -> P $ V2 (p^._x) (p^._z)) ps
    <>
  fmap (\p -> P $ V2 (p^._x) (p^._y)) (reverse ps)

areaData' :: (HasColors a, HasLines a, Monad m, MonadReader Styling m, Monoid (m (Draft a))) => AreaData -> [P2 Double] -> m (Draft a)
areaData' _ []     = mempty
areaData' _ [_]    = mempty
areaData' (AreaData colorN) (p:ps) = do
  style <- ask
  let lineStyle = fillColorA (style^.linePlotFillColor.to (`getColorFromPalette` colorN))

  return $ maskRenderingRectangle style $ (either (translate . relOrigin) (translate . relOrigin) $ getRenderingPosition style p)
    $ lineStyle $ segments $ betweenPoints $ mapFilterEitherBoth (getRenderingPosition style) (p:ps)

-- data CircularData = CircularData
--   { circularDataWeight :: Double --i.e. relative size
--   }

-- TODO pie vs. donut
circularData :: (HasColors a, HasLines a, Monad m, MonadReader Styling m) => [Double] -> m (Draft a)
circularData ps = do
  style <- ask
  pure
    $ transform (scaleToRR style)
    $ mconcat (zipWith strokeColorA (colorList style) shapes)
  -- barDataHV (style^.barPlotOrientation) xs
  where
    -- To keep this a circle, we scale by (min X Y) of the rendering rectangle
    --
    -- It is placed in the middle of the RR (at the origin would make more sense,
    -- but this will make it an out of the box replacement for bar charts in most
    -- contexts)
    --
    -- Zoom is ignored
    scaleToRR :: Styling -> T2 Double
    scaleToRR st = translation (V2 dx dy) <> scaling r <> scaling (1/2)
      where
        dx = (st^.renderingRectangle._x) / 2
        dy = (st^.renderingRectangle._y) / 2
        r  = min (st^.renderingRectangle._x) (st^.renderingRectangle._y)


    -- TODO it is not clear how a circular chart would listen to the color aesthetic
    -- as we must also ensure colors are unique.
    --
    -- For now, just use default color values (from the 'circularPlotColor palette).
    colorList st = fmap (getColorFromPalette $ st^.circularPlotColor) [0..]

    -- | Given a list of proportions, split the range [0..1] into the subranges of
    -- the same relative size.
    --
    -- > intoRanges [1,2]
    -- [(0.0,0.3333333333333333),(0.3333333333333333,1.0)]
    -- > intoRanges [1,2,1]
    -- [(0.0,0.25),(0.25,0.75),(0.75,1.0)]
    intoRanges xs = let rs = scanl (+) 0 (fmap (/ sum xs) xs) in zip rs (tail rs)
    -- rangeToScaleOffset (a1,a2) = (a2-a1,a1)

    -- (scale,offset) values
    plotData :: [(Double, Double)]
    -- plotData = [(0.0,0.25),(0.25,0.75),(0.75,1.0)]
    plotData = intoRanges $ ps

    angleFromTurns :: Double -> Angle Double
    angleFromTurns x = realToFrac x * turn

    -- shapes :: [(Draft a)]
    shapes = fmap (\(s,o) -> fillColorA transparent $
      circleSector (angleFromTurns s) (angleFromTurns o)) plotData

    transparent = Colors.black `withOpacity` 0

-- | Draw a one-dimensional bar graph.
--
-- For grouped/stacked charts, see `barData2`, `barData3` etc.
barData :: (HasLines a, HasColors a, Monad m, MonadReader Styling m) => [P1 Double] -> m (Draft a)
barData xs = do
  style <- ask
  barDataHV (style^.barPlotOrientation) xs

barDataV :: (HasLines a, HasColors a, Monad m, MonadReader Styling m) => [P1 Double] -> m (Draft a)
barDataV = barDataHV Vertical

barDataH :: (HasLines a, HasColors a, Monad m, MonadReader Styling m) => [P1 Double] -> m (Draft a)
barDataH = barDataHV Horizontal

{-
NOTE
  Better naming convention:

          | Vertical      Horizontal
    Along | Y             X
    Cross | X             Y
    Home  | Bottom        Left
    Away  | Top           Right

NOTE
  How are bars positioned (in RR, assuming horizontal conventions)?
  Y positions are simply (0..y)
  For X positions, we rely on the conventions from the categorical scales to render at [1..]

  So for n elements, our bars barWidthwill be placed at [1..n].
  The width of the whole plot is 1, so the distance between plots will be (1/(n + 1)), adding an extra space between the last element and the rightmost edge.

NOTE
  There are various ways of handling color in a bar plot.
  For now we either
    - Pick the default value in the palette
    - Use a separate color for each bar

TODO prove that width = 1, regardless of the value of barPlotUngroupedOffset

-}
barDataHV :: (HasColors a, HasLines a, Monad m, MonadReader Styling m) => VerticalHorizontal -> [P1 Double] -> m (Draft a)
barDataHV hv ps = do
  style <- ask

  let barWidth      :: Double             = 1 / fromIntegral (length ps + 1)
  let scaleUp       :: Double             = 1 + style^.barPlotUngroupedOffset
  let barFullOffset :: Double             = barWidth * scaleUp

  let hsState       :: IntMap HoverSelect           = style^.hoverSelectStates
  let hsSink        :: Sink (HoverSelectUpdate Int) = style^.hoverSelectEvents

  -- This is a square with correct color and handlers attached
  -- TODO render colors from palette?
  let base n maybeHS = alignHome
            $ fillColorA (colorForIndexVal (style^.barPlotRotateColors) (style^.barPlotBarColor) maybeHS (fromIntegral n))
            $ addBasicHandlers (handleInteraction hsSink (pred n))
            $ square

  return $ scaleCross (recip $ scaleUp) $ scaleRR style $ mconcat
    $ zipWith (\n x -> translateCross (realToFrac n * barFullOffset)
        (scaleCrossAlong barWidth x (base n (IntMap.lookup (pred n) hsState))))
      [1..]
      ps
  where
    handleInteraction u n mouseEv = case mouseEv of
      MouseOver         -> u $ HoverSelectMouseOver n
      MouseOut          -> u $ HoverSelectMouseOut n
      MouseDown         -> u $ HoverSelectMouseDown n
      MouseUp           -> u $ HoverSelectMouseUp n
      MouseMovedInside  -> u $ HoverSelectMouseMovedInside n
      _ -> return ()

    colorForIndexVal :: Any -> InteractivePalette Double -> Maybe HoverSelect -> Double -> AlphaColour Double
    colorForIndexVal rotateColors color maybeHS n =
        getInteractivePalette color (defHoverSelect maybeHS)
          `getColorFromPalette`
        (if getAny rotateColors then n else 0)

    defHoverSelect :: Maybe HoverSelect -> HoverSelect
    defHoverSelect Nothing  = NoHoverSelect
    defHoverSelect (Just x) = x

    -- scaleCrossAlong :: Double -> Point V1 Double -> (Draft a) -> (Draft a)
    scaleCrossAlong c (P (V1 a)) = scaleCross c . scaleAlong a

    alignB = translate (V2 0 0.5)
    alignL = translate (V2 0.5 0)
    -- The above is identical to
    --
    -- > alignB = align B
    -- > alignL = align L
    --
    -- for any shape whose envelope is a centered square

    -- Align to the bottom (if vertical/default)
    alignHome = case hv of { Vertical -> alignB ; Horizontal -> alignL }
    -- Scale X (if vertical/default)
    scaleCross     = case hv of { Vertical -> scaleX ; Horizontal -> scaleY }
    -- Scale Y (if vertical/default)
    scaleAlong  = case hv of { Vertical -> scaleY ; Horizontal -> scaleX }
    -- Translate X (if vertical/default)
    translateCross = case hv of { Vertical -> translateX ; Horizontal -> translateY }

    -- Reads (renderingRectangle, zoom) from the style
    -- scaleRR :: Styling -> (Draft a) -> (Draft a)
    scaleRR = getRenderingPositionD







baseImage :: (HasLines a, HasColors a, Monad m, MonadReader Styling m) => (Draft a) -> Double -> Double -> Maybe Double -> m (Draft a)
baseImage dr x y Nothing     = baseImage dr x y (Just 1)
baseImage dr x y (Just size) = do
  style <- ask
  return $ getRenderingPositionRel (V2 x y) style
    $ Lubeck.Drawing.scale size
    $ dr

baseLabel :: (HasText a, HasLines a, HasColors a) => MonadReader Styling m => Double -> Double -> Str -> m (Draft a)
baseLabel x y str = do
  style <- ask
  return $ getRenderingPositionRel (V2 x y) style
    $ text_ style str
  where
    text_ style = fmap (Lubeck.Drawing.translate absOffset) $ Lubeck.Drawing.textWithOptions $ mempty
      {
      Lubeck.Drawing.textAnchor       = style^.labelTextAnchor
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
  :: (HasText a, HasLines a, HasColors a, Monad m, MonadReader Styling m)
  => [(Double, Maybe Str)] -- ^ X axis ticks.
  -> [(Double, Maybe Str)] -- ^ Y axis ticks.
  -> m (Draft a)
ticks xTickList1 yTickList1 = do
  style <- ask
  if getAny $ style^.noXY
  then
    pure mempty
  else do
    let !xTickList = case style^.barPlotOrientation of
              Vertical   -> xTickList1
              Horizontal -> yTickList1
    let !yTickList = case style^.barPlotOrientation of
              Vertical   -> yTickList1
              Horizontal -> xTickList1

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
              , maybe mempty id $ fmap (translateY (tl * (-1.5)) . rotate xTickTurn . textX style) $ str
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
              , maybe mempty id $ fmap (translateX (tl * (-1.5)) . rotate yTickTurn . textY style) $ str
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
  :: (HasLines a, HasColors a, HasText a, Monad m, MonadReader Styling m)
  => Str -- ^ X axis label.
  -> Str -- ^ Y axis label.
  -> m (Draft a)
labeledAxis labelX labelY = do
  style <- ask
  if getAny $ style^.noXY
  then
    pure mempty
  else do
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

-- Utility

{-| Take a point in the unit hypercube and return the corresponding rendering
position. If the resulting position falls outside the rendering rectangle,
returns (Left p), if it falls inside, return (Right p).

This is accomplished as follows:

- Run the zoom affine transformation (if default zoom, this is the identity)
- Optionally filter points that now falls outside the UHQ (i.e. when zooming in)
- Run the linear transformation that defines the rendering rectangle (always a
- scaling) -}
getRenderingPosition :: Styling -> P2 Double -> Either (P2 Double) (P2 Double)
getRenderingPosition styling x =
  bimapSame (transformPoint $ scalingXY $ styling^.renderingRectangle)
    $ insideUnitHyperCube $ transformPoint (styling^.zoom) x
  where
    insideUnitHyperCube p@(P (V2 x y)) =
      if withinNormRange x && withinNormRange y
        then Right p
        else Left p
    bimapSame f = bimap f f

{-|
Like getRenderingPosition but works for any value with a point.
-}
getRenderingPosition2 :: Lens' a (P2 Double) -> Styling -> a -> Either a a
getRenderingPosition2 l styling x =
  bimapSame (transformPointWithLens l (scalingXY $ styling^.renderingRectangle))
    $ insideUnitHyperCube $ transformPointWithLens l (styling^.zoom) x
  where
    insideUnitHyperCube p =
      if withinNormRange (view (l._x) p) && withinNormRange (view (l._y) p)
        then Right p
        else Left p
    bimapSame f = bimap f f
    transformPointWithLens l t x = over l (transformPoint t) x

{-| Similar to 'getRenderingPosition', but return 'empty' if the point falls
outside the rendering rectangle. -}
getRenderingPositionM :: (Monad m, Alternative m) => Styling -> m (P2 Double) -> m (P2 Double)
getRenderingPositionM style = mapFilterEither (getRenderingPosition style)


{-| Similar 'getRenderingPosition' for (Draft SVG)s. Transforming a drawing is
conceptually the same as transformting every point inside it using
'getRenderingPosition'. However, this function ignores the filtering peformed by
'getRenderingPosition', so the resulting image might fall partially, or
completely outside the rendering rectangle.

TODO prove/assure that this is equal to
getRenderingPosition/getRenderingPositionT modulo filtering -}
getRenderingPositionD :: Backend a => Styling -> (Draft a) -> (Draft a)
getRenderingPositionD styling x = transform (scalingXY $ styling^.renderingRectangle) $ transform (styling^.zoom) x


{-|
Like 'getRenderingPositionD', but don't actually scale the image.

Effectively, the given image is treated as an image which "anchor point" at
(origin +^ v). The anchor is transformed into the rendering space (taking zoom
and rendering rectangle into account), and the image is then translated so that
its origin aligns with the transformed anchor.

This is useful for things like labels and text. -}
getRenderingPositionRel :: Backend a => V2 Double -> Styling -> (Draft a) -> (Draft a)
getRenderingPositionRel v styling dr = case getRenderingPosition styling (origin .+^ v) of
  Left _ -> mempty
  Right p -> translate (p .-. origin) dr
  where
    origin = P $ V2 0 0

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

{- Given a point, return the vector from the origin to that point. -}
relOrigin :: (Num n, Num (v n), Additive v) => Point v n -> v n
relOrigin p = p .-. 0
{-# INLINE relOrigin #-}

-- | Is a number within the normalized (UHQ) range?
withinNormRange :: Double -> Bool
withinNormRange x = (0-0.001) <= x && x <= (1+0.001)

{-|
This is intended to assure that no pieces of data are accidentally rendered
outside the rendering rectangle. As masking is not yet implemented in Lubeck (Draft SVG)
it is a no-op.
-}
maskRenderingRectangle :: Styling -> (Draft a) -> (Draft a)
maskRenderingRectangle _ x = x






{-|
Diffable is AffineSpace, except the Diff type is just a monoid (rather than an additive group).
I.e. the vectors/patches can not be inverted.

@
  (.-.) ~ diff
  (.+^) ~ patch
@
-}
class Monoid (Diff p) => Diffable p where
  type Diff p :: *
  diff :: p -> p -> Diff p
  patch :: p -> Diff p -> p

data MouseEv
  = MouseNone -- TODO should we have this?
  | MouseUp
  | MouseDown
  | MouseOver
  | MouseOut
  | MouseMovedInside -- TODO should we have this?
  | MouseDoubleClick
  deriving (Enum, Eq, Ord, Show, Read)

instance Monoid MouseEv where
  mempty = MouseNone
  mappend x y = x -- last event to happen as

data MouseState = MouseState { mouseInside :: !Bool, mouseDown :: !Bool }
  deriving (Eq, Ord, Show, Read)

instance Monoid MouseState where
  mempty = MouseState False False -- how do we know this?
  mappend x y = x -- ?

instance Diffable MouseState where
  type Diff MouseState = MouseEv
  diff (MouseState False d1) (MouseState True d2)  = MouseOut
  diff (MouseState True  d1) (MouseState False d2) = MouseOver
  diff (MouseState i1 False) (MouseState i2 True)  = MouseUp
  diff (MouseState i1 True)  (MouseState i2 False) = MouseDown
  diff (MouseState i1 d1)    (MouseState i2 d2)
    | i1 == i2 && d1 == d2 = MouseMovedInside
    | otherwise            = MouseNone

  patch (MouseState inside down) MouseUp   = MouseState inside False
  patch (MouseState inside down) MouseDown = MouseState inside True
  patch (MouseState inside down) MouseOver = MouseState True   down

  -- If mouse goes while button is still pressed, release
  patch (MouseState inside down) MouseOut  = MouseState False  False

  -- If the mouse is moved, we must be inside
  patch (MouseState inside down) MouseMovedInside = MouseState True   down

  -- Double clicks events need not affect state, as they are always
  -- sent in conjunction with up/down events.
  patch x _ = x

-- FIXME support this legacy API in Fast backend (or better solution)
addBasicHandlers :: (MouseEv -> IO ()) -> (Draft a) -> (Draft a)
addBasicHandlers _ x = x
-- addBasicHandlers mouseS dr = ( id
--    . addHandler "mouseover" (\ev -> mouseS MouseOver)
--    . addHandler "mouseout"  (\ev -> mouseS MouseOut)
--    . addHandler "mouseup"   (\ev -> mouseS MouseUp)
--    . addHandler "mousedown" (\ev -> mouseS MouseDown)
--    . addHandler "mousemove" (\ev -> mouseS MouseMovedInside)
--    . addHandler "dblclick"  (\ev -> mouseS MouseDoubleClick)
--    ) dr
