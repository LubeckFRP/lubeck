
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

scatterData2 :: (Monad m, MonadReader Styling m, Monoid (m Drawing)) => [ScatterData2] -> m Drawing
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

scatterDataX :: (Monad m, MonadReader Styling m, Monoid (m Drawing)) => [P2 Double] -> m Drawing
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
  let mPrec = style^.precision
  let lineStyle = id
                . strokeColorA  (style^.linePlotStrokeColor.to (`getColorFromPalette` colorN))
                . fillColorA    (Colors.black `withOpacity` 0) -- transparent
                . strokeWidth   (style^.linePlotStrokeWidth)
                . dash          (style^.linePlotStroke. to (`extractLineStyle` dashN))

  return $ maskRenderingRectangle style $ lineStyle $ ((either (translate . relOrigin) (translate . relOrigin) $ getRenderingPosition style p) :: Drawing -> Drawing) $
    ((segments $ betweenPoints $ mapFilterEitherBoth (getRenderingPosition style) (p:mFilterToPrecision mPrec ps)) :: Drawing)
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

fillData :: (Monad m, MonadReader Styling m, Monoid (m Drawing)) => AreaData -> [P2 Double] -> m Drawing
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

  return $ maskRenderingRectangle style $ (either (translate . relOrigin) (translate . relOrigin) $ getRenderingPosition style p)
    $ lineStyle $ segments $ betweenPoints $ mapFilterEitherBoth (getRenderingPosition style) (p:ps)

-- | Draw a one-dimensional bar graph.
--
-- For grouped/stacked charts, see `barData2`, `barData3` etc.
barData :: (Monad m, MonadReader Styling m) => [P1 Double] -> m Drawing
barData xs = do
  style <- ask
  barDataHV (style^.barPlotOrientation) xs

barDataV :: (Monad m, MonadReader Styling m) => [P1 Double] -> m Drawing
barDataV = barDataHV Vertical

barDataH :: (Monad m, MonadReader Styling m) => [P1 Double] -> m Drawing
barDataH = barDataHV Horizontal

{-
Note:
  The naming convention for the orientation style attributes all assume vertical plots (the default).

  So things like width/offset._x etc will make sense when rendering verticals need to be transposed when renderign horizontal.

  For example the space between two bars in a non-grouped/stacked plot is always the scalar 'barPlotUngroupedOffset._x',
  which may be rendered horizontally or vertically and the space "thickness" of a bar is always referred to as its "barPlotWidth".


-}
barDataHV :: (Monad m, MonadReader Styling m) => VerticalHorizontal -> [P1 Double] -> m Drawing
barDataHV hv ps = do
  style <- ask
  let barWidth      :: Double             = 1/fromIntegral (length ps + 1)
  let barFullOffset :: Double             = barWidth + barWidth * (style^.barPlotUngroupedOffset._x)
  let hsState       :: IntMap HoverSelect           = style^.hoverSelectStates
  let hsSink        :: Sink (HoverSelectUpdate Int) = style^.hoverSelectEvents

  -- TODO unify approach to n/pred n
  let base n maybeHS = alignHV
            $ fillColorA (style^.barPlotBarColor.to (paletteToColor . flip getInteractivePalette (foo1 maybeHS)))
            $ addBasicHandlers (handleInteraction hsSink (pred n))
            $ square
  return $ scaleHV (2/3) $ scaleRR style $ mconcat
    $ zipWith (\n x -> translateHV (realToFrac n * barFullOffset) (foo barWidth (base n (IntMap.lookup (pred n) hsState)) x))
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

    foo1 Nothing  = NoHoverSelect
    foo1 (Just x) = x

    foo barWidth base = (\(P (V1 v)) -> scaleHV barWidth $ scaleHVInv v $ base)
    -- Either of these works (first version more efficient):
    -- alignB = translate (V2 0 0.5)
    -- alignL = translate (V2 0.5 0)
    alignB = align B
    alignL = align L

    alignHV = case hv of { Vertical -> alignB ; Horizontal -> alignL }

    scaleHV     = case hv of { Vertical -> scaleX ; Horizontal -> scaleY }
    scaleHVInv  = case hv of { Vertical -> scaleY ; Horizontal -> scaleX }
    translateHV = case hv of { Vertical -> translateX ; Horizontal -> translateY }

    -- Reads (renderingRectangle, zoom) from the style
    scaleRR :: Styling -> Drawing -> Drawing
    scaleRR = getRenderingPositionD

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


{-| Similar 'getRenderingPosition' for Drawings. Transforming a drawing is
conceptually the same as transformting every point inside it using
'getRenderingPosition'. However, this function ignores the filtering peformed by
'getRenderingPosition', so the resulting image might fall partially, or
completely outside the rendering rectangle.

TODO prove/assure that this is equal to
getRenderingPosition/getRenderingPositionT modulo filtering -}
getRenderingPositionD :: Styling -> Drawing -> Drawing
getRenderingPositionD styling x = transform (scalingXY $ styling^.renderingRectangle) $ transform (styling^.zoom) x


{-|
Like 'getRenderingPositionD', but don't actually scale the image.

Effectively, the given image is treated as an image which "anchor point" at
(origin +^ v). The anchor is transformed into the rendering space (taking zoom
and rendering rectangle into account), and the image is then translated so that
its origin aligns with the transformed anchor.

This is useful for things like labels and text. -}
getRenderingPositionRel :: V2 Double -> Styling -> Drawing -> Drawing
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

maskRenderingRectangle :: Styling -> Drawing -> Drawing
maskRenderingRectangle style = mask $
  transform (scalingXY $ style^.renderingRectangle)
    -- Nice color in case we want to debug the mask
    $ fillColorA (Colors.orange `withOpacity` 0.5)
    $ align BL square






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


addBasicHandlers :: (MouseEv -> IO ()) -> Drawing -> Drawing
addBasicHandlers mouseS dr = ( id
   . addHandler "mouseover" (\ev -> mouseS MouseOver)
   . addHandler "mouseout"  (\ev -> mouseS MouseOut)
   . addHandler "mouseup"   (\ev -> mouseS MouseUp)
   . addHandler "mousedown" (\ev -> mouseS MouseDown)
   . addHandler "mousemove" (\ev -> mouseS MouseMovedInside)
   . addHandler "dblclick"  (\ev -> mouseS MouseDoubleClick)
   ) dr

{-
Most general way of creating a drawing with mosue interaction.
Only event handlers sent to the given drawing a are processed (i.e. transparent areas are ignored).
-}
withMousePositionState :: (Signal MouseState -> Signal Drawing) -> FRP (Signal Drawing, Signal MouseState, Events MouseEv)
withMousePositionState drawingF = do
  (mouseS,    mouseE    :: Events MouseEv)     <- newEvent
  state :: Signal MouseState <- accumS mempty (fmap (flip patch) mouseE)
  let (dWithHandlers :: Signal MouseState -> Signal Drawing) = (fmap . fmap)
                           ( id
                           . addHandler "mouseover" (\ev -> mouseS MouseOver)
                           . addHandler "mouseout"  (\ev -> mouseS MouseOut)
                           . addHandler "mouseup"   (\ev -> mouseS MouseUp)
                           . addHandler "mousedown" (\ev -> mouseS MouseDown)
                           . addHandler "mousemove" (\ev -> mouseS MouseMovedInside)
                           . addHandler "dblclick"  (\ev -> mouseS MouseDoubleClick)
                           ) drawingF
  pure $ (dWithHandlers state, state, mouseE)
