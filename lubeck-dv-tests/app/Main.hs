
{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, ScopedTypeVariables, TypeFamilies
  , FlexibleContexts, BangPatterns, NamedFieldPuns, CPP #-}

{-# OPTIONS_GHC
  -fwarn-incomplete-patterns
  -fno-warn-name-shadowing
  -fno-warn-unused-binds
  -fno-warn-unused-matches
  -fno-warn-unused-imports
  -fno-warn-type-defaults
  -fno-warn-missing-signatures
  #-}

module Main where

import BasePrelude hiding ((|||), Signal, rotate)
import Control.Lens(to, _1, _2, (.~), view, (^.))
import Linear.Affine ((.+^))
import Data.Colour (withOpacity)
import Data.Colour.Names as Colors

-- Debug
import Debug.Trace
import Control.Concurrent(threadDelay)

import Lubeck.DV hiding (strokeColor)
import Lubeck.FRP
import Lubeck.Str
import Lubeck.Drawing.Transformation
import Lubeck.Drawing hiding (text, addProperty)

#ifdef __GHCJS__
import Lubeck.Forms(componentEvent, componentSignal)
import Lubeck.Forms.Button(multiButtonWidget)
import Lubeck.Forms.Select(selectEnumBoundedWidget)
import Lubeck.App(runAppReactive)
import Web.VirtualDom.Html(Html, h1, text)
import qualified Web.VirtualDom as VirtualDom
-- TODO separate/consolidate Html/Svg events
-- Currently they can be used more or less interchangebly

import Web.VirtualDom.Html.Events(Event(..), mousemove)

import           GHCJS.Foreign.Callback(syncCallback,Callback,OnBlocked(..))
#endif

-- TODO
import Data.Time

----------

debugHandlers = False

-- TODO handle onmousedown, onmousemove, onmouseout, onmouseup
-- Ref: http://www.petercollingridge.co.uk/interactive-svg-components/draggable-svg-element

-- From mouse events we can read various properties (clientX, screenX, movementX)
-- https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/movementX
-- Easiest would be to just use movementX!

#ifdef __GHCJS__

-- onE e n = VirtualDom.on n . contramapS e
--   where
--     contramapS f k x = k (f x)
--
-- mouseover  = onE Event "mouseover"
-- mouseout   = onE Event "mouseout"
-- -- Note: use over/out rather than enter/leave!
-- mouseenter = onE Event "mouseenter"
-- mouseleave = onE Event "mouseleave"
-- mouseup    = onE Event "mouseup"
-- mousedown  = onE Event "mousedown"
-- mousemove = onE Event "mousemove"


{-
Position types:
  screenX
    Global screen coordinates.
  clientX
    Relative browser window client area (i.e. where page is rendered), increasing TL to BR
  pageX
    Relative page, increasing TL to BR (compare clientX).
  offsetX
    "offset in between that event and the padding edge of the target node"
  movementX
     currentEvent.movementX = currentEvent.screenX - previousEvent.screenX

TODO
  There are some shims/libraries we could use for better performance
  http://interactjs.io/

-}
foreign import javascript unsafe "$1.movementX"
  movementX :: Event -> Double
foreign import javascript unsafe "$1.movementY"
  movementY :: Event -> Double
foreign import javascript unsafe "$1.screenX"
  screenX :: Event -> Double
foreign import javascript unsafe "$1.screenY"
  screenY :: Event -> Double
foreign import javascript unsafe "$1.offsetX"
  offsetX :: Event -> Double
foreign import javascript unsafe "$1.offsetY"
  offsetY :: Event -> Double
-- TODO right property to read?
#else
type Html = ()
data Event = Event ()
movementX :: Event -> Double
movementY :: Event -> Double
screenX :: Event -> Double
screenY :: Event -> Double
offsetX :: Event -> Double
offsetY :: Event -> Double
movementX = const 0
movementY = const 0
screenX = const 0
screenY = const 0
offsetX = const 0
offsetY = const 0
#endif

{-
TODO all initial arguments *could* be in S/B moinad
What is the most general interface?
-}

-- hoverable d = do
--   (overOut, overOuted :: Events Bool) <- newEvent
--   (state :: Signal Bool) <- stepperS False overOuted
--   let dWithHandlers = fmap ( addProperty (mouseover $ const $ overOut True)
--                            . addProperty (mouseout $ const $ overOut False)
--                            ) d
--   pure $ (fmap dWithHandlers state, state)


{-
Diffable is AffineSpace, except the Diff type is just a monoid (rather than an additive group).
I.e. the vectors/patches can not be inverted.

  (.-.) ~ diff
  (.+^) ~ patch

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

data MouseState = MouseState { mouseInside :: !Bool, mouseDown :: !Bool } deriving (Eq, Ord, Show, Read)

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



{-
TODO write down equations relating SVG-style positions to Lubeck.Drawing style positions properly!

Note: Properly, in (transform t (addHandler h x)), the handler should apply (-t) to all positions it recieves.
Thus when scaling/translating an image, interaction till works "as expected" (i.e. mouse drag rectangles still appear where the mouse is).
-}
compensatePosition :: RenderingOptions -> P2 Double -> P2 Double
compensatePosition (RenderingOptions {dimensions, originPlacement}) (P (V2 x y)) = case originPlacement of
    TopLeft     -> P $ V2 x (-y)
    Center      -> P $ V2 (x - dx/2) (-(y - dy/2))
    BottomLeft  -> P $ V2 x (-y)
  where
    P (V2 dx dy) = dimensions

-- = (dimensions rOpts^.dx) (dimensions rOpts^.dy) (P (V2 x y)) = P (V2 x (negate y))

-- TODO get this from rendering context instead of assuming mempty (below!)
compensatePosition' = compensatePosition mempty

{-
Most general way of creating a drawing with mosue interaction.

Mouse position is relative to the entire drawing area (TODO currently assumes origin is in TL corner).
Only event handlers sent to the given drawing a are processed (i.e. transparent areas are ignored).
-}
withMousePositionState :: (Signal MousePositionState -> Signal Drawing) -> FRP (Signal Drawing, Signal MousePositionState, Events MouseEv)
withMousePositionState drawingF = do
  (mouseS, mouseE :: Events MouseEv) <- newEvent
  (state :: Signal MouseState) <- accumS mempty (fmap (flip patch) mouseE)

  (mousePosS2, mousePosE :: Events (P2 Double)) <- newEvent
  let mousePosS = contramapSink compensatePosition' mousePosS2
  (pos :: Signal (P2 Double)) <- stepperS (P $ V2 0 0) (mousePosE)

  let (posState :: Signal MousePositionState) = liftA2 MousePositionState pos state
  let (dWithHandlers :: Signal MousePositionState -> Signal Drawing) = (fmap . fmap)
                           ( id
                           . addHandler "mouseover" (\ev -> let e = Event ev in mousePosS (P $ V2 (offsetX e) (offsetY e)) >> mouseS MouseOver)
                           . addHandler "mouseout"  (\ev -> let e = Event ev in mousePosS (P $ V2 (offsetX e) (offsetY e)) >> mouseS MouseOut)
                           . addHandler "mouseup"   (\ev -> let e = Event ev in mousePosS (P $ V2 (offsetX e) (offsetY e)) >> mouseS MouseUp)
                           . addHandler "mousedown" (\ev -> let e = Event ev in mousePosS (P $ V2 (offsetX e) (offsetY e)) >> mouseS MouseDown)
                           . addHandler "mousemove" (\ev -> let e = Event ev in mousePosS (P $ V2 (offsetX e) (offsetY e)) >> mouseS MouseMovedInside)
                           . addHandler "dblclick"  (\ev -> let e = Event ev in mousePosS (P $ V2 (offsetX e) (offsetY e))  >> mouseS MouseDoubleClick)
                           ) drawingF
  pure $ (dWithHandlers posState, posState, mouseE)


{-
Like withMousePositionState, optimized for non-recursive case.
-}
withMousePositionStateNR :: Signal Drawing -> FRP (Signal Drawing, Signal MousePositionState, Events MouseEv)
withMousePositionStateNR drawing = withMousePositionState (const drawing)

withMouseState :: (Signal MouseState -> Signal Drawing) -> FRP (Signal Drawing, Signal MouseState)
withMouseState d = fmap ((second $ fmap mouseState) . (\(a,b,c) -> (a,b))) $ withMousePositionState (d . fmap mouseState)

{-|
Like MouseState, but include position.
-}
data MousePositionState = MousePositionState
  { mousePosition :: !(P2 Double)
  , mouseState    :: !MouseState
  }
  deriving (Eq, Ord, Show)

-- TODO this function, 1) without actually moving the drawing, 2) relative the drawing in question
-- withMousePositionState :: Signal Drawing -> FRP (Signal Drawing, Signal MousePositionState)
-- withMousePositionState d = do
--   (d2, p) <- nudgeable d
--   (d3, ms) <- withMouseState (const d2)
--   pure $ (d3, liftA2 MousePositionState p ms)



hoverable :: (Signal Bool -> Signal Drawing) -> FRP (Signal Drawing, Signal Bool)
hoverable d = fmap (second $ fmap mouseInside) $ withMouseState (d . fmap mouseInside)

hoverable_ :: (Signal Bool -> Signal Drawing) -> FRP (Signal Drawing)
hoverable_ = fmap fst . hoverable

-- TODO implement in terms of (poss. opt version of withMouseState)
nudgeableDraggable :: Behavior Bool -> Signal Drawing -> FRP (Signal Drawing, Signal (P2 Double))
nudgeableDraggable allowMove d =  do
  (move, moved1 :: Events (V2 Double)) <- newEvent
  let moved = filterJust $ snapshotWith (\allow m -> if allow then Just m else Nothing) allowMove moved1
  (pos :: Signal (P2 Double)) <- accumS (P $ V2 0 0) (fmap (^+.) moved)

  let (dWithHandlers :: Signal Drawing) = fmap (addHandler "mousemove" $ handleMouseMove move) d

  pure $ ( liftA2 (\(P v) dWithHandlers' -> translate v dWithHandlers') pos dWithHandlers, pos )
  where
    (^+.) = flip (.+^)
    handleMouseMove dest ev1 = let ev = Event ev1 in
      dest $ V2 (movementX ev) (-movementY ev)

{-
Make something "nudgeable".
I.e. it responds to all mousemove events by moving in the same direction.
-}
nudgeable :: Signal Drawing -> FRP (Signal Drawing, Signal (P2 Double))
nudgeable = nudgeableDraggable (pure True)
-- Note: Could be optimized by copying nudgeableDraggable and removing the filtering altogether

nudgeable_ :: Signal Drawing -> FRP (Signal Drawing)
nudgeable_ = fmap fst . nudgeable

{-
Make something "draggable".

I.e. it responds to mousemove events by moving in the same direction iff the object is "grabbed".
An object becomes grabbed whenever it recieves a mousedown event and retains that property
until it recieves a mousedown or mouseup event.
-}
draggable :: Signal Drawing -> FRP (Signal Drawing, Signal (P2 Double))
draggable d = do
  (d2, mouseStateS :: Signal MouseState) <- withMouseState (const d)
  nudgeableDraggable (fmap mouseDown $ current $ mouseStateS) d2

draggable_ :: Signal Drawing -> FRP (Signal Drawing)
draggable_ = fmap fst . draggable



{-\
Represents one of a series of actions used to input a rectangle.

A rectangle is considered to have been described whenever:

- An EndRect event is emitted
- The most recent event, excepting all ContinueRect events, is a BeginRect event
  (and thus not an AbortRect) event.
-}
data RectInputEvent
  -- Begin creating a square from this point
  = BeginRect (P2 Double)
  -- Continue creating a square
  | ContinueRect (P2 Double)
  -- Finish creating square
  | EndRect (P2 Double)
  -- Abort current square in process
  | AbortRect
  deriving (Eq, Ord, Show)


{-|
A drag overlay interface.

Whenever the given bool signal is False, the resulting signal behaves like the empty image (i.e. 'mempty').

Whenever the given bool signal is True, this creates a in infinite transparent pane that absorbs mouse
events from whatever objects may be drawn below it. Drag gestures are captured and drawn as a setupEmit
transparent rectangle, to provide the user with feedback that a drag operation is taking place.

The resulting events provide a double-click events and a rectangle that represents the starting
and ending points of a complete drawing gesture. This is only emitted after the drawing gesture
is completed.

Note that in this rectangle @p2@ might be greater than @p1@, depending on the
direction of the gesture. If you don't care about direction (i.e. when) drag is
used for zoom, you can normalize the rectange with @normalizeRect@.

Returns @(view, (completed) dragged rectangles, double clicks)@.
-}
dragRect  :: Maybe (Signal Double) -- display fixed X
          -> Maybe (Signal Double) -- display fixed Y
          -> Signal Bool
          -> FRP (Signal Drawing, Events (Rect Double), Events ())
dragRect _ fixedY activeS = do

  ( bigTransparentWithHandlers, bigTransparentMPS :: Signal MousePositionState, bigTransparentME :: Events MouseEv )
          <- withMousePositionStateNR (pure bigTransparent)

    -- TODO test bad translation
  let overlay :: Signal Drawing = liftA2 (\bt s -> if s then bt else mempty) (bigTransparentWithHandlers) activeS
  (finishedRects, intermediateRects) <- emittedAndIntermediateRectangles bigTransparentMPS

  let (dragRectD :: Signal Drawing) = fmap dragRectPrim (case fixedY of
          Just fixedY' -> liftA2 (\x -> fmap (setYBounds 0 x)) fixedY' intermediateRects
          Nothing -> intermediateRects
          )

  pure $
          ( fmap (scaleX 1) $ overlay <> dragRectD
          , filterJust $ ignoreSmallishRectangles <$> finishedRects
          , filterJust $ getDoubleClicks          <$> bigTransparentME
          )
  where
    getDoubleClicks :: MouseEv -> Maybe ()
    getDoubleClicks MouseDoubleClick = Just ()
    getDoubleClicks _                = Nothing

    {-|
    This is necessary to prevent double clicks from being percieved as very small zoom requests.
    -}
    ignoreSmallishRectangles :: Rect Double -> Maybe (Rect Double)
    ignoreSmallishRectangles r
      | area r > 12 = Just r
      | otherwise   = Nothing
    area r = width r * height r
    width r  = abs $ r^._right - r^._left
    height r = abs $ r^._top - r^._bottom

    setYBounds :: Double -> Double -> Rect Double -> Rect Double
    setYBounds y1 y2 rect = _top .~ y2 $ _bottom .~ y1 $ rect

    dragRectPrim :: Maybe (Rect Double) -> Drawing
    dragRectPrim Nothing = mempty
    dragRectPrim (Just rect) =
      fillColorA (Colors.blue `withOpacity` 0.1) $ fitInsideRect rect $ square

    bigTransparent :: Drawing
    bigTransparent =
        fillColorA (Colors.green `withOpacity` 0) $ Lubeck.Drawing.scale 3000 square

    emitFinishedRects :: Events RectInputEvent -> FRP (Events (Rect Double))
    emitFinishedRects = fmap (filterJust . fmap snd) . foldpE f (Nothing, Nothing)
      where
        f (BeginRect p1)   _            = (Just p1, Nothing)
        f (EndRect p2)     (Just p1, _) = (Nothing, Just (Rect_ p1 p2))
        f AbortRect        _            = (Nothing, Nothing)
        f (ContinueRect _) (x, _)       = (x,       Nothing)

    emitIntermediateRects :: Events RectInputEvent -> FRP (Signal (Maybe (Rect Double)))
    emitIntermediateRects ri = do
      rs <- emitIntermediateRectsE ri
      stepperS Nothing rs
      where
        emitIntermediateRectsE :: Events RectInputEvent -> FRP (Events (Maybe (Rect Double)))
        emitIntermediateRectsE = fmap (fmap snd) . foldpE f (Nothing, Nothing)
          where
            f (BeginRect p1)   _             = (Just p1, Just (Rect_ p1 p1))
            f (ContinueRect p2) (Just p1, _) = (Just p1, Just (Rect_ p1 p2))
            f _ _ = (Nothing, Nothing)

    mpsToRectInput :: P2 Double -> MouseEv -> RectInputEvent
    mpsToRectInput pos MouseDown        = BeginRect pos
    mpsToRectInput pos MouseUp          = EndRect pos
    mpsToRectInput pos MouseOut         = AbortRect
    mpsToRectInput pos _                = ContinueRect pos
    -- mpsToRectInput pos MouseP

    emittedAndIntermediateRectangles :: Signal MousePositionState -> FRP (Events (Rect Double), Signal (Maybe (Rect Double)))
    emittedAndIntermediateRectangles bigTransparentMPS = do
      let (mouseMove :: Events MousePositionState) = (updates bigTransparentMPS)
      mouseMove2 <- withPreviousWith (\e1 e2 -> mpsToRectInput (mousePosition e2) (mouseState e2 `diff` mouseState e1)) mouseMove
      finishedRects <- emitFinishedRects mouseMove2
      intermediateRects <- emitIntermediateRects mouseMove2
      pure (finishedRects, intermediateRects)



#ifdef __GHCJS__
{-
Displays a pair or plus/minus-labeled buttons.
-}
plusMinus :: Str -> Double -> Events Double -> FRP (Signal Html, Signal Double)
plusMinus label init reset = do
  (view, alter :: Events (Double -> Double)) <- componentEvent
    id
    (multiButtonWidget [] [("-", (subtract 0.1)), ("+", (+ 0.1))])
    (fmap const reset)
  zoom <- accumS init alter
  pure (mconcat [pure $ text (toJSString label), view], zoom)

plusMinusI :: Str -> Int -> Events Int -> FRP (Signal Html, Signal Int)
plusMinusI label init reset = do
  (view, alter :: Events (Int -> Int)) <- componentEvent
    id
    (multiButtonWidget [] [("-", (subtract 1)), ("+", (+ 1))])
    (fmap const reset)
  zoom <- accumS init alter
  pure (mconcat [pure $ text (toJSString label), view], zoom)


onOff :: Str -> Bool -> FRP (Signal Html, Signal Bool)
onOff label init = do
  (view, alter :: Events Bool) <- componentEvent init selectEnumBoundedWidget mempty
  state <- stepperS init alter
  pure (mconcat [pure $ text (toJSString label), view], state)
-- selectEnumBoundedWidget :: (Eq a, Enum a, Bounded a, Show a) => Widget' a

rendRectAlts :: Str -> FRP (Signal Html, Signal (V2 Double))
rendRectAlts label = do
  (view, val) <- componentSignal (V2 400 300)
    (multiButtonWidget []
            [ ("(400x300)", V2 400 300)
            , ("(700x500)", V2 700 500)
            , ("(700x700)", V2 700 700)
            , ("(90x90)", V2 90 90)
            , ("(100x500)", V2 100 500)
            ])
    mempty
  pure (mconcat [pure $ text (toJSString label), view], val)

basicButton :: Str -> FRP (Signal Html, Events ())
basicButton label = do
  (view, val) <- componentSignal ()
    (multiButtonWidget []
            [ (toJSString label, ())
            ])
    mempty
  pure (mconcat [pure $ text (toJSString label), view], updates val)
#endif

showCurrentValue :: Show a => (Signal Html, Signal a) -> (Signal Html, Signal a)
showCurrentValue (vs,xs) = (liftA2 (\x v -> x <> text (toJSString $ toStr v)) vs xs,xs)

----------

type SDrawing  = Signal Drawing
type SRDrawing = Signal RDrawing

-- renderDrawingTrace msg d = trace msg $ renderDrawing mempty d
renderDrawingTrace msg d = renderDrawing mempty d


kSeries = zipWith (+)
  [81,32,17,84,45,52,91,26,76,38,58,20,42,71,97,26,69,62,42,17,99,37,42,75,57,94,9,33,27,8,21,28,12,71,69,99,76,99,37,92,82,7,85,1,65,93,68,44,23,2,37,60,21,16,2,7,58,14,39,57,87,61,90,29,88,27,93,14,99,25,27,84,61,56,25,63,63,10,37,15,21,28,37,59,8,75,42,70,88,89,44,52,72,8,62,77,94,60,67,60]
  (concatMap (replicate 10) (fmap (* 10) [1..]))


{-|
Normalize a rectangle to represent a "positive" transformation (i.e. one that only performs positive scaling
and translation to the left and upwards).
-}
normalizeRect :: Ord a => Rect a -> Rect a
normalizeRect (Rect_ (P (V2 x1 y1)) (P (V2 x2 y2))) = Rect_ (P (V2 xL yL)) (P (V2 xU yU))
  where
    !xL = x1 `min` x2
    !xU = x1 `max` x2
    !yL = y1 `min` y2
    !yU = y1 `max` y2


foldpS_ :: (a -> b -> b) -> b -> Events a -> FRP (Signal b)
foldpS_ f z s = accumS z (fmap f s)

{-|
Like foldpS, but allow destructively sending a new updated value.
-}
foldpSRestart :: (a -> b -> b) -> b -> Events a -> Events b -> FRP (Signal b)
foldpSRestart f z u r = accumS z ((fmap f u) <> fmap (const) r)


-- main :: IO ()
-- main = do
-- #ifdef __GHCJS__
--   setupEmit
--   setupEmit2
--   setupRender
--   setupRender2
--
--   -- retainMain
--   (rrV, rrS)          <- rendRectAlts "Rendering rectangle"
--   (asV, asS)          <- onOff "Auto-scale Y" True
--   (view0, zoomActive) <- onOff "Zoom active" True
--   (resetV, resetE)    <- basicButton "Reset zoom"
--   (rectU, rectE :: Events (Rect Double))      <- newEvent
--   (view1a, zoomXO)    <- showCurrentValue <$> plusMinus "Zoom X^1" 0 (view _left   <$> rectE)
--   (view2a, zoomYO)    <- showCurrentValue <$> plusMinus "Zoom Y^1" 0 (view _bottom <$> rectE)
--   (view1, zoomX)      <- showCurrentValue <$> plusMinus "Zoom X^2" 1 (view _right  <$> rectE)
--   (view2, zoomY)      <- showCurrentValue <$> plusMinus "Zoom Y^2" 1 (view _top    <$> rectE)
--   let (zoomXY :: Signal (Rect Double)) = liftA4 (\x y xo yo -> rect xo yo ({-xo+-}x) ({-yo+-}y)) zoomX zoomY zoomXO zoomYO
--   -- let zoomV = mconcat [resetV, rrV, view0, asV, view1a, view2a, view1, view2]
--   let zoomV = mempty
-- #else
--   let zoomActive = pure True :: Signal Bool
--   -- let zoomXY = pure $ V2 1 1
-- #endif
--   -- Debug
--   when debugHandlers $ void $
--     subscribeEvent (updates zoomXY) print
--
--   let !(plotSD :: Styled Drawing) = trace "> plotSD" $ drawPlot $ mconcat
--           [ plot (zip ([1..]::[Double])
--             -- [31,35,78,23,9,71,53,92,53,42::Double]
--             (
--             kSeries
--             ::[Double])
--             )
--               [x <~ _1, y <~ _2]
--               (mconcat [line {-, fill, pointG, xInterceptAlways, yInterceptAlways-}])
--           , plotLabel "(4,50)" [(4::Double, 50::Double)]
--               [x <~ _1, y <~ _2]
--           , plotLabel "(7,65)" [(7::Double, 65::Double)]
--               [x <~ _1, y <~ _2]
--           ]
--
--   -- TODO use rrS here
--   let (plotVD :: SDrawing) =
--             let g currentZoom currentRenderingRectangle autoScaleOn =
--                         getStyled plotSD
--                           $ renderingRectangle .~ currentRenderingRectangle
--                           $ zoom .~ (focusFromRectangle currentZoom)
--                           $ zoomType .~ (if autoScaleOn then AutoScaleY else mempty)
--                           $ mempty
--             in liftA3 g zoomXY rrS asS
--   subscribeEvent (updates $ zoomXY) $ \_ -> print "U: zoomXY"
--   subscribeEvent (updates $ rrS) $ \_ -> print "U: rrS"
--   subscribeEvent (updates $ plotVD) $ \_ -> print "U: plotVD"
--
--   -- let plotD = getStyled plotSD mempty :: Drawing
--   (plotSD :: SDrawing) <- draggable_ $ plotVD
--   (plotSD2 :: SDrawing) <- draggable_ $ plotVD
--   (plotSD3 :: SDrawing) <- draggable_ $ plotVD
--   (plotSD4 :: SDrawing) <- draggable_ $ plotVD
--
--   let purpleCircle = trace "> purpleCircle" $ Lubeck.Drawing.fillColorA (Colors.purple `withOpacity` 0.2) $ Lubeck.Drawing.scale 190 circle
--   let pinkCircle   = trace "> pinkCircle" $ Lubeck.Drawing.fillColor Colors.pink $ Lubeck.Drawing.scale 150 circle
--   let !redSquare    = trace "> redSquare" $ Lubeck.Drawing.fillColor Colors.red $ Lubeck.Drawing.scale 190 square
--   let !blueSquare   = trace "> blueSquare" $ Lubeck.Drawing.fillColor Colors.blue $ Lubeck.Drawing.scale 190 square
--
--   dc1 <- draggable_ $ pure (pinkCircle ||| redSquare)
--   -- dc2 <- draggable_ $ pure purpleCircle
--
--   let (sqs  :: Signal Bool -> SDrawing) = (fmap $ \t -> if t then blueSquare else redSquare)
--
--   (sqs2 :: SRDrawing) <- (draggable_ <=< hoverable_) sqs >>= strictifyS . fmap (renderDrawing2 opts)
--   (sqs3 :: SRDrawing) <- (draggable_ <=< hoverable_) sqs >>= strictifyS . fmap (renderDrawing2 opts)
--   (sqs4 :: SRDrawing) <- (draggable_ <=< hoverable_) sqs >>= strictifyS . fmap (renderDrawing2 opts)
--   (sqs5 :: SRDrawing) <- (draggable_ <=< hoverable_) sqs >>= strictifyS . fmap (renderDrawing2 opts)
--   (sqs6 :: SRDrawing) <- (draggable_ <=< hoverable_) sqs >>= strictifyS . fmap (renderDrawing2 opts)
--   (sqs7 :: SRDrawing) <- (draggable_ <=< hoverable_) sqs >>= strictifyS . fmap (renderDrawing2 opts)
--   (sqs8 :: SRDrawing) <- (draggable_ <=< hoverable_) sqs >>= strictifyS . fmap (renderDrawing2 opts)
--   -- (sqsb :: SDrawing) <- hoverable_ (fmap $ \t -> if t then blueSquare else redSquare)
--   -- sqs2b <- draggable_ $ fmap (Lubeck.Drawing.scale 0.5) sqsb
--
--   (dr, rectDraggedE, clickResetE) <- dragRect Nothing (Just $ view _y <$> rrS) zoomActive
--
--   let (zoomDragInputs :: Events (Rect Double)) =
--         snapshotWith (\rr zr -> transformRect (recip $ scalingXY rr) zr) (current rrS) (normalizeRect <$> rectDraggedE)
--
--   {-
--   Consider the foo updates as a series of linear transformations, accumulate with (1 and (*))
--   -}
--   (compoundZoom :: Signal (T2 Double)) <- foldpSRestart (flip (*)) 1 (rectToTransf <$> zoomDragInputs) (1 <$ (resetE <> clickResetE))
--   -- Tie the knot!
--   subscribeEvent (transfToRect <$> updates compoundZoom) rectU
--
--   subscribeEvent (fmap (($ "") . showFFloat (Just 2)) <$> transfToRect <$> updates compoundZoom) print
--   subscribeEvent resetE print
--
--   {-
--   TODO get a signal/event that yields the integral of all zoom transformation
--   Also add an event that resets it
--   -}
--
--
--   (srds :: [SRDrawing]) <- mapM strictifyS $
--   -- let (srds :: [SRDrawing]) =
--               -- fmap (fmap $ renderDrawing opts)
--                 [ mempty
--                 -- fmap (renderDrawingTrace "R Circle and square") dc1
--                 , fmap (renderDrawingTrace "R Zoom") dr
--                 -- , fmap (renderDrawingTrace "Hoverable square") sqs2
--                 -- , sqs2
--                 , fmap (renderDrawingTrace "R Plot") plotVD
--                 , mempty
--                 -- , fmap (renderDrawingTrace "Plot2") (fmap (rotate (turn/3)) plotSD2)
--                 -- , fmap (renderDrawingTrace "Plot3") plotSD3
--                 -- , fmap (renderDrawingTrace "R Circles") $ pure $ duplicateN 10 (V2 1 1) purpleCircle
--                 -- , fmap (renderDrawingTrace "Plot4") plotSD4
--                 ]
--   let (sd :: SRDrawing) = mconcat srds
--   -- (sd :: SRDrawing) <- let [a,b,c,d,e,f,g] = srds in do
--   --   x <- fastMconcatS3 a b c
--   --   y <- fastMconcatS3 d e f
--   --   fastMconcatS3 x y f
-- #ifdef __GHCJS__
--   let allS =
--             mconcat [zoomV,
--             fmap (\x -> {-trace "E" $-} emitDrawing2 opts x) sd
--             ]
--   -- runAppReactive $ allS
--   -- allS2 <- strictifyS allS
--   let allS2 = allS
--   runWithAnimation $ (allS2 :: Signal Html)
--   print "Done!"
-- #else
--   let allS = fmap (\x -> {-trace "E" $-} emitDrawing2 opts x) sd
--   subscribeEvent (updates allS) $ \_ -> print "New image"
--   return ()
-- #endif
--   where
--     !opts = mempty { dimensions = P (V2 1600 800), originPlacement = BottomLeft }
--
-- -- -- | Evaluates events passing through to WHNF before propagating
-- -- strictify :: Events a -> FRP (Events a)
-- -- strictify e = do
-- --   (s,e2) <- newEvent
-- --   subscribeEvent e $ \x -> seq x (s x)
-- --   return e2
-- --
-- -- -- | Evaluates events passing through to WHNF before propagating
-- -- strictifyS :: Signal a -> FRP (Signal a)
-- -- strictifyS s = do
-- --   !z <- pollBehavior (current s)
-- --   u2 <- strictify (updates s)
-- --   stepperS z u2
--

emitDrawing2 opts x = unsafePerformIO $ do
  -- beginEmit
  let !r = emitDrawing opts x
  -- endEmit
  pure r
renderDrawing2 opts x = unsafePerformIO $ do
  -- beginRender
  let !r = renderDrawing opts x
  -- endRender
  pure r

foreign import javascript safe "window.beginEmit = function(){ for (i = 0; i < 100000; i++){ i = i } }" setupEmit :: IO ()
foreign import javascript safe "window.endEmit = function(){ for (i = 0; i < 100000; i++){ i = i } }" setupEmit2 :: IO ()
foreign import javascript unsafe "window.beginEmit()" beginEmit :: IO ()
foreign import javascript unsafe "window.endEmit()" endEmit :: IO ()

foreign import javascript safe "window.beginRender = function(){ for (i = 0; i < 100000; i++){ i = i } }" setupRender :: IO ()
foreign import javascript safe "window.endRender = function(){ for (i = 0; i < 100000; i++){ i = i } }" setupRender2 :: IO ()
foreign import javascript unsafe "window.beginRender()" beginRender :: IO ()
foreign import javascript unsafe "window.endRender()" endRender :: IO ()

-- MISC test

duplicateN :: Int -> V2 Double -> Drawing -> Drawing
duplicateN n v d = mconcat $ take n $ iterate (translate v) d

-- The classic Photosphop "duplicate" function
duplicate :: Drawing -> Drawing
duplicate = duplicateAt (V2 50 50)

duplicateAt :: V2 Double -> Drawing -> Drawing
duplicateAt v d = d <> translate v d




-- animate' :: JSFun (IO ()) -> IO ()

#ifdef __GHCJS__
foreign import javascript unsafe
  "var req = window.requestAnimationFrame;   var f = function() { $1(); req(f); };   req(f);"
  animate' :: Callback (IO ()) -> IO ()

-- foreign import javascript unsafe "h$mainZCZCMainzimain();"
  -- retainMain :: IO ()

animate :: IO () -> IO ()
animate k = do
  cb <- syncCallback ThrowWouldBlock k
  animate' cb

data WithCount a = WithCount !Int a

{-|
Sample and render using requestAnimationFrame (forever).
-}
runWithAnimation :: Signal Html -> IO ()
runWithAnimation b = do
  (s, start) <- animated2 b
  runAppReactive s
  print "Starting animation"
  start


{-|
Similar to animated, but eliminate double rendering.
Only actually propagates when input has propagated.
-}
animated2 :: Signal a -> FRP (Signal a, IO ())
animated2 b = do
  let hB = current b
  z <- pollBehavior hB
  countB <- counter (updates b)

  let (cAndCountB) = liftA2 WithCount countB hB
    -- cAndCountB :: Behavior (WithCount a)

  (sink, ev) <- newEvent
  s <- stepperS z ev
  last <- newIORef (-1)
  -- forkIO $ forever $ do
  --   lastV <- readIORef last
  --   threadDelay 500000
  --   print lastV
  let start = animate $ do
            lastV <- readIORef last
            (WithCount n x) <- pollBehavior cAndCountB
            -- TODO prevent re-render
            when (n /= lastV) $
              sink (seq x x)
            writeIORef last n
            -- print (n, lastV)
  return (s, start)

{-|
Browser only: Poll the given behavior from the animation callback.
Returns a signal that will be pushed to when invoked from the ACB and an action to start animation.
-}
animated :: Behavior a -> FRP (Signal a, IO ())
animated b = do
  z <- pollBehavior b
  (sink, ev) <- newEvent
  s <- stepperS z ev
  let start = animate $ do
            x <- pollBehavior b
            sink x
  return (s, start)
#endif

-- #else
-- main = print "Dummy"
-- #endif

liftA4 f a b c d = f <$> a <*> b <*> c <*> d









data GrowthGraphRaw = GrowthGraphRaw
  { followers   :: [(UTCTime, Int)]
  , likes       :: [(UTCTime, Int)] -- up to 5'000
  , comments    :: [(UTCTime, Int)] -- up to 5'000
  , positions   :: [(UTCTime, String, Int, Int)] -- (time, img url, likes, comments)
  }
data GrowthType = Followers | Likes | Comments
data GrowthGraphFilter = GrowthGraphFilter
  { growthType :: GrowthType
  , dateRange  :: Maybe (UTCTime, UTCTime)
  }


-- main = runAppReactive $ pure $ text "Hello!"

main = do
  (rotationV,rotationS) <- plusMinusI "Rotation" 0 mempty
  runAppReactive $ mconcat [rotationV, fmap plotRotatedNSteps rotationS]
  where
    plotRotatedNSteps n = toSvg mempty
      $ withDefaultStyle
      $ drawPlot
      $ plot (dat n) [ x <~ _1 , y <~ _2 ] pointG
    -- dat n = zip [0..] (rotate n [1..200]) :: [(Int,Int)]

    dat 0 = zip [0..] [1..nPoints] :: [(Int,Int)]
    dat 1 = zip [0..] [nPoints,nPoints-1..1]
    dat _ = error "No such data set"

    nPoints = 2000

    rotate n xs = drop n xs ++ take n xs
