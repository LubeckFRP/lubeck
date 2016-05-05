
{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, ScopedTypeVariables, TypeFamilies
  , FlexibleContexts #-}

module Main where

import BasePrelude hiding ((|||))
import Control.Lens(to, _1, _2, (.~))
import Linear.Affine ((.+^))

import Lubeck.DV
import Lubeck.FRP
import Lubeck.Str
import Lubeck.Drawing hiding (text)
-- import Lubeck.Drawing(Drawing, toSvg, V2(..))
import Lubeck.Forms(componentEvent)
import Lubeck.Forms.Button(multiButtonWidget)
import Lubeck.Forms.Select(selectEnumBoundedWidget)
import Lubeck.App(runAppReactive)
import Web.VirtualDom.Html(Html, h1, text)
import qualified Web.VirtualDom as VirtualDom

-- TODO separate/consolidate Html/Svg events
-- Currently they can be used more or less interchangebly
import Web.VirtualDom.Html.Events(Event(..), mousemove)

import Data.Colour (withOpacity)
import Data.Colour.Names as Colors

----------

debugHandlers = True

-- TODO handle onmousedown, onmousemove, onmouseout, onmouseup
-- Ref: http://www.petercollingridge.co.uk/interactive-svg-components/draggable-svg-element

-- From mouse events we can read various properties (clientX, screenX, movementX)
-- https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/movementX
-- Easiest would be to just use movementX!

onE e n = VirtualDom.on n . contramapS e
  where
    contramapS f k x = k (f x)

mouseover  = onE Event "mouseover"
mouseout   = onE Event "mouseout"
-- Note: use over/out rather than enter/leave!
mouseenter = onE Event "mouseenter"
mouseleave = onE Event "mouseleave"
mouseup    = onE Event "mouseup"
mousedown  = onE Event "mousedown"
-- mousemove = onE Event "mousemove"


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

data MouseEv = MouseNone | MouseUp | MouseDown | MouseOver | MouseOut | MouseMovedInside deriving (Enum, Eq, Ord, Show, Read)

instance Monoid MouseEv where
  mempty = MouseNone
  mappend x y = x -- last event to happen as

data MouseState = MouseState { mouseInside :: Bool, mouseDown :: Bool } deriving (Eq, Ord, Show, Read)

instance Monoid MouseState where
  mempty = MouseState False False -- how do we know this?
  mappend x y = x -- ?

instance Diffable MouseState where
  type Diff MouseState = MouseEv
  diff _ _ = error "TODO MouseState.diff"
  patch (MouseState inside down) MouseUp   = MouseState inside False
  patch (MouseState inside down) MouseDown = MouseState inside True
  patch (MouseState inside down) MouseOver = MouseState True   down

  -- If mouse goes while button is still pressed, release
  patch (MouseState inside down) MouseOut  = MouseState False  False

  -- If the mosue is moved, we must be inside
  patch (MouseState inside down) MouseMovedInside = MouseState True   down

  patch x _ = x

withMousePositionState :: (Signal MousePositionState -> Signal Drawing) -> FRP (Signal Drawing, Signal MousePositionState)
withMousePositionState d = do
  (mouseS, mouseE :: Events MouseEv) <- newEvent
  (state :: Signal MouseState) <- accumS mempty (fmap (flip patch) mouseE)

  (mousePosS, mousePosE :: Events (P2 Double)) <- newEvent
  (pos :: Signal (P2 Double)) <- stepperS (P $ V2 0 0) (mousePosE)

  let (posState :: Signal MousePositionState) = liftA2 MousePositionState pos state
  when debugHandlers $ void $
    subscribeEvent (updates posState) print
  -- TODO clean up this mess

  let (dWithHandlers :: Signal MousePositionState -> Signal Drawing) = (fmap . fmap)
                           ( id
                           . addHandler "mouseover" (\ev -> let e = Event ev in mousePosS (P $ V2 (offsetX e) (offsetY e)) >> mouseS MouseOver)
                           . addHandler "mouseout"  (\ev -> let e = Event ev in mousePosS (P $ V2 (offsetX e) (offsetY e)) >> mouseS MouseOut)
                           . addHandler "mouseup"   (\ev -> let e = Event ev in mousePosS (P $ V2 (offsetX e) (offsetY e)) >> mouseS MouseUp)
                           . addHandler "mousedown" (\ev -> let e = Event ev in mousePosS (P $ V2 (offsetX e) (offsetY e)) >> mouseS MouseDown)
                           . addHandler "mousemove" (\ev -> let e = Event ev in mousePosS (P $ V2 (offsetX e) (offsetY e)) >> mouseS MouseMovedInside)
                           ) d
  pure $ (dWithHandlers posState, posState)

withMouseState :: (Signal MouseState -> Signal Drawing) -> FRP (Signal Drawing, Signal MouseState)
withMouseState d = fmap (second $ fmap mouseState) $ withMousePositionState (d . fmap mouseState)

{-|
Like MouseState, but include position.
-}
data MousePositionState = MousePositionState
  { mousePosition :: P2 Double
  , mouseState    :: MouseState
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

nudgeableDraggable :: Behavior Bool -> Signal Drawing -> FRP (Signal Drawing, Signal (P2 Double))
nudgeableDraggable allowMove d =  do
  (move, moved1 :: Events (V2 Double)) <- newEvent
  let moved = filterJust $ snapshotWith (\allow m -> if allow then Just m else Nothing) allowMove moved1
  (pos :: Signal (P2 Double)) <- accumS (P $ V2 0 0) (fmap (^+.) moved)

  -- TODO use addHandler
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

{-
A rectangle, represented as two points.
-}
data Rect = Rect (P2 Double) (P2 Double)

squares :: Events RectInputEvent -> FRP (Events Rect)
squares = fmap (filterJust . fmap snd) . foldpE f (Nothing, Nothing)
  where
    f (BeginRect p1)   _            = (Just p1, Nothing)
    f (EndRect p2)     (Just p1, _) = (Nothing, Just (Rect p1 p2))
    f AbortRect        _            = (Nothing, Nothing)
    f (ContinueRect _) (x, _)       = (x,       Nothing)

{-
A drag overlay interface.

Whenever the given bool signal is False, this behaves exactly as the original drawing signal.
Otherwise, this displays an invisible drawing across the original image, that registers event
handlers.
-}
dragRect :: Signal Bool -> Signal Drawing -> FRP (Signal Drawing, Events Rect)
dragRect activeS dS = do
  -- TODO
  (dragS, dragE :: Events RectInputEvent) <- newEvent

  (bigTransparent2, bigTransparentMPS) <- withMousePositionState (\_ -> pure bigTransparent)

  subscribeEvent (updates bigTransparentMPS) print

  -- origina drawing, with the overlay whenever activeS is True
  let dWithOverlay :: Signal Drawing = liftA3 (\bt s d -> if s then mconcat [bt, d] else d) bigTransparent2 activeS dS
  pure $ (dWithOverlay, mempty)
  where

    dragRect =
        fillColorA (Colors.blue `withOpacity` 0.3) $ Lubeck.Drawing.scale 1 square
    bigTransparent =
      -- addHandler "" $
      -- TODO no color
        fillColorA (Colors.green `withOpacity` 0.1) $ Lubeck.Drawing.scale 3000 square

{-
Displays a pair or plus/minus-labeled buttons.
-}
plusMinus :: Str -> Double -> FRP (Signal Html, Signal Double)
plusMinus label init = do
  (view, alter :: Events (Double -> Double)) <- componentEvent id (multiButtonWidget [("-", (/ 1.1)), ("+", (* 1.1))]) mempty
  zoom <- accumS init alter
  pure (mconcat [pure $ text (toJSString label), view], zoom)


onOff :: Str -> FRP (Signal Html, Signal Bool)
onOff label = do
  (view, alter :: Events Bool) <- componentEvent False selectEnumBoundedWidget mempty
  state <- stepperS False alter
  pure (mconcat [pure $ text (toJSString label), view], state)
-- selectEnumBoundedWidget :: (Eq a, Enum a, Bounded a, Show a) => Widget' a


----------

type SDrawing = Signal Drawing

main :: IO ()
main = do
  (view0, zoomActive) <- onOff "Zoom active"
  (view1, zoomX) <- plusMinus "Zoom X" 1
  (view2, zoomY) <- plusMinus "Zoom Y" 1
  let zoomXY = liftA2 V2 zoomX zoomY

  -- Debug
  when debugHandlers $ void $
    subscribeEvent (updates zoomXY) print

  let (plotSD :: Styled Drawing) = drawPlot $ mconcat
          [ plot (zip [1..10::Double] [31,35,78,23,9,92,53,71,53,42::Double])
              [x <~ _1, y <~ _2]
              (mconcat [line, fill])
          , plotLabel "(4,50)" [(4::Double, 50::Double)]
              [x <~ _1, y <~ _2]
          , plotLabel "(7,65)" [(7::Double, 65::Double)]
              [x <~ _1, y <~ _2]
          ]

  let (plotVD :: SDrawing) = fmap (\currentZoom -> (getStyled plotSD (zoom .~ currentZoom $ mempty))) zoomXY
  -- let plotD = getStyled plotSD mempty :: Drawing
  (plotSD :: SDrawing) <- draggable_ $ plotVD

  let purpleCircle = Lubeck.Drawing.fillColor Colors.purple $ Lubeck.Drawing.scale 190 circle
  let pinkCircle   = Lubeck.Drawing.fillColor Colors.pink $ Lubeck.Drawing.scale 150 circle
  let redSquare    = Lubeck.Drawing.fillColor Colors.red $ Lubeck.Drawing.scale 190 square
  let blueSquare   = Lubeck.Drawing.fillColor Colors.blue $ Lubeck.Drawing.scale 190 square

  dc1 <- nudgeable_ $ pure (pinkCircle ||| redSquare)
  dc2 <- nudgeable_ $ pure purpleCircle

  (sqs  :: SDrawing) <- hoverable_ (fmap $ \t -> if t then blueSquare else redSquare)
  sqs2 <- draggable_ sqs
  (sqsb :: SDrawing) <- hoverable_ (fmap $ \t -> if t then blueSquare else redSquare)
  sqs2b <- draggable_ $ fmap (Lubeck.Drawing.scale 0.5) sqsb

  (dr, _) <- dragRect zoomActive sqs2b
  let (sd :: SDrawing) = mconcat
                [ mempty
                , dr
                , fmap (translateX 120) dc1
                , dc2
                , sqs2
                , sqs2b
                , plotSD
                ]

  runAppReactive $ mconcat [view0, view1, view2, fmap (toSvg mempty) $ sd]
