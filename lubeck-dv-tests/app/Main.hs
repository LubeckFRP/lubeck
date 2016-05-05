
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

data MouseEv = MouseNone | MouseUp | MouseDown | MouseOver | MouseOut | MouseMove deriving (Enum, Eq, Ord, Show, Read)

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

  patch x _ = x


withMouseState2 :: (Signal MouseState -> Signal Drawing) -> FRP (Signal Drawing, Signal MouseState)
withMouseState2 d = do
  (mouseS, mouseE :: Events MouseEv) <- newEvent
  (state :: Signal MouseState) <- accumS mempty (fmap (flip patch) mouseE)
  when debugHandlers $ do
    subscribeEvent (updates state) print
    pure ()
  let (dWithHandlers :: Signal MouseState -> Signal Drawing) = (fmap . fmap)
                           ( id
                          --  . addProperty (mouseover $ const $ mouseS MouseOver)
                          --  . addProperty (mouseout  $ const $ mouseS MouseOut)
                          --  . addProperty (mouseup   $ const $ mouseS MouseUp)
                          --  . addProperty (mousedown $ const $ mouseS MouseDown)
                           . addHandler "mouseover" (const $ mouseS MouseOver)
                           . addHandler "mouseout"  (const $ mouseS MouseOut)
                           . addHandler "mouseup"   (const $ mouseS MouseUp)
                           . addHandler "mousedown" (const $ mouseS MouseDown)
                           ) d
  pure $ (dWithHandlers state, state)

-- withMouseState_ :: Drawing -> FRP (Signal Drawing, Signal MouseState)
-- withMouseState_ d = withMouseState2 (fmap $ const d)

-- TODO consolidate with above
withMouseState_2 :: Signal Drawing -> FRP (Signal Drawing, Signal MouseState)
withMouseState_2 d = withMouseState2 (const d)

hoverable :: (Bool -> Drawing) -> FRP (Signal Drawing, Signal Bool)
hoverable d = fmap (second $ fmap mouseInside) $ withMouseState2 (fmap (d . mouseInside))

hoverable_ :: (Bool -> Drawing) -> FRP (Signal Drawing)
hoverable_ = fmap fst . hoverable



nudgeableDraggable :: Behavior Bool -> Signal Drawing -> FRP (Signal Drawing, Signal (P2 Double))
nudgeableDraggable allowMove d =  do
  (move, moved1 :: Events (V2 Double)) <- newEvent
  let moved = filterJust $ snapshotWith (\allow m -> if allow then Just m else Nothing) allowMove moved1
  (pos :: Signal (P2 Double)) <- accumS (P $ V2 0 0) (fmap (^+.) moved)

  let (dWithHandlers :: Signal Drawing) = fmap (addProperty (mousemove (handleMouseMove move))) d

  -- TODO handle down/up/out
  pure $ ( liftA2 (\(P v) dWithHandlers' -> translate v dWithHandlers') pos dWithHandlers, pos )
  where
    (^+.) = flip (.+^)
    handleMouseMove dest ev = do
      dest $ V2 (movementX ev) (-movementY ev)
      pure ()

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
  (d2, mouseStateS :: Signal MouseState) <- withMouseState_2 d
  nudgeableDraggable (fmap mouseDown $ current $ mouseStateS) d2

draggable_ :: Signal Drawing -> FRP (Signal Drawing)
draggable_ = fmap fst . draggable


{-
Displays a pair or plus/minus-labeled buttons.
-}
plusMinus :: Str -> Double -> IO (Signal Html, Signal Double)
plusMinus label init = do
  (view, alter :: Events (Double -> Double)) <- componentEvent id (multiButtonWidget [("-", (/ 1.1)), ("+", (* 1.1))]) mempty
  zoom <- accumS init alter
  pure (mconcat [pure $ text (toJSString label), view], zoom)



data DragSquare
  -- Begin creating a square from this point
  = BeginSquare (P2 Double)
  -- Continue creating a square (mainly for GUI feedback)
  | ContinueSquare (P2 Double)
  -- Finish creating square
  | EndSquare (P2 Double)
  -- Abort current square in process
  | Abort

dragSquare :: Drawing -> Signal Bool -> FRP (Signal Drawing, Signal (P2 Double, P2 Double))
dragSquare d activeS = do
  -- TODO
  (dragSquare, squareDragged :: Events DragSquare) <- newEvent

  let dWithOverlay :: Signal Drawing = flip fmap activeS $ \s -> if s then mconcat [bigTransparent, d] else d
  pure $ (dWithOverlay, pure (0, 0))
  where
    bigTransparent =
      -- TODO no color
        fillColorA (Colors.green `withOpacity` 0.1) $ Lubeck.Drawing.scale 3000 square


----------

type SDrawing = Signal Drawing

main :: IO ()
main = do
  (view1, zoomX) <- plusMinus "Zoom X" 1
  (view2, zoomY) <- plusMinus "Zoom Y" 1
  let zoomXY = liftA2 V2 zoomX zoomY

  -- Debug
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

  (sqs  :: SDrawing) <- hoverable_ (\t -> if t then blueSquare else redSquare)
  sqs2 <- draggable_ sqs
  (sqsb :: SDrawing) <- hoverable_ (\t -> if t then blueSquare else redSquare)
  sqs2b <- draggable_ $ fmap (Lubeck.Drawing.scale 0.5) sqsb

  let (sd :: SDrawing) = mconcat
                [ fmap (translateX 120) dc1
                , dc2
                , sqs2
                , sqs2b
                , plotSD
                ]

  runAppReactive $ mconcat [view1, view2, fmap (toSvg mempty) $ sd]
