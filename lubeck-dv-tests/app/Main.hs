
{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, ScopedTypeVariables #-}

module Main where

import BasePrelude
import Control.Lens(to, _1, _2, (.~))
import Linear.Affine ((.+^))
-- import Linear.V2 (V2(..))

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

{-
TODO all initial arguments *could* be in S/B moinad
What is the most general interface?
-}

hoverable :: (Bool -> Drawing) -> FRP (Signal Drawing, Signal Bool)
hoverable d = do
  (overOut, overOuted :: Events Bool) <- newEvent
  (state :: Signal Bool) <- stepperS False overOuted
  let dWithHandlers = fmap (addProperty (mouseover (handleMouseOver overOut)) . addProperty (mouseout (handleMouseOut overOut))) d
  return $ (fmap dWithHandlers state, state)
  where
    -- TODO clean up this
    handleMouseOver dest ev = do
      dest True
    handleMouseOut dest ev = do
      dest False


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
  return $ (dWithOverlay, pure (0, 0))
  where
    -- TODO no color
    bigTransparent =
        fillColorA (Colors.green `withOpacity` 0.1) $ Lubeck.Drawing.scale 3000 square

-- TODO move
draggable :: Signal Drawing -> FRP (Signal Drawing, Signal (P2 Double))
draggable d =  do
  (move, moved :: Events (V2 Double)) <- newEvent
  (pos :: Signal (P2 Double)) <- accumS (P $ V2 0 0) (fmap (^+.) moved)
  let (dWithHandlers :: Signal Drawing) = fmap (addProperty (mousemove (handleMouseMove move))) d
  -- TODO handle down/up/out
  return $ ( liftA2 (\(P v) dWithHandlers' -> translate v dWithHandlers') pos dWithHandlers, pos )
  where
    (^+.) = flip (.+^)
    handleMouseMove dest ev = do
      dest $ V2 (movementX ev) (-movementY ev)
      return ()

draggable_ :: Signal Drawing -> FRP (Signal Drawing)
draggable_ = fmap fst . draggable

----------

plusMinus :: Str -> Double -> IO (Signal Html, Signal Double)
plusMinus label init = do
  (view, alter :: Events (Double -> Double)) <- componentEvent id (multiButtonWidget [("-", (/ 1.1)), ("+", (* 1.1))]) mempty
  zoom <- accumS init alter
  return (mconcat [pure $ text (toJSString label), view], zoom)

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

  let plotVD = fmap (\currentZoom -> toSvg mempty (getStyled plotSD (zoom .~ currentZoom $ mempty))) zoomXY
  -- runAppReactive $ mconcat [pure $ h1 [] [text "Zoom test"], view1, view2, plotVD]

  let purpleCircle = Lubeck.Drawing.fillColor Colors.purple $ Lubeck.Drawing.scale 190 circle
  let pinkCircle = Lubeck.Drawing.fillColor Colors.pink $ Lubeck.Drawing.scale 150 circle
  let redSquare = Lubeck.Drawing.fillColor Colors.red $ Lubeck.Drawing.scale 90 square
  let blueSquare = Lubeck.Drawing.fillColor Colors.blue $ Lubeck.Drawing.scale 90 square

  dc1 <- draggable_ $ pure pinkCircle
  dc2 <- draggable_ $ pure purpleCircle
  (sqs :: Signal Drawing) <- fmap fst $ hoverable (\t -> if t then blueSquare else redSquare)
  sqs2 <- draggable_ sqs
  let (sd :: Signal Drawing) = mconcat [fmap (translateX 120) dc1, dc2, sqs2]

  runAppReactive $ fmap (toSvg mempty) $ sd
