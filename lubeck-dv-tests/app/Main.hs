
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

-- TODO separate/consolidate Html/Svg events
-- Currently they can be used more or less interchangebly
import Web.VirtualDom.Html.Events(Event, mousemove)

import Data.Colour.Names as Colors

----------

-- TODO handle onmousedown, onmousemove, onmouseout, onmouseup
-- Ref: http://www.petercollingridge.co.uk/interactive-svg-components/draggable-svg-element

-- From mouse events we can read various properties (clientX, screenX, movementX)
-- https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/movementX
-- Easiest would be to just use movementX!

foreign import javascript unsafe "$1.movementX"
  movementX :: Event -> Double
foreign import javascript unsafe "$1.movementY"
  movementY :: Event -> Double

-- TODO move
draggable :: Drawing -> FRP (Signal Drawing, Signal (P2 Double))
draggable d =  do
  (move, moved :: Events (V2 Double)) <- newEvent
  (pos :: Signal (P2 Double)) <- accumS (P $ V2 0 0) (fmap (^+.) moved)

  let dWithHandlers = (addProperty (mousemove (handleMouseMove move)) d)

  -- TODO handle down/up/out

  return $ (fmap (\(P v) -> translate v dWithHandlers) pos, pos)
  where
    (^+.) = flip (.+^)
    handleMouseMove dest ev = do
      dest $ V2 (movementX ev) (-movementY ev)
      return ()

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

  dc1 <- fmap fst $ draggable pinkCircle
  dc2 <- fmap fst $ draggable purpleCircle
  let (sd :: Signal Drawing) = mconcat [fmap (translateX 120) dc1, dc2]

  runAppReactive $ fmap (toSvg mempty) $ sd
