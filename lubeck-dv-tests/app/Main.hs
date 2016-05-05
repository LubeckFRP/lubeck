
{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, ScopedTypeVariables #-}

module Main where

import BasePrelude
import Control.Lens(to, _1, _2, (.~))
-- import Linear.V2 (V2(..))

import Lubeck.DV
import Lubeck.FRP
import Lubeck.Drawing(Drawing, toSvg, V2(..))
import Lubeck.Forms(componentEvent)
import Lubeck.Forms.Button(multiButtonWidget)
import Lubeck.App(runAppReactive)
import Web.VirtualDom.Html(text)

plusMinus label init = do
  (view, alter :: Events (Double -> Double)) <- componentEvent id (multiButtonWidget [] [("-", (/ 1.1)), ("+", (* 1.1))]) mempty
  zoom <- accumS init alter
  return (mconcat [pure $ text label, view], zoom)

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
  runAppReactive $ mconcat [view1, view2, plotVD]
