
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

plusMinus label = do
  (view, add :: Events Double) <- componentEvent 0 (multiButtonWidget [("-", -1), ("+", 1)]) mempty
  zoom <- accumS 0 (fmap (+) add)
  return (mconcat [pure $ text label, view], zoom)

main :: IO ()
main = do
  (view1, zoomX) <- plusMinus "Zoom X"
  (view2, zoomY) <- plusMinus "Zoom Y"
  let zoomXY = liftA2 V2 zoomX zoomY

  -- Debug
  subscribeEvent (updates zoomXY) print

  let (plotSD :: Styled Drawing) = drawPlot $
          plot (zip [1..10::Double] [31,35,78,23,9,92,53,71,53,42::Double])
            [x <~ _1, y <~ _2]
            (mconcat [line, fill])
  let plotVD = toSvg mempty (getStyled plotSD (zoom .~ (V2 1 1) $ mempty))

  runAppReactive $ mconcat [view1, view2, pure plotVD]
