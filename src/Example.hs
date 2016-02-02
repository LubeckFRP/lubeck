
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import Data.Monoid ((<>))

import GHCJS.Types(JSString, jsval)
import qualified Web.VirtualDom.Html as H
import qualified Web.VirtualDom.Html.Attributes as H
import qualified Web.VirtualDom.Html.Events as H
import qualified Web.VirtualDom.Svg.Events as SvgEv
import qualified Data.JSString

import Lubeck.FRP
import Lubeck.App (Html, runAppReactive)
import Lubeck.Forms (Widget, Widget', component, bothWidget)
import Lubeck.Forms.Basic
-- import Lubeck.Drawing (drawTest)
import Lubeck.Drawing hiding (text)

import Data.Colour (withOpacity)
import qualified Data.Colour.Names as Colors


render :: Widget' Int
render = bothWidget mappend intW svgW

intW = rangeWidget 0 200 1
svgW output model = toSvg (RenderingOptions (Point 1200 1200) Center) $ drawing output model


drawing :: Sink Int -> Int -> Drawing
drawing output n = mempty
  <> circles
  <> addProperty (SvgEv.onClick $ \_ -> output (pred n)) (scale 0.8 $ redCircle $ negate n)
  <> blueRect
  <> addProperty (SvgEv.onClick $ \_ -> output (pred n)) (redCircle n)
  <> scale 10 xyAxis
  <> scale 10 smokeBackground
  where
    nCircles = 40
    circles = mconcat $ fmap (\i -> rotate (turn/fromIntegral nCircles*fromIntegral (negate i)) $ translateX (100+fromIntegral i+fromIntegral n) ci) [1..nCircles]
      where ci = fillColorA (Colors.green `withOpacity` 0.5) $ scale 10 $ scaleX 0.5 $ square
    blueRect = fillColorA (Colors.blue `withOpacity` 0.5) $ scale 50 $ scaleX 1.2 $ square
    redCircle n = fillColor Colors.red $
      translateY (negate $ 3 * fromIntegral n) $ translateX (4 * fromIntegral n) $
      scale (50 + 4 * fromIntegral n) $
      circle


-- MAIN

main :: IO ()
main = do
  (view, _) <- component 1 render
  runAppReactive view
