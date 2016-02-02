
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
import Lubeck.Forms (Widget, Widget', component)
-- import Lubeck.Drawing (drawTest)
import Lubeck.Drawing hiding (text)

import Data.Colour (withOpacity)
import qualified Data.Colour.Names as Colors


render :: Widget' Int
render output model = H.div []
  [ H.div [] $ pure $ H.h1 [] $ pure $ H.text (Data.JSString.pack $ replicate model '!')
  , H.div [] $ pure $ H.button
    [ H.click (\_ -> output (succ model)) ]
    [ H.text "Click me" ]
  ,
  -- drawTest (length $ Data.JSString.unpack $ model)
  toSvg (RenderingOptions (Point 400 400) Center) $ drawing output model

  ]

drawing :: Sink Int -> Int -> Drawing
drawing output n = mempty
  <> addProperty (SvgEv.onClick $ \_ -> output (pred n)) (scale 0.8 $ redCircle $ negate n)
  <> blueRect
  <> addProperty (SvgEv.onClick $ \_ -> output (pred n)) (redCircle n)
  <> xyAxis
  <> smokeBackground
  where
    blueRect = fillColorA (Colors.blue `withOpacity` 0.5) $ scale 50 $ scaleX 1.2 $ square
    redCircle n = fillColor Colors.red $
      translateY (5 * fromIntegral n) $ translateX (10 * fromIntegral n) $
      scale (50 + 5 * fromIntegral n) $
      circle


-- MAIN

main :: IO ()
main = do
  (view, _) <- component 1 render
  runAppReactive view
