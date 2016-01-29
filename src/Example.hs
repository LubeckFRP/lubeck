
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import Data.Monoid ((<>))

import GHCJS.Types(JSString, jsval)
import Web.VirtualDom.Html (p, h1, div, text, form, button, img, hr, a, table, tbody, th, tr, td, input, label)
import Web.VirtualDom.Html.Events (click, change, keyup, submit, stopPropagation, preventDefault, value)
import Web.VirtualDom.Html.Attributes (src, width, class_, href, target, width, src)
import qualified Web.VirtualDom.Html as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events as Ev
import qualified Web.VirtualDom.Svg.Events as SvgEv
import qualified Data.JSString

import Lubeck.FRP
import Lubeck.App (Html, runAppReactive)
import Lubeck.Forms (Widget, Widget', component)
-- import Lubeck.Drawing (drawTest)
import Lubeck.Drawing hiding (text)

import qualified Data.Colour.Names as Colors


render :: Widget' JSString
render output model = div []
  [ div [] $ pure $ h1 [] $ pure $ text model
  , div [] $ pure $ button
    [ click (\_ -> output (model <> "!")) ]
    [ text "Click me" ]
  ,

  -- drawTest (length $ Data.JSString.unpack $ model)
  toSvg (RenderingOptions (Point 400 400) Center) $ drawing (length model)

  ]

drawing n = (addProperty (SvgEv.onClick $ \_ -> print "Clicked!") (redCircle n)) <> xyAxis <> smokeBackground
redCircle n = fillColor Colors.red (scale (15 + 5 * fromIntegral n) circle)

-- MAIN

main :: IO ()
main = do
  (view, _) <- component "!" render
  runAppReactive view
