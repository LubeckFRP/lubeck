
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import Data.Monoid ((<>))

import GHCJS.Types(JSString, jsval)
import GHCJS.VDOM.Event (click, change, submit, stopPropagation, preventDefault, value)
import GHCJS.VDOM.Element (p, h1, div, text, form, button, img, hr, custom)
import GHCJS.VDOM.Attribute (src, width, class_)
import qualified Data.JSString

import Lubeck.FRP
import Lubeck.App (Html, runAppReactive)
import Lubeck.Forms (Widget, Widget', component)
import Lubeck.Drawing (drawTest)

render :: Widget' JSString
render output model = div ()
  [ div () $ h1 () $ text model
  , div () $ button
    [ click (\_ -> output (model <> "!")) ]
    [ text "Click me" ]
  , drawTest (length $ Data.JSString.unpack $ model) ]

-- MAIN

main :: IO ()
main = do
  (view, _) <- component "!" render
  runAppReactive view
