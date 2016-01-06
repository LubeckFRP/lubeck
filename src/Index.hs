
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import GHCJS.Types(JSString, jsval)
import GHCJS.VDOM.Event (click, change, submit, stopPropagation, preventDefault, value)
import GHCJS.VDOM.Element (p, h1, div, text, form, button, img, hr, custom)
import GHCJS.VDOM.Attribute (src, width, class_)

import Lubeck.FRP
import Lubeck.App (Html, runAppPure)
import Lubeck.Forms (Widget, Widget')

update :: Events () -> IO (Behavior JSString)
update = foldpR step initial
  where
    initial = "Index"
    step () model = model

render :: Widget JSString ()
render actions model = h1 () [text model]

-- MAIN

main :: IO ()
main = runAppPure update render
