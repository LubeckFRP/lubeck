
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import GHCJS.Types(JSString, jsval)
import GHCJS.VDOM.Event (click, change, submit, stopPropagation, preventDefault, value)
import GHCJS.VDOM.Element (p, h1, div, text, form, button, img, hr, custom)
import GHCJS.VDOM.Attribute (src, width, class_)

import Lubeck.FRP
import Lubeck.App (Html, runApp)

import BD.Query.PostQuery

type Widget i o = Sink o -> i -> Html
type Widget' a  = Widget a a

update :: E () -> IO (R (JSString, Maybe (IO ())))
update = foldpR step initial
  where
    initial = ("Hello Web!", Nothing)
    step () (model,_) = (model, Nothing)

render :: Widget JSString ()
render actions model = h1 () [text model]

-- MAIN

main :: IO ()
main = runApp update render
