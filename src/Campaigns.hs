
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import Data.Monoid ((<>))

import GHCJS.Types(JSString, jsval)
import GHCJS.VDOM.Event (click, change, submit, stopPropagation, preventDefault, value)
import GHCJS.VDOM.Element (p, h1, div, text, form, button, img, hr, custom)
import GHCJS.VDOM.Attribute (src, width, class_)

import Lubeck.FRP
import Lubeck.App (Html, runAppPure)
import Lubeck.Web.History
import Lubeck.Forms (Widget, Widget')

update :: Events () -> IO (Behavior JSString)
update = foldpR step initial
  where
    initial = "Campaigns"
    step () model = model <> "!"

render :: Widget JSString ()
render actions model = h1 ()
  [ div () $ text model
  , div () $ button
    [ click (\_ -> actions ()) ]
    [ text "Click me" ] ]

-- MAIN

main :: IO ()
main = runAppPure update render
