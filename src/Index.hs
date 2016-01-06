
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import GHCJS.Types(JSString, jsval)
import GHCJS.VDOM.Event (click, change, submit, stopPropagation, preventDefault, value)
import GHCJS.VDOM.Element (a, p, h1, div, text, form, button, img, hr, ul, li, custom)
import GHCJS.VDOM.Attribute (src, href, width, class_)

import Lubeck.App (Html, runAppStatic)

page :: Html
page = div () [h1 () [text "Index"]
  , ul ()
    [ li () [a (href "adplatform/")   [text "Ad platform"]]
    , li () [a (href "interactions/") [text "Interaction browser"]]
    , li () [a (href "posts/")        [text "Post search"]]
    ]
  ]

-- MAIN

main :: IO ()
main = runAppStatic page
