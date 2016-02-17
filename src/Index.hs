
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import GHCJS.Types(JSString, jsval)
import Web.VirtualDom.Html (p, h1, div, text, form, button, img, hr, a, table, tbody, th, tr, td, input, label, ul, li)
import Web.VirtualDom.Html.Events (click, change, keyup, submit, stopPropagation, preventDefault, value)
import Web.VirtualDom.Html.Attributes (src, width, class_, href, target, width, src)
import qualified Web.VirtualDom.Html as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events as Ev

import Lubeck.App (Html, runAppStatic)

page :: Html
page = div [] [h1 [] [text "Index"]
  , ul []
    [ li [] [a [href "adplatform/"]      [text "Ad platform"]]
    , li [] [a [href "interactions/"]    [text "Interaction browser"]]
    , li [] [a [href "example-static/"]  [text "Example: Static page"]]
    , li [] [a [href "example-dynamic/"] [text "Example: Counters"]]
    , li [] [a [href "example-api-req/"] [text "Example: BD Api"]]
    , li [] [a [href "example-history/"] [text "Example: History"]]
    , li [] [a [href "example/"]         [text "Example: Reactive page"]]
    , li [] [a [href "doc/"]             [text "Haddock docs"]]
    , li [] [a [href "https://www.stackage.org/nightly-2015-12-14"] [text "Stackage docs"]]
    ]
  ]

-- MAIN

main :: IO ()
main = runAppStatic page
