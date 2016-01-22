
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import GHCJS.Types(JSString, jsval)
import Web.VirtualDom.Html (p, h1, div, text, form, button, img, hr, a, table, tbody, th, tr, td, input, label, li, ul)
import Web.VirtualDom.Html.Events (click, change, keyup, submit, stopPropagation, preventDefault, value)
import Web.VirtualDom.Html.Attributes (src, width, class_, href, target, width, src)

import Lubeck.App (Html, runAppStatic)

page :: Html
page = div [] [h1 [] [text "Index"]
  , ul []
    [ li [] [a [href "adplatform/"]   [text "Ad platform"]]
    , li [] [a [href "interactions/"] [text "Interaction browser"]]
    , li [] [a [href "posts/"]        [text "Post search"]]
    , li [] [a [href "campaigns/"]    [text "Campaigns"]]
    , li [] [a [href "example/"]      [text "Example page"]]
    ]
  ]

-- MAIN

main :: IO ()
main = runAppStatic page
