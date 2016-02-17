
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (div)
import qualified Prelude
import GHCJS.Types(JSString, jsval)

import Web.VirtualDom.Html (p, h1, h2, div, text, li, ul, a)
import Web.VirtualDom.Html.Attributes (href)

import Lubeck.App (Html, runAppStatic)

page :: Html
page = div [] [h1 [] [text "Index"]
  , h2 [] [text "Applications"]
  , ul []
    [ li [] [a [href "adplatform/"]      [text "Ad platform"]]
    , li [] [a [href "interactions/"]    [text "Interaction browser"]]
    ]
  , h2 [] [text "Examples"]
  , ul []
    [ li [] [a [href "example-static/"]  [text "Example: Static page"]]
    , li [] [a [href "example-dynamic/"] [text "Example: Counters"]]
    , li [] [a [href "example-api-req/"] [text "Example: BD Api"]]
    , li [] [a [href "example-history/"] [text "Example: History"]]
    , li [] [a [href "example/"]         [text "Example: Reactive page"]]
    , li [] [a [href "example-plots/"]   [text "Example: DV 1"]]
    ]
  , h2 [] [text "Documentation"]
  , ul []
    [ li []
      [a [href "doc/"]
      [text "Haddock docs"]]
    , li []
      [a [href "https://www.stackage.org/nightly-2015-12-14"]
      [text "Stackage docs"]]
    , li []
      [a [href "https://github.com/BeautifulDestinations/lubeck/wiki"]
      [text "Wiki"]]
    ]
  ]

main :: IO ()
main = runAppStatic page
