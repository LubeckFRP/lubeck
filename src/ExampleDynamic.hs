{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import GHCJS.Types (JSString, jsval)
import qualified Web.VirtualDom as VD
import qualified Web.VirtualDom.Html as VDHtml
import qualified Web.VirtualDom.Html.Events as Event
import qualified Web.VirtualDom.Html.Attributes as Attr

import Lubeck.App (Html, runAppReactive)
import Lubeck.Forms
import Lubeck.Forms.Basic
import Lubeck.FRP

color :: JSString -> VD.Property
color = VD.property "color" . jsval

page :: Html
page = VDHtml.div [color "red"] [VDHtml.h1 [] [VDHtml.text "Heading1!"]
  , VDHtml.h3 [] [VDHtml.text "Heading3!!!"]
  , VDHtml.ol [] 
    [ VDHtml.li [color "blue"] [VDHtml.text "Item1?"]
    , VDHtml.li [color "green"] [VDHtml.text "Item2??"]
    ]
  ]

main :: IO ()
main = do
  (page, _) <- component 1 (bothWidget mappend integerWidget $ rangeWidget 1 100 1)
  runAppReactive page
