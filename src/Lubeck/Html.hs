
{-# LANGUAGE CPP, TypeSynonymInstances #-}

{-|
Provides a HTML representation.

Currently we use the virtual-dom bindings directly, so you want to import modules from @ghcjs-vdom@ to construct these nodes.

Here is a complete example, from @ExampleStatic.hs@.

@
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
    , li () [a (href "campaigns/")    [text "Campaigns"]]
    , li () [a (href "example/")      [text "Example page"]]
    ]
  ]

main :: IO ()
main = runAppStatic page
@
-}
module Lubeck.Html (Html) where

#ifdef __GHCJS__
import Web.VirtualDom.Html (Html)
import qualified Web.VirtualDom.Html as E

instance Monoid Html where
  mempty      = E.div [] []
  mappend x y = E.div [] [x, y]
#else

type Html = ()
#endif
