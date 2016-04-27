{-# LANGUAGE CPP                  #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-|
Provides a HTML representation. This is just a wrapper around "Web.VirtualDom".

For details, see:

- "Web.VirtualDom.Html"
- "Web.VirtualDom.Svg"

Here is a complete example, from @ExampleStatic.hs@.

@
module Main where

import Prelude hiding (div)
import qualified Prelude

import GHCJS.Types(JSString, jsval)
import Web.VirtualDom.Html (a, p, h1, div, text, form, button, img, hr, ul, li, custom)
import Web.VirtualDom.Html.Events (click, change, submit, stopPropagation, preventDefault, value)
import Web.VirtualDom.Html.Attributes (src, href, width, class_)

import Lubeck.App (Html, runAppStatic)

page :: Html
page = div () [h1 () [text "Index"]
  , ul ()
    [ li () [a (href "bdplatform/")   [text "BD Platform"]]
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
module Lubeck.Html
  ( Html
  ) where

#ifdef __GHCJS__
import           Web.VirtualDom.Html (Html)
import qualified Web.VirtualDom.Html as E

instance Monoid Html where
  mempty      = E.div [] []
  mappend x y = E.div [] [x, y]
#else

type Html = ()
#endif
