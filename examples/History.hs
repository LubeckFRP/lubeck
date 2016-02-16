
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import GHCJS.Types(JSString, jsval)
-- import Web.VirtualDom.Html (p, h1, div, text, form, button, img, hr, a, table, tbody, th, tr, td, input, label, ul, li)
-- import Web.VirtualDom.Html.Events (click, change, keyup, submit, stopPropagation, preventDefault, value)
-- import Web.VirtualDom.Html.Attributes (src, width, class_, href, target, width, src)
import qualified Web.VirtualDom.Html as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events as Ev

import Lubeck.FRP
import Lubeck.App (Html, runAppStatic)
import JavaScript.Cast (cast) -- JSVal -> Maybe a
import Lubeck.Web.History
import Lubeck.Forms
import Lubeck.Forms.Basic (rangeWidget)
import Lubeck.FRP.History

page :: IO (Signal Html)
page = do
  (inputView, intsE) <- componentEvent 0 (rangeWidget 0 100 1) mempty
  let outputView   = componentListen integerWidget (stepperS 0 intsE)
  return $ mconcat [inputView, outputView]

-- MAIN

main :: IO ()
main = page >>= runAppReactive
