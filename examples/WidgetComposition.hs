
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import GHCJS.Types(JSString, jsval)
import Web.VirtualDom.Html as H
import Web.VirtualDom.Html.Events as H
import Web.VirtualDom.Html.Attributes as H

import Lubeck.App (Html, runAppStatic)


data Big = Big Int Double

page :: Html
page = H.div [] [H.h1 [] [H.text "Index"]]


-- MAIN

main :: IO ()
main = runAppStatic page
