{-# LANGUAGE OverloadedStrings  #-}

module BDPlatform.HTMLCombinators where

import           Data.Maybe
import           Data.Monoid

import qualified Web.VirtualDom.Html            as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events     as Ev


toolbar' x          = E.div [A.class_ "btn-toolbar"] [ x ]
toolbar x           = E.div [A.class_ "btn-toolbar"] x
buttonGroup' x      = E.div [A.class_ "btn-group"] [ x ]
buttonGroup x       = E.div [A.class_ "btn-group"] x
button' title attrs = E.button ([A.class_ "btn"] <> [attrs]) [E.text title]
button title attrs  = E.button ([A.class_ "btn"] <> attrs) [E.text title]

markActive (Just x) y = if x == y then A.class_ "btn btn-primary" else A.class_ "btn"
markActive Nothing  _ = A.class_ "btn"
