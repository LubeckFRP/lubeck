{-# LANGUAGE OverloadedStrings  #-}

module BDPlatform.HTMLCombinators where

import           Data.Maybe
import           Data.Monoid

import qualified Web.VirtualDom.Html            as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events     as Ev


Nothing  ~== _ = False
(Just x) ~== y = x == y

toolbar x      = E.div [A.class_ "btn-toolbar"] x
toolbar' x     = toolbar [x]

buttonGroup x  = E.div [A.class_ "btn-group"] x
buttonGroup' x = buttonGroup [x]

button  title primary attrs = E.button ([A.class_ ("btn " <> markActive primary)] <> attrs) [E.text title]
button' title primary attrs = button title primary [attrs]

buttonIcon  title icon primary attrs =
  E.button ([A.class_ ("btn " <> markActive primary)] <> attrs)
    [ E.i [A.class_ ("fa fa-" <> icon), A.style "margin-right: 5px;"] []
    , E.text title]
buttonIcon' title icon primary attrs = buttonIcon title icon primary [attrs]

markActive True  = "btn-primary"
markActive False = ""
