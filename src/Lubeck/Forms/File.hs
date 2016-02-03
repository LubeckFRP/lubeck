{-# LANGUAGE OverloadedStrings, JavaScriptFFI #-}

module Lubeck.Forms.File
  ( filesSelectWidget
  ) where

import           Data.Monoid

import Lubeck.Forms
import qualified Data.List
import Lubeck.Util
import qualified Data.Map
import Data.JSString (JSString, pack, unpack)

import JavaScript.Web.XMLHttpRequest (FormDataVal(..))
import GHCJS.Types

import qualified Web.VirtualDom as VD
import qualified Web.VirtualDom.Html as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events as Ev


filesSelectWidget :: JSString -> Bool -> Widget' [(JSString, FormDataVal)]
filesSelectWidget formFieldName multi sink _ =
  E.input
    ([ A.class_ "form-control"
    , A.type_ "file"
    , Ev.change $ \e -> case Ev.files e of
                          Nothing -> return ()
                          Just fs -> sink $ zip (repeat formFieldName) fs

    ] <> multiAttr) []

  where
    multiAttr = if multi then [(VD.attribute "multiple") "true"] else []
