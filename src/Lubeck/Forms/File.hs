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

buttonCSS = "position: relative;"

inputCSS = "position: absolute; "
        <> "top: 0; left: 0; "
        <> "display: block !important; "
        <> "width: 100% !important; height: 100% !important; "
        <> "opacity: 0 !important; "
        <> "overflow: hidden !important;"
        <> "margin:0px;"
        <> "padding: 0px;"
        <> "border: none"

-- | A widget for selecting files.
filesSelectWidget
  :: JSString               -- ^ Form field name.
  -> Maybe JSString         -- ^ MIME type.
  -> Bool                   -- ^ Multiple file selection.
  -> Widget' [(JSString, FormDataVal)]
filesSelectWidget formFieldName mime multi sink _ =
  let multiAttr  = if multi then [(VD.attribute "multiple") "true"] else []
      acceptAttr = case mime of
                     Nothing -> []
                     Just x  -> [(VD.attribute "accept") x]
  in E.div []
    [ E.button
        [ A.class_ "btn btn-link"
        , A.style buttonCSS ]
        [ E.i [ A.class_ "fa fa-cloud-upload"
              , A.style "margin-right: 5px"] []
        , E.text "Upload"

        , E.input
          ([ A.class_ "form-control"
           , A.type_ "file"
           , A.style inputCSS
           , Ev.change $ \e -> case Ev.files e of
                                 Nothing -> return ()
                                 Just fs -> sink $ zip (repeat formFieldName) fs

           ] <> multiAttr <> acceptAttr) []
        ]

    ]
