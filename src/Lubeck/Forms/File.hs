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

-- a 'standard' hack to hide ugly real <input type=file /> and show some nice UI
foreign import javascript unsafe "$1.target.nextSibling.click()"
  clickRealInput ::  Ev.Event -> IO ()

filesSelectWidget :: JSString -> Maybe JSString -> Bool -> Widget' [(JSString, FormDataVal)]
filesSelectWidget formFieldName mime multi sink _ =
  E.div []
    [ E.button [ A.class_ "btn btn-default"
               , Ev.click (\ev -> clickRealInput ev) ]
               [ E.i [ A.class_ "fa fa-cloud-upload"
                     , A.style "margin-right: 5px"] []
               , E.text "Upload" ]
    , E.input
      ([ A.class_ "form-control"
       , A.type_ "file"
       , A.style "display: none"
       , Ev.change $ \e -> case Ev.files e of
                             Nothing -> return ()
                             Just fs -> sink $ zip (repeat formFieldName) fs

       ] <> multiAttr <> acceptAttr) []
    ]


  where
    multiAttr  = if multi then [(VD.attribute "multiple") "true"] else []
    acceptAttr = case mime of
                   Nothing -> []
                   Just x  -> [(VD.attribute "accept") x]
