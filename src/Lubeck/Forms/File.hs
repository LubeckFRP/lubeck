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
import qualified JavaScript.Web.File as WF
import           JavaScript.Web.File
import GHCJS.Types

import qualified Web.VirtualDom as VD
import qualified Web.VirtualDom.Html as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events as Ev


newtype FileList = FileList JSVal

foreign import javascript unsafe "$1.target.files"
  targetFiles :: Ev.Event -> FileList

foreign import javascript unsafe "$1.length"
  fileListLength :: FileList -> Int

foreign import javascript unsafe "$1[$2]"
  fileListItem :: FileList -> Int -> File

eventTargetFiles :: Ev.Event ->  [FormDataVal]
eventTargetFiles e = fmap ((\f -> FileVal f (Just $ WF.name f)) . fileListItem fl) [0..pred len]
  where
    fl = targetFiles e
    len = fileListLength fl

filesSelectWidget :: Bool -> Widget' [(JSString, FormDataVal)]
filesSelectWidget multi sink _ =
  E.input
    ([ A.class_ "form-control"
    , A.type_ "file"
    , Ev.change $ \e -> do
          let fn = "images[]"
          let files = eventTargetFiles e
          let formFiles = zip (repeat fn) files

          sink formFiles
          return ()
    ] <> multiAttr) []

  where
    multiAttr = if multi then [(VD.attribute "multiple") "true"] else []
