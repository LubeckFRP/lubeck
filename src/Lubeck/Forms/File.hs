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
import           JavaScript.Cast
import GHCJS.Types

import qualified Web.VirtualDom as VD
import qualified Web.VirtualDom.Html as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events as Ev


foreign import javascript unsafe "console.log($1)"
  elog :: Ev.Event -> IO ()

foreign import javascript unsafe "console.log($1)"
  vlog :: JSVal -> IO ()

-- this belongs here https://github.com/BeautifulDestinations/virtual-dom/blob/master/src/Web/VirtualDom/Html/Events.hs
foreign import javascript unsafe "Array.prototype.slice.call($1.target.files)"
  files :: Ev.Event -> JSVal

-- jsValToVal :: JSVal -> [File]
-- jsValToVal = unsafeCast
--
-- filesToFormVal :: [File] -> [FormDataVal]
-- filesToFormVal = fmap $ \f -> FileVal f (Just $ WF.name f)

filesSelectWidget :: Widget' [(JSString, FormDataVal)]
filesSelectWidget sink _ =
  E.input
    [ A.class_ "form-control"
    , A.type_ "file"
    , Ev.change $ \e -> do
          let fn = "images[]"
          elog e

          let f = files e
          vlog f

          -- let jsf = jsValToVal f
          -- let formDataFiles = filesToFormVal jsf
          -- let formFiles = zip (repeat fn) formDataFiles
          --
          -- sink formFiles
          return ()
    ] []

  -- where
    -- multiAttr = if multi then [(VD.attribute "multiple") "true"] else []
