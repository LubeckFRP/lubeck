
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

{-|
Bindings to Web History API.

/Experimental/
-}
module Lubeck.Web.History
  ( go
  , pushState
  , onPopState
  ) where

import GHCJS.Types (JSVal)
import GHCJS.Foreign.QQ (js_, jsu_)
import GHCJS.Types(JSString, jsval)

import GHCJS.Foreign.Callback (Callback, syncCallback1, OnBlocked(ThrowWouldBlock))

go :: Int -> IO ()
go n = [jsu_| window.history.go(`n) |]

pushState :: JSVal -> JSString -> JSString -> IO ()
pushState state title url = [jsu_| window.history.pushState(`state, `title, `url) |]

onPopState :: (JSVal -> IO ()) -> IO ()
onPopState f = do
  cb <- syncCallback1 ThrowWouldBlock f
  js_onPopState cb

foreign import javascript unsafe "window.onpopstate($1)"
  js_onPopState :: Callback (JSVal -> IO ()) -> IO ()
