
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

go :: Int -> IO ()
go n = [jsu_| window.history.go(`n) |]

pushState :: JSVal -> JSString -> JSString -> IO ()
pushState state title url = [jsu_| window.history.pushState(`state, `title, `url) |]

onPopState :: (JSVal -> IO ()) -> IO ()
onPopState = error "TODO"
-- onPopState cb = [jsu_| window.onpopstate = function(e) { `cb(e.state); console.log("State!") } |]
-- TODO figure out how to quote the callback, or use standard FFI
