
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Lubeck.Web.History where

import GHCJS.Foreign.QQ (js, jsu, jsu')
import GHCJS.Types(JSString, jsval)

go :: Int -> IO ()
go n = [js_| window.history.go(n) |]

pushState :: JSVal -> JSString -> JSString -> IO ()
pushState state title url = [js_| window.history.pushState(state, title, url) |]

onPopState :: (JSVal -> IO ()) -> IO ()
onPopState cb = [js_| window.onpopstate = function() { console.log("State!")} |]
-- TODO
