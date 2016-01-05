
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Lubeck.Web.History where

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
-- TODO
