
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

{-|
Bindings to Web History API.

See https://developer.mozilla.org/en-US/docs/Web/API/History_API

/Experimental/
-}
module Lubeck.Web.History
  (
  -- * Navigating the browser
    forward
  , back
  , go
  -- * Reacting to bavigation
  , pushState
  , PopStateEvent
  , getPopStateEventState
  , onpopstate
  ) where

import GHCJS.Types (JSVal)
import GHCJS.Foreign.QQ (js_, jsu_)
import GHCJS.Types(JSString, jsval)

import GHCJS.Foreign.Callback (Callback, syncCallback1, OnBlocked(ThrowWouldBlock))

forward, back :: IO ()
forward = go 1
back    = go (-1)

-- |
-- See https://developer.mozilla.org/en-US/docs/Web/API/History/go
go :: Int -> IO ()
go n = [jsu_| window.history.go(`n) |]

-- |
-- See https://developer.mozilla.org/en-US/docs/Web/API/History/pushState
pushState :: JSVal -> JSString -> JSString -> IO ()
pushState state title url = [jsu_| window.history.pushState(`state, `title, `url) |]


newtype PopStateEvent = PopStateEvent JSVal

foreign import javascript safe "$1.state"
  getPopStateEventState :: PopStateEvent -> JSVal

-- |
-- See https://developer.mozilla.org/en-US/docs/Web/API/WindowEventHandlers/onpopstate
onpopstate :: (PopStateEvent -> IO ()) -> IO ()
onpopstate f = do
  cb <- syncCallback1 ThrowWouldBlock f
  js_onpopstate cb

foreign import javascript unsafe "window.onpopstate($1)"
  js_onpopstate :: Callback (JSVal -> IO ()) -> IO ()
