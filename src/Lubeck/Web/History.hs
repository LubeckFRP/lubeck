
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

{-|
Low-level bindings to Web History API.

For a more high-level interface, see "Lubeck.FRP.History".

See https://developer.mozilla.org/en-US/docs/Web/API/History_API
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

-- |
-- See https://developer.mozilla.org/en-US/docs/Web/API/History/forward
forward :: IO ()
forward = go 1

-- |
-- See https://developer.mozilla.org/en-US/docs/Web/API/History/back
back :: IO ()
back    = go (-1)

-- |
-- See https://developer.mozilla.org/en-US/docs/Web/API/History/go
go :: Int -> IO ()
go n = [jsu_| window.history.go(`n) |]

-- |
-- See https://developer.mozilla.org/en-US/docs/Web/API/History/pushState
pushState :: JSVal -> JSString -> JSString -> IO ()
pushState state title url = [jsu_| window.history.pushState(`state, `title, `url) |]
-- pushState state title url = [jsu_| console.log([`state, `title, `url]) |]


newtype PopStateEvent = PopStateEvent JSVal

foreign import javascript safe "$1.state"
  getPopStateEventState :: PopStateEvent -> JSVal

-- foreign import javascript safe "((function() { console.log($1.state); return $1.state; })())"
  -- getPopStateEventState :: PopStateEvent -> JSVal

-- |
-- See https://developer.mozilla.org/en-US/docs/Web/API/WindowEventHandlers/onpopstate
onpopstate :: (PopStateEvent -> IO ()) -> IO ()
onpopstate f = do
  cb <- syncCallback1 ThrowWouldBlock (f . PopStateEvent)
  js_onpopstate cb

foreign import javascript unsafe "window.onpopstate = $1"
-- foreign import javascript unsafe "window.bdpop = $1"
  js_onpopstate :: Callback (JSVal -> IO ()) -> IO ()
