
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Lubeck.Web.Uri (getUriParameter) where

import GHCJS.Types (JSVal)
import GHCJS.Foreign.QQ (js, js_, jsu_)
import GHCJS.Types(JSString, jsval)

getUriParameter :: JSString -> IO JSString
getUriParameter theName = [js|
  (function(){
    function getParameterByName(name) {
      name = name.replace(/[\[]/, "\\[").replace(/[\]]/, "\\]");
      var regex = new RegExp("[\\?&]" + name + "=([^&#]*)"),
          results = regex.exec(location.search);
      return results === null ? "" : decodeURIComponent(results[1].replace(/\+/g, " "));
    }
    return getParameterByName(`theName);
  }()) |]
