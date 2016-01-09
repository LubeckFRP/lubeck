
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Lubeck.Web.Uri
  ( encodeURIComponent
  , decodeURIComponent
  , getUriParameter
  ) where

import GHCJS.Types (JSVal)
import GHCJS.Foreign.QQ (js, js_, jsu_, jsu')
import GHCJS.Types(JSString, jsval)


decodeURIComponent :: JSString -> JSString
decodeURIComponent x = [jsu'| decodeURIComponent(`x) |]

encodeURIComponent :: JSString -> JSString
encodeURIComponent x = [jsu'| encodeURIComponent(`x) |]

-- | Get the value of the given URI parameter.
getUriParameter :: JSString -> IO (Maybe JSString)
getUriParameter paramHs = noEmpty $ [js|
  (function(){

  function parseQueryString(query) {
        var obj = {},
            qPos = query.indexOf("?"),
	    tokens = query.substr(qPos + 1).split('&'),
	    i = tokens.length - 1;
	if (qPos !== -1 || query.indexOf("=") !== -1) {
		for (; i >= 0; i--) {
			var s = tokens[i].split('=');
			obj[decodeURIComponent(s[0])] = s.hasOwnProperty(1) ? decodeURIComponent(s[1]) : null;
		};
	}
	return obj;
}
  var r = parseQueryString(window.location.search)[`paramHs];
  return (r ? r : "");
  }())
  |]
  where
    noEmpty [] = Nothing
    noEmpty xs = Just xs
