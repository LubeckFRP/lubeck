{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module Lubeck.Web.URI
  ( encodeURIComponent
  , decodeURIComponent
  , getURIParameter
  ) where

import           GHCJS.Foreign.QQ (js, js_, jsu', jsu_)
import           GHCJS.Types      (JSVal, JSString, jsval)

-- |
-- See https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects/decodeURIComponent
decodeURIComponent :: JSString -> JSString
decodeURIComponent x = [jsu'| decodeURIComponent(`x) |]

-- |
-- See https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent
encodeURIComponent :: JSString -> JSString
encodeURIComponent x = [jsu'| encodeURIComponent(`x) |]

-- | Get the value of the given URI parameter.
getURIParameter :: JSString -> IO (Maybe JSString)
getURIParameter paramHs = fmap noEmpty $ [js|
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
    noEmpty xs
      | xs == ""  = Nothing
      | otherwise = Just xs
