{-# LANGUAGE OverloadedStrings  #-}

module BDPlatform.Validators where

import           Control.Applicative

import           Data.Char                      (isAlphaNum, isPrint)
import           Data.Either.Validation
import qualified Data.JSString
import           Data.Monoid

import           GHCJS.Types                    (JSString)
import           Lubeck.Util                    (showJS)

-- TBD these functions may perform cleanup too
runValidation1 a       = ()
runValidation2 a b     = ()
runValidation3 a b c   = ()
runValidation4 a b c d = ()

lengthGT :: JSString -> Int -> JSString -> Validation [JSString] ()
lengthGT fn n s = if (Data.JSString.length s) > n
  then Success ()
  else Failure ["\"" <> fn <> "\" min length must be > " <> showJS n]

lengthLT :: JSString -> Int -> JSString -> Validation [JSString] ()
lengthLT fn n s = if (Data.JSString.length s) < n
  then Success ()
  else Failure ["\"" <> fn <> "\" max length must be < " <> showJS n]

isPrintable :: JSString -> JSString -> Validation [JSString] ()
isPrintable fn s = if (all id $ isPrint <$> (Data.JSString.unpack s))
  then Success ()
  else Failure ["\"" <> fn <> "\" must be printable"]

isAlphanum :: JSString -> JSString -> Validation [JSString] ()
isAlphanum fn s = if (all id $ isAlphaNum <$> (Data.JSString.unpack s))
  then Success ()
  else Failure ["\"" <> fn <> "\" must be alphanum"]

validateMatch :: JSString -> JSString -> JSString -> JSString -> Validation [JSString] ()
validateMatch fn1 fn2 a b = if a == b
  then Success ()
  else Failure ["\"" <> fn1 <> "\" and \"" <> fn2 <> "\" do not match"]

showValidationErrors :: [JSString] -> JSString
showValidationErrors es = Data.JSString.intercalate ", " es
