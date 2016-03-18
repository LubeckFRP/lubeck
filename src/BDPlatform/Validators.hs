{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module BDPlatform.Validators where

import           Control.Applicative

import           Data.Char              (isAlphaNum, isPrint)
import           Data.Either.Validation
import qualified Data.JSString
import           Data.Monoid
import qualified Data.Semigroup

import           GHCJS.Types            (JSString)
import           Lubeck.Util            (showJS)

-- TBD these functions may perform cleanup too
runValidation1 a       = VSuccess
runValidation2 a b     = VSuccess
runValidation3 a b c   = VSuccess
runValidation4 a b c d = VSuccess

newtype VError = VError [JSString] deriving (Data.Semigroup.Semigroup)
data VSuccess = VSuccess

longString :: JSString -> Int -> Int -> JSString -> Validation VError VSuccess
longString fn minl maxl s = runValidation2 <$> lengthBetween fn minl maxl s <*> isAlphanum fn s

passwordString :: JSString -> Int -> Int -> JSString -> Validation VError VSuccess
passwordString fn minl maxl s = runValidation2 <$> lengthBetween fn minl maxl s <*> isPrintable fn s

notEqualTo :: (Eq a, Show a) => JSString -> a -> a -> Validation VError VSuccess
notEqualTo fn s pattern = if s /= pattern
  then Success VSuccess
  else Failure . VError $ ["\"" <> fn <> "\" must not be equal to " <> showJS pattern]

equalTo :: (Eq a, Show a) => JSString -> a -> a -> Validation VError VSuccess
equalTo fn s pattern = if s == pattern
  then Success VSuccess
  else Failure . VError $ ["\"" <> fn <> "\" must be equal to " <> showJS pattern]

notEmpty :: JSString -> JSString -> Validation VError VSuccess
notEmpty fn s = if Data.JSString.length s > 0
  then Success VSuccess
  else Failure . VError $ ["\"" <> fn <> "\" must not be empty"]

lengthGT :: JSString -> Int -> JSString -> Validation VError VSuccess
lengthGT fn n s = if Data.JSString.length s > n
  then Success VSuccess
  else Failure . VError $ ["\"" <> fn <> "\" min length must be > " <> showJS n]

lengthLT :: JSString -> Int -> JSString -> Validation VError VSuccess
lengthLT fn n s = if Data.JSString.length s < n
  then Success VSuccess
  else Failure . VError $ ["\"" <> fn <> "\" max length must be < " <> showJS n]

lengthBetween :: JSString -> Int -> Int -> JSString -> Validation VError VSuccess
lengthBetween fn minl maxl s = runValidation2 <$> lengthGT fn minl s <*> lengthLT fn maxl s

isPrintable :: JSString -> JSString -> Validation VError VSuccess
isPrintable fn s = if and $ isPrint <$> Data.JSString.unpack s
  then Success VSuccess
  else Failure . VError $ ["\"" <> fn <> "\" must be printable"]

isAlphanum :: JSString -> JSString -> Validation VError VSuccess
isAlphanum fn s = if and $ isAlphaNum <$> Data.JSString.unpack s
  then Success VSuccess
  else Failure . VError $ ["\"" <> fn <> "\" must be alphanum"]

validateMatch :: JSString -> JSString -> JSString -> JSString -> Validation VError VSuccess
validateMatch fn1 fn2 a b = if a == b
  then Success VSuccess
  else Failure . VError $ ["\"" <> fn1 <> "\" and \"" <> fn2 <> "\" do not match"]

showValidationErrors :: VError -> JSString
showValidationErrors (VError es) = Data.JSString.intercalate ", " es
