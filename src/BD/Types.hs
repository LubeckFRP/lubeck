{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BD.Types where

import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson.Types
import           Data.Data
import           Data.Monoid
import           Data.Text          (Text)
import           Data.Time.Clock    (UTCTime)
import qualified GHC.Generics       as GHC

import           Data.JSString.Text (textFromJSString, textToJSString)
import           GHCJS.Types        (JSString)

type Text = JSString

instance ToJSON JSString where
  toJSON = toJSON . textFromJSString

instance FromJSON JSString where
  parseJSON = fmap textToJSString . parseJSON

data AppError = ApiError JSString | BLError JSString | NotImplementedError JSString
  -- deriving (Show)
