
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, OverloadedStrings, GADTs, DeriveGeneric, DeriveDataTypeable #-}

module BD.Api (
  unsafeGetAPI,
  Envelope(..),
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson
import qualified Data.Aeson.Types
import Data.Data
import Data.Monoid
import Data.Text(Text)
import Data.Time.Clock (UTCTime)

import qualified GHC.Generics as GHC

import GHCJS.Types (JSString)
import JavaScript.Web.XMLHttpRequest -- TODO

getAPI :: (FromJSON a, Monad m, MonadError s m, s ~ JSString, MonadIO m) => JSString -> m a
getAPI path = do
  result <- xhrByteString request
  case contents result of
    Nothing          -> fail "getAPI: No response"
    Just byteString  -> case Data.Aeson.decodeStrict byteString of
      Nothing -> fail "getAPI: Parse error"
      Just x  -> return x
  where
    request = Request {
            reqMethod          = GET
          , reqURI             = "http://data.beautifuldestinations.com/api/v1/" <> q
          , reqLogin           = Nothing
          , reqHeaders         = []
          , reqWithCredentials = False
          , reqData            = NoData
          }

unsafeGetAPI :: FromJSON a => JSString -> IO a
unsafeGetAPI q = do
  result <- xhrByteString request
  case contents result of
    Nothing          -> error "TODO no response"
    Just byteString  -> case Data.Aeson.decodeStrict byteString of
      Nothing -> error "TODO parse error"
      Just x -> return x
  where
    request = Request {
            reqMethod          = GET
          , reqURI             = "http://data.beautifuldestinations.com/api/v1/" <> q
          , reqLogin           = Nothing
          , reqHeaders         = []
          , reqWithCredentials = False
          , reqData            = NoData
          }

data Envelope a = Envelope { payload :: a } deriving (GHC.Generic,Show, Eq, Data, Typeable)

instance ToJSON a => ToJSON (Envelope a)
instance FromJSON a => FromJSON (Envelope a)
