{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, OverloadedStrings, GADTs, DeriveGeneric, DeriveDataTypeable #-}

module BD.Api (
  unsafeGetAPI,
  Envelope(..),
  ) where

import Control.Monad
import Data.Aeson (FromJSON(..), ToJSON(..))
import Control.Monad.Except
import Control.Monad.IO.Class

import Data.Data
import Data.Text(Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Aeson.Types
import qualified GHC.Generics as GHC
import JavaScript.Web.XMLHttpRequest -- TODO
import GHCJS.Types (JSString)
import Data.Monoid

getAPI :: (FromJSON a, Monad m, MonadError s m, s ~ JSString, MonadIO m) => JSString -> m a
getAPI path = liftIO $ unsafeGetAPI path

unsafeGetAPI :: FromJSON a => JSString -> IO a
unsafeGetAPI q = do
  r <- xhrByteString r
  case contents r of
    Nothing          -> error "TODO no response"
    Just byteString  -> case Data.Aeson.decodeStrict byteString of
      Nothing -> error "TODO parse error"
      Just x -> return x
  where
    r = Request {
            reqMethod          = GET
          , reqURI             = "http://data.beautifuldestinations.com/api/v1/"
                                    <> q
          , reqLogin           = Nothing
          , reqHeaders         = []
          , reqWithCredentials = False
          , reqData            = NoData
          }

data Envelope a = Envelope { payload :: a } deriving (GHC.Generic,Show, Eq, Data, Typeable)

instance ToJSON a => ToJSON (Envelope a)
instance FromJSON a => FromJSON (Envelope a)
