
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, OverloadedStrings, GADTs, DeriveGeneric, DeriveDataTypeable #-}

module BD.Api (
  getAPI,
  getAPI',
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
import qualified Data.JSString
import JavaScript.Web.XMLHttpRequest -- TODO

getAPI :: (FromJSON a, Monad m, MonadError s m, s ~ JSString, MonadIO m) => JSString -> m a
getAPI path = do
  result <- liftIO $ xhrByteString request
  case contents result of
    Nothing          -> throwError "getAPI: No response"
    Just byteString  -> case Data.Aeson.decodeStrict byteString of
      Nothing -> throwError "getAPI: Parse error"
      Just x  -> return x
  where
    request = Request {
            reqMethod          = GET
          , reqURI             = "http://data.beautifuldestinations.com/api/v1/" <> path
          , reqLogin           = Nothing
          , reqHeaders         = []
          , reqWithCredentials = False
          , reqData            = NoData
          }

getAPI' :: FromJSON a => JSString -> IO (Either JSString a)
getAPI' = runExceptT . getAPI

unsafeGetAPI :: FromJSON a => JSString -> IO a
unsafeGetAPI = fmap noLeft . getAPI'
  where
    noLeft (Right x) = x
    noLeft (Left  e) = error (Data.JSString.unpack e)

data Envelope a = Envelope { payload :: a } deriving (GHC.Generic,Show, Eq, Data, Typeable)

instance ToJSON a => ToJSON (Envelope a)
instance FromJSON a => FromJSON (Envelope a)
