
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, OverloadedStrings, GADTs, DeriveGeneric, DeriveDataTypeable #-}

module BD.Data.Account
    ( Account(..)
    , getUser
    ) where

import Control.Monad
import Data.Aeson -- TODO proper
import Data.Data
import Data.Text(Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Aeson.Types
import qualified GHC.Generics as GHC
import JavaScript.Web.XMLHttpRequest -- TODO
import GHCJS.Types (JSString)
import Data.Monoid


-- import BD.Data.Count
-- import BD.Data.Account
-- import BD.Data.SearchPost

data Account = Account
  { id              :: Int
  , username        :: Text
  , full_name       :: Text
  , profile_picture :: Maybe Text
  , website         :: Maybe Text
  , tier            :: Int
  , discovered      :: Bool
  , location        :: Maybe Text -- 3 letter country code ISO (+ US states)
  , bio             :: Maybe Text
  , sector_id       :: Maybe Int
  , is_private      :: Bool
  , numposts        :: Maybe Int
  , numfollowing    :: Maybe Int
  , p_is_male       :: Maybe Double
  , latest_count    :: Maybe Int
  } deriving (GHC.Generic,Show, Eq, Data, Typeable)

data Envelope a = Envelope { payload :: a } deriving (GHC.Generic,Show, Eq, Data, Typeable)

instance ToJSON a => ToJSON (Envelope a)
instance FromJSON a => FromJSON (Envelope a)

instance ToJSON Account where
  toJSON = Data.Aeson.Types.genericToJSON
    Data.Aeson.Types.defaultOptions { Data.Aeson.Types.omitNothingFields = True }

instance FromJSON Account

getUser :: JSString -> IO Account
getUser unm = do
  r <- xhrByteString r
  case contents r of
    Nothing          -> error "TODO no response"
    Just byteString  -> case Data.Aeson.decodeStrict byteString of
      Nothing -> error "TODO parse error"
      Just (Envelope x) -> return x
  where
    r = Request {
            reqMethod          = GET
          , reqURI             = "http://data.beautifuldestinations.com/api/v1/"
                                    <> unm <> "/account"
          , reqLogin           = Nothing
          , reqHeaders         = []
          , reqWithCredentials = False
          , reqData            = NoData
          }
