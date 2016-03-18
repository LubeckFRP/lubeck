
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BD.Data.Count
    ( Count(..)
    ) where

import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson.Types
import           Data.Data
import           Data.Time.Clock  (UTCTime)
import qualified GHC.Generics     as GHC

data Count = Count
  { account_id :: Int
  , count_at   :: UTCTime
  , value      :: Int
  } deriving  (GHC.Generic,Show)

instance FromJSON Count where
    parseJSON (Object v) = Count <$>
                           v .:? "account_id" .!= 0 <*> -- since there is not account information in json
                           v .: "count_at" <*>
                           v .: "value"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero
instance ToJSON Count where
  toJSON (Count _ tm v) = object  ["count_at" .= tm, "value" .= v]
