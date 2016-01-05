
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, OverloadedStrings, GADTs, DeriveGeneric, DeriveDataTypeable #-}

module BD.Data.Count
    ( Count(..)
    ) where

import Control.Monad
import Data.Aeson -- TODO proper
import Data.Data
import Data.Text(Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Aeson.Types
import qualified GHC.Generics as GHC

-- import BD.Data.Count
-- import BD.Data.Account
-- import BD.Data.SearchPost

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
