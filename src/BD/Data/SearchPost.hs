
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, OverloadedStrings, GADTs, DeriveGeneric, DeriveDataTypeable #-}

module BD.Data.SearchPost
    ( SearchPost(..)
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

data SearchPost = SearchPost
  { post_id          :: Int
  , account_id       :: Int
  , thumbnail_url    :: Text
  , url              :: Text
  , description      :: Maybe Text
  , created_at       :: UTCTime
  , comment_count    :: Int
  , like_count       :: Int
  , location_id      :: Maybe Int
  , follower_count   :: Maybe Int
  , username         :: Text
  , deleted          :: Bool
  , ig_web_url       :: Maybe Text
  , latitude         :: Maybe Double
  , longitude        :: Maybe Double
  } deriving (GHC.Generic, Show, Eq, Data, Typeable)

instance FromJSON SearchPost
instance ToJSON   SearchPost
