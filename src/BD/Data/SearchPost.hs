
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, OverloadedStrings, GADTs, DeriveGeneric, DeriveDataTypeable #-}

module BD.Data.SearchPost
    ( SearchPost(..)
    ) where

import Control.Monad
import Data.Aeson -- TODO proper
import Data.Data
import Data.Time.Clock (UTCTime)
import qualified Data.Aeson.Types
import qualified GHC.Generics as GHC

import BD.Types

data SearchPost = SearchPost
  { post_id          :: Integer
  , account_id       :: Integer
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
  } deriving (GHC.Generic)

instance FromJSON SearchPost
instance ToJSON   SearchPost
