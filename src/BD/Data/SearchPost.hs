{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BD.Data.SearchPost
    ( SearchPost(..)
    , TrackedHashtag(..)
    , getTrackedHashtags
    ) where

import           BD.Api
import           BD.Data.AdTypes
import           BD.Types
import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson.Types
import           Data.Data
import           Data.Time.Clock               (UTCTime)
import qualified GHC.Generics                  as GHC

import           Data.Bifunctor                (first)
import           JavaScript.Web.XMLHttpRequest (FormDataVal (..))


data SearchPost = SearchPost
  { post_id        :: Integer
  , account_id     :: Integer
  , thumbnail_url  :: Text
  , url            :: Text
  , description    :: Maybe Text
  , created_at     :: UTCTime
  , comment_count  :: Int
  , like_count     :: Int
  , location_id    :: Maybe Int
  , follower_count :: Maybe Int
  , username       :: Text
  , deleted        :: Bool
  , ig_web_url     :: Maybe Text
  , latitude       :: Maybe Double
  , longitude      :: Maybe Double
  } deriving (GHC.Generic, Eq, Ord, Show)

instance FromJSON SearchPost
instance ToJSON   SearchPost

data TrackedHashtag = TrackedHashtag
  { post_count :: Int
  , tag        :: Text
  } deriving (GHC.Generic, Show)

instance FromJSON TrackedHashtag
instance ToJSON   TrackedHashtag

getTrackedHashtags :: IO (Either AppError [TrackedHashtag])
getTrackedHashtags = getAPIEither defaultAPI "hashtags" >>= return . first ApiError
