
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, OverloadedStrings, GADTs, DeriveGeneric, DeriveDataTypeable #-}

module Interactions where

import Control.Monad

import Data.Aeson -- TODO proper
import qualified Data.Aeson.Types

import Data.Time.Clock (UTCTime)
import qualified GHC.Generics as GHC
-- import GHC.Generics(Generic)
import Data.Text(Text)
import Data.Data

data Account = Account
  { xx_id              :: Int
  , xx_username        :: Text
  , xx_full_name       :: Text
  , xx_profile_picture :: Maybe Text
  , xx_website         :: Maybe Text
  , xx_tier            :: Int
  , xx_discovered      :: Bool
  , xx_location        :: Maybe Text -- 3 letter country code ISO (+ US states)
  , xx_bio             :: Maybe Text
  , xx_sector_id       :: Maybe Int
  , xx_is_private      :: Bool
  , xx_numposts        :: Maybe Int
  , xx_numfollowing    :: Maybe Int
  , xx_p_is_male       :: Maybe Double
  , xx_latest_count    :: Maybe Int
  } deriving (GHC.Generic,Show, Eq, Data, Typeable)

instance ToJSON Account where
  toJSON = Data.Aeson.Types.genericToJSON
    Data.Aeson.Types.defaultOptions { Data.Aeson.Types.omitNothingFields = True }

-- instance ToJSON (AsApiResponse Account) where
--   toJSON (AsApiResponse a) = Beautilytics.Utils.deleteJSONField "discovered" $ toJSON a
--
instance FromJSON Account
--
-- instance FromJSON (AsApiResponse Account) where
--   parseJSON v = do
--     acc <- parseJSON $ Beautilytics.Utils.addJSONField "discovered" (toJSON True) v
--     return $ AsApiResponse acc
--










data Count = Count
  { x_account_id :: Int
  , x_count_at   :: UTCTime
  , x_value      :: Int
  } deriving  (GHC.Generic,Show, Eq, Data, Typeable)

instance FromJSON Count where
    parseJSON (Object v) = Count <$>
                           v .:? "account_id" .!= 0 <*> -- since there is not account information in json
                           v .: "count_at" <*>
                           v .: "value"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero
instance ToJSON Count where
  toJSON (Count _ tm v) = object  ["count_at" .= tm, "value" .= v]


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

-- instance Generic         SearchPost
-- instance HasDatatypeInfo SearchPost
-- instance HasFieldNames   SearchPost
instance FromJSON SearchPost
instance ToJSON   SearchPost



data InteractionSet m = InteractionSet
  {
    from_account :: Maybe Account,
    to_account :: Maybe Account,
    interactions :: [Interaction m]
  } deriving (GHC.Generic,Show, Eq)

data Interaction m = Interaction
  {
    target_counts :: [Count],
    target_account :: Account,
    interaction_time :: UTCTime,
    medium :: m -- i.e. a post
  } deriving (GHC.Generic,Show, Eq)

data InteractionMedia m where
  Shoutouts :: InteractionMedia SearchPost

instance ToJSON m => ToJSON (Interaction m)
instance ToJSON m => ToJSON (InteractionSet m)



type DB = IO

loadInteractionSet :: InteractionMedia m -> Maybe Account -> Maybe Account -> DB (InteractionSet m)
loadInteractionSet = undefined
