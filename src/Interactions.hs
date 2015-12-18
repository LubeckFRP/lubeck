
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, OverloadedStrings, GADTs, DeriveGeneric #-}

module Interactions where

import Data.Aeson -- TODO proper
import Data.Time.Clock (UTCTime)
import qualified GHC.Generics as GHC

data Account = Account deriving (Eq, Ord, Show)
data Count = Count deriving (Eq, Ord, Show)
data SearchPost = SearchPost deriving (Eq, Ord, Show)

{-
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

instance Generic         Account
instance HasDatatypeInfo Account
instance HasFieldNames   Account

instance FromRow Account where fromRow = gfromRow
instance ToRow   Account where toRow   = gtoRow

instance ToJSON Account where
  toJSON = Data.Aeson.Types.genericToJSON
    Data.Aeson.Types.defaultOptions { Data.Aeson.Types.omitNothingFields = True }

instance ToJSON (AsApiResponse Account) where
  toJSON (AsApiResponse a) = Beautilytics.Utils.deleteJSONField "discovered" $ toJSON a

instance FromJSON Account

instance FromJSON (AsApiResponse Account) where
  parseJSON v = do
    acc <- parseJSON $ Beautilytics.Utils.addJSONField "discovered" (toJSON True) v
    return $ AsApiResponse acc






data Count = Count
  { account_id :: Int
  , count_at   :: UTCTime
  , value      :: Int
  } deriving  (GHC.Generic,Show, Eq, Data, Typeable)

instance Generic         Count
instance HasDatatypeInfo Count
instance HasFieldNames   Count

instance FromRow Count where fromRow = gfromRow
instance ToRow   Count where toRow   = gtoRow

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
  , thumbnail_url    :: T.Text
  , url              :: T.Text
  , description      :: Maybe T.Text
  , created_at       :: UTCTime
  , comment_count    :: Int
  , like_count       :: Int
  , location_id      :: Maybe Int
  , follower_count   :: Maybe Int
  , username         :: T.Text
  , deleted          :: Bool
  , ig_web_url       :: Maybe T.Text
  , latitude         :: Maybe Double
  , longitude        :: Maybe Double
  } deriving (GHC.Generic,Show, Eq, Data, Typeable)

instance Generic         SearchPost
instance HasDatatypeInfo SearchPost
instance HasFieldNames   SearchPost

instance FromRow SearchPost where fromRow = gfromRow
instance ToRow   SearchPost where toRow   = gtoRow

instance FromJSON SearchPost
instance ToJSON   SearchPost

-}

type DB = IO

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


loadInteractionSet :: InteractionMedia m -> Maybe Account -> Maybe Account -> DB (InteractionSet m)
loadInteractionSet = undefined
