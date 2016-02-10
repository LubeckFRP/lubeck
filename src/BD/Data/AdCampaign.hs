{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BD.Data.AdCampaign where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson.Types
import           Data.Aeson.Types
import           Data.Bifunctor   (bimap)
import           Data.Data
import qualified Data.HashMap.Strict       as H
import           Data.Monoid
import           Data.Time.Clock  (UTCTime)
import qualified GHC.Generics     as GHC

import           GHCJS.Types      (JSString)

import           Lubeck.FRP       (Sink)
import           Lubeck.Util      (showJS)

import           BD.Api
import           BD.Data.AdTypes
import           BD.Types

data AdCampaign = AdCampaign
  { fbid              :: FBGraphId
  , fb_user_id        :: Int
  , target_account_id :: Int
  , objective         :: Objective
  , daily_budget      :: USDcents
  , status            :: AdStatus
  , campaign_name     :: Text
  } deriving (GHC.Generic)

instance FromJSON AdCampaign
instance ToJSON AdCampaign


data AdCampaignPerformance = AdCampaignPerformance
  { call_to_action_clicks :: Int
  , captured_at           :: JSString
  , current_budget        :: USDcents
  , fb_adset_id           :: FBGraphId
  -- , insight            :: {spend : 33.08, total_unique_actions: 1041, frequency: 1.0552570045535, unique_impressions: 24377,…}
  , reach                 :: Int
  , spend                 :: Double
  , unique_clicks         :: Int
  , unique_ctr            :: Double
  , unique_impressions    :: Int
  } deriving (GHC.Generic)

instance FromJSON AdCampaignPerformance
instance ToJSON AdCampaignPerformance

data AdOk = AdOk AdCampaignPerformance | AdNok JSString deriving (GHC.Generic)

parseResponse :: Bool -> Bool -> Object -> Parser AdOk
parseResponse hasId hasError obj
    | hasId   = AdOk <$> parseJSON (Object obj)
    | hasError  = AdNok <$> obj .: "error"
    | otherwise = pure $ AdNok $ "Unknown write response: " <> showJS obj

instance FromJSON AdOk where
    parseJSON (Object o) = parseResponse hasId hasError o
      where
        hasId    = H.member "fb_adset_id" o
        hasError = H.member "error" o


getUserCampaigns :: JSString -> IO [AdCampaign]
getUserCampaigns unm = fmap payload $ unsafeGetAPI $ unm <> "/ad-campaigns"

getUserCampaignsOrError :: JSString -> IO (Either AppError [AdCampaign])
getUserCampaignsOrError unm = getAPIEither (unm <> "/ad-campaigns") >>= return . bimap ApiError payload

getCampaignPerformanceOrError :: JSString -> FBGraphId -> IO (Either AppError AdOk)
getCampaignPerformanceOrError unm cId = getAPIEither (unm <> "/ad-performance-latest/" <> showJS cId) >>= return . bimap ApiError id
