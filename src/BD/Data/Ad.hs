{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BD.Data.Ad where

import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson.Types
import           Data.Bifunctor   (bimap)
import           Data.Data
import           Data.Monoid
import           Data.Time.Clock  (UTCTime)
import qualified GHC.Generics     as GHC

import           GHCJS.Types      (JSString)

import           Lubeck.FRP       (Sink)

import           BD.Api
import           BD.Data.AdTypes
import           BD.Types

type PGArray a = [a]

data Ad = Ad
  { fb_adset_id    :: Int
  , campaign_id    :: Int
  , fb_ad_id       :: Int
  , fb_creative_id :: Int
  , image_ids      :: PGArray Int
  , ad_title       :: Text
  , ad_caption     :: Text
  , current_budget :: USDcents
  } deriving (GHC.Generic)

instance FromJSON Ad
instance ToJSON Ad

getCampaignAds :: JSString -> JSString -> IO [Ad]
getCampaignAds unm campid =  unsafeGetAPI $ unm <> "/ads/" <> campid

getCampaignAdsOrError :: JSString -> JSString -> IO (Either AppError [Ad])
getCampaignAdsOrError unm campid = getAPIEither (unm <> "/ads/" <> campid) >>= return . bimap ApiError id
