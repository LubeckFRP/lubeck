{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BD.Data.Ad where

import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson.Types
import           Data.Bifunctor   (bimap, first)
import           Data.Data
import           Data.Monoid
import           Data.Time.Clock  (UTCTime)
import qualified GHC.Generics     as GHC

import           GHCJS.Types      (JSString)

import           Lubeck.FRP       (Sink)
import           Lubeck.Util

import           BD.Api
import           BD.Data.AdTypes
import           BD.Types

type PGArray a = [a]

data Ad = Ad
  { fb_adset_id    :: Integer
  , campaign_id    :: Integer
  , fb_ad_id       :: Integer
  , fb_creative_id :: Integer
  , image_ids      :: PGArray Integer
  , ad_title       :: Text
  , ad_caption     :: Text
  , current_budget :: USDcents
  } deriving (GHC.Generic, Show)

instance FromJSON Ad
instance ToJSON Ad

getCampaignAds :: JSString -> JSString -> IO [Ad]
getCampaignAds unm campid =  unsafeGetAPI $ unm <> "/ads/" <> campid

getCampaignAdsOrError :: JSString -> JSString -> IO (Either AppError [Ad])
getCampaignAdsOrError unm campid = getAPIEither (unm <> "/ads/" <> campid) >>= return . bimap ApiError id

updateStatusOrError :: JSString -> Integer -> AdStatus -> IO (Either AppError Ok)
updateStatusOrError unm adId status = postAPIEither (unm <> "/ad-status/" <> showJS adId <> "/" <> showStatus status) () >>= return . first ApiError
  where showStatus Paused   = "paused"
        showStatus Running  = "running"
        showStatus Archived = "archived"

updateBudgetOrError :: JSString -> Integer -> USDcents -> IO (Either AppError Ok)
updateBudgetOrError unm adId budget = postAPIEither (unm <> "/set-ad-budget/" <> showJS adId <> "/" <> showJS budget) () >>= return . first ApiError
