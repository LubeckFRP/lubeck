{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

module BD.Data.Ad where

import Control.Monad
import Data.Aeson -- TODO proper
import Data.Data
import Data.Time.Clock (UTCTime)
import qualified Data.Aeson.Types
import qualified GHC.Generics as GHC
import GHCJS.Types (JSString)
import Data.Monoid
import BD.Api
import BD.Types
import BD.Data.AdTypes

type PGArray a = [a]

data Ad = Ad
  { fb_adset_id          :: Int
  , campaign_id          :: Int
  , fb_ad_id             :: Int
  , fb_creative_id       :: Int
  , image_ids            :: PGArray Int
  , ad_title             :: Text
  , ad_caption           :: Text
  , current_budget       :: USDcents
  } deriving (GHC.Generic)

instance FromJSON Ad
instance ToJSON Ad

getCampaignAds :: JSString -> JSString -> IO [Ad]
getCampaignAds unm campid =  getAPI $ unm <> "/ads/" <> campid
