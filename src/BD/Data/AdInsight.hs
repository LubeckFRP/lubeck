{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BD.Data.AdInsight where

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
import           BD.Data.Ad(Ad)
import qualified BD.Data.Ad
import           BD.Types

data AdInsight = AdInsight
  { fb_adset_id           :: Integer
  , captured_at           :: UTCTime
  , current_budget        :: USDcents
  , insight               :: Value
  , spend                 :: Double
  , reach                 :: Integer
  , unique_impressions    :: Integer
  , unique_clicks         :: Integer
  , unique_ctr            :: Double
  , call_to_action_clicks :: Integer
  } deriving (GHC.Generic, Show)

instance FromJSON AdInsight
instance ToJSON AdInsight

getAdPerformance :: JSString -> Integer -> IO (Either AppError [AdInsight])
getAdPerformance unm adId =  getAPIEither (BD.Api.defaultAPI) (unm <> "/ad-performance/" <> showJS adId) >>= return . first ApiError
