{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BD.Data.AdCampaign where

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

getUserCampaigns :: JSString -> IO [AdCampaign]
getUserCampaigns unm = fmap payload $ unsafeGetAPI $ unm <> "/ad-campaigns"

getUserCampaignsOrError :: JSString -> IO (Either AppError [AdCampaign])
getUserCampaignsOrError unm = getAPIEither (unm <> "/ad-campaigns") >>= return . bimap ApiError payload
