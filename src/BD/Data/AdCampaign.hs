{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

module BD.Data.AdCampaign where

import Control.Monad
import Data.Aeson -- TODO proper
import Data.Data
import Data.Text(Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Aeson.Types
import qualified GHC.Generics as GHC
import GHCJS.Types (JSString)
import Data.Monoid
import BD.Api
import BD.Data.AdTypes

data AdCampaign = AdCampaign
  { fbid                 :: FBGraphId
  , fb_user_id           :: Int
  , target_account_id    :: Int
  , objective            :: Objective
  , daily_budget         :: USDcents
  , status               :: AdStatus
  , campaign_name        :: Text
  } deriving (GHC.Generic)

instance FromJSON AdCampaign
instance ToJSON AdCampaign
