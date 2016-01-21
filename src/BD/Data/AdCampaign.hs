{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

module BD.Data.AdCampaign where

import Control.Monad
import Data.Aeson -- TODO proper
import Data.Data
import Data.Time.Clock (UTCTime)
import qualified Data.Aeson.Types
import qualified GHC.Generics as GHC
import Data.Monoid

import GHCJS.Types (JSString)

import Lubeck.FRP (Sink)

import BD.Api
import BD.Types
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

getUserCampaigns :: JSString -> IO [AdCampaign]
getUserCampaigns unm = fmap payload $ unsafeGetAPI $ unm <> "/ad-campaigns"

getUserCampaignsOrError :: Sink (Maybe AppError) -> JSString -> IO (Maybe [AdCampaign])
getUserCampaignsOrError errorSink unm  = do
  x <- getAPIEither $ unm <> "/ad-campaigns"
  case x of
    Right y -> return $ Just $ payload y
    Left z -> do
      errorSink $ Just $ ApiError z
      return Nothing
