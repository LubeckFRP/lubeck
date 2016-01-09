{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings, RecordWildCards, FlexibleContexts #-}

module BD.Data.AdTypes where

import Control.Monad
import Data.Aeson -- TODO proper
import Data.Data
import Data.Time.Clock (UTCTime)
import qualified Data.Aeson.Types
import qualified GHC.Generics as GHC
import Data.Monoid

import GHCJS.Types (JSString)

import BD.Api

type USDcents = Int

type URL = String

type FBGraphId = Integer

data Objective = VideoViews
               | ClicksToWebsite URL
               deriving (GHC.Generic)

instance FromJSON Objective
instance ToJSON Objective

data AdStatus = Paused | Running deriving (GHC.Generic)

instance ToJSON AdStatus
instance FromJSON AdStatus
