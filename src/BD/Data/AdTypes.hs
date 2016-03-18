{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}

module BD.Data.AdTypes where

import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson.Types
import           Data.Data
import           Data.Monoid
import           Data.Time.Clock  (UTCTime)
import qualified GHC.Generics     as GHC

import           GHCJS.Types      (JSString)

import           BD.Api

type USDcents = Int

type URL = String

type FBGraphId = Integer

data Objective = VideoViews
               | ClicksToWebsite URL
               deriving (GHC.Generic, Show)

instance FromJSON Objective
instance ToJSON Objective

data AdStatus = Paused | Running | Archived deriving (GHC.Generic, Show, Eq)

instance ToJSON AdStatus
instance FromJSON AdStatus
