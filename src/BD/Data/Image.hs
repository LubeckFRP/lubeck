{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

module BD.Data.Image where

import Control.Monad
import Data.Aeson -- TODO proper
import Data.Data
import Data.Time.Clock (UTCTime)
import qualified Data.Aeson.Types
import qualified GHC.Generics as GHC
import Data.Monoid

import GHCJS.Types (JSString)

import BD.Api
import BD.Types
import BD.Data.AdTypes

data Image = Image
  { id            :: Int
  , fb_image_id   :: Maybe Int
  , fb_image_url  :: Maybe Text
  , fb_image_hash :: Maybe Text
  , fb_thumb_url  :: Maybe Text
  , ig_post_id    :: Maybe Int
  , localpath     :: Maybe Text
  } deriving (GHC.Generic)


instance FromJSON Image
instance ToJSON Image
