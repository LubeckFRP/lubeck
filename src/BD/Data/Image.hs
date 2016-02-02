{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BD.Data.Image where

import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson.Types
import           Data.Data
import           Data.Monoid
import           Data.Time.Clock  (UTCTime)
import qualified GHC.Generics     as GHC

import           GHCJS.Types      (JSString)

import           BD.Api
import           BD.Data.AdTypes
import           BD.Types

import           Data.Bifunctor   (first)

data Image = Image
  { id            :: Int
  , fb_image_id   :: Maybe Int
  , fb_image_url  :: Maybe Text
  , fb_image_hash :: Maybe Text
  , fb_thumb_url  :: Maybe Text
  , ig_post_id    :: Maybe Int
  , localpath     :: Maybe Text
  , prediction    :: Maybe Double

  } deriving (GHC.Generic, Show)


instance FromJSON Image
instance ToJSON Image

getAllImages :: Text -> IO [Image]
getAllImages unm = unsafeGetAPI $ unm <> "/ad-images"

getAllImagesOrError :: Text -> IO (Either AppError [Image])
getAllImagesOrError unm = getAPIEither (unm <> "/ad-images") >>= return . first ApiError
