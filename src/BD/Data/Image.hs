{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BD.Data.Image where

import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson.Types
import           Data.Data
import           Data.Monoid
import           Data.String      (fromString)
import           Data.Time.Clock  (UTCTime)
import qualified GHC.Generics     as GHC

import           GHCJS.Types      (JSString)

import           BD.Api
import           BD.Data.AdTypes
import           BD.Types

import           Data.Bifunctor   (first)
import JavaScript.Web.XMLHttpRequest (FormDataVal(..))

data Image = Image
  { id            :: Int
  , fb_image_id   :: Maybe Int
  , fb_image_url  :: Maybe Text
  , fb_image_hash :: Maybe Text
  , fb_thumb_url  :: Maybe Text
  , ig_post_id    :: Maybe Int
  , localpath     :: Maybe Text
  , prediction    :: Maybe Double
  , image_width   :: Maybe Int
  , image_height  :: Maybe Int
  } deriving (GHC.Generic, Show, Eq, Ord)


showJS :: Show a => a -> JSString
showJS = fromString . show

instance FromJSON Image
instance ToJSON Image

getAllImages :: Text -> IO [Image]
getAllImages unm = unsafeGetAPI BD.Api.defaultAPI $ unm <> "/ad-images"

getAllImagesOrError :: Text -> IO (Either AppError [Image])
getAllImagesOrError unm = getAPIEither BD.Api.defaultAPI (unm <> "/ad-images") >>= return . first ApiError

deleteImageOrError :: Text -> Int -> IO (Either AppError Ok)
deleteImageOrError unm imageId = deleteAPIEither BD.Api.defaultAPI (unm <> "/ad-image/" <> showJS imageId) >>= return . first ApiError

uploadImagesOrError :: Text -> [(JSString, FormDataVal)] -> IO (Either AppError Ok)
uploadImagesOrError unm files = postFileAPIEither BD.Api.defaultAPI (unm <> "/ad-image") files >>= return . first ApiError

enhanceImageOrError :: Text -> Int -> IO (Either AppError Ok)
enhanceImageOrError unm imageId = postAPIEither BD.Api.defaultAPI (unm <> "/ad-image/" <> showJS imageId <> "/enhance-async") () >>= return . first ApiError

suitableForAd :: Image -> Bool
suitableForAd img
  = case (image_width img, image_height img) of
      (Just w,Just h) -> w >= h
      _ -> False
