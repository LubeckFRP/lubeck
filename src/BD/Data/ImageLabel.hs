{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BD.Data.ImageLabel where

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

import           Data.Bifunctor   (first)
import           Data.Text        (Text)

data Label = Label
 { id :: Int
 , name :: Text
 } deriving (GHC.Generic, Show, Eq)

instance FromJSON        Label
instance ToJSON          Label

data ImageLabel = ImageLabel
 { image_id :: Int
 , label_id :: Int
 , confidence :: Int
 , label_src :: Int
 } deriving (GHC.Generic, Show, Eq)

instance FromJSON ImageLabel
instance ToJSON ImageLabel

getRandomLabel :: API -> IO (Either AppError Label)
getRandomLabel api = getAPIEither api "label-refiner/labels/random" >>= return . first ApiError

