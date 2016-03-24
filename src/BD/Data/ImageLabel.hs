{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BD.Data.ImageLabel where

import           Control.Applicative

import           Data.Aeson
import qualified Data.Aeson.Types
import           Data.Data
import           Data.Monoid
import           Data.JSString    (JSString, pack)
import           Data.String      (fromString)
import           Data.Time.Clock  (UTCTime)
import           Data.Bifunctor   (first)
import           Data.Text        (Text, unpack)
import           Prelude hiding   (id)

import qualified GHC.Generics     as GHC

import           BD.Api
import           BD.Types hiding (Text)

import           BD.Data.ImageLR  (Image)


data Label = Label
 { id :: Int
 , name :: Text
 } deriving (GHC.Generic, Show, Eq)

instance FromJSON Label
instance ToJSON   Label

data ImageLabel = ImageLabel
 { image_id :: Int
 , label_id :: Int
 , confidence :: Int
 , label_src :: Int
 } deriving (GHC.Generic, Show, Eq)

instance FromJSON ImageLabel
instance ToJSON ImageLabel

text2JS :: Text -> JSString
text2JS = pack . unpack

showJS :: Show a => a -> JSString
showJS = fromString . show

getRandomLabel :: API -> IO (Either AppError Label)
getRandomLabel api = first ApiError <$> getAPIEither api "label-refiner/labels/random"

getNimagesWithLabel :: API -> Int -> Label -> IO (Either AppError [Image])
getNimagesWithLabel api n label = liftA (first ApiError) $
  getAPIEither api ("label-refiner/images/"<>showJS (id label)<>"/"<>showJS n)
