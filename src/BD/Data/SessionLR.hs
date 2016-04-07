{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BD.Data.SessionLR where

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
import           Lubeck.Util      (showJS)
import           BD.Types hiding  (Text)

import qualified BD.Data.ImageLR as I
import qualified BD.Data.ImageLabel as IL

data Session = Session
  { id :: Int
  , start_time :: UTCTime 
  , page_data :: SPageData
  } deriving (GHC.Generic, Show, Eq)

instance FromJSON Session

data SPageData = SPageData 
  { time_req :: UTCTime
  , label :: IL.Label
  , images :: [I.Image]
  } deriving (GHC.Generic, Show, Eq)

instance FromJSON SPageData

data SessionPage = SessionPage
  { time_sent :: UTCTime
  , page_number :: Int
  , session_id :: Int
  , label_id :: Int 
  , session_images :: [SessionImage]
  } deriving (GHC.Generic, Show, Eq)

instance ToJSON SessionPage

data SessionImage = SessionImage
  { image_id :: Int
  , time_selected :: Maybe UTCTime
  , num_selected :: Maybe Int
  } deriving (GHC.Generic, Show, Eq) 

instance ToJSON SessionImage

initializeSession :: API -> Int -> IO (Either AppError Session)
initializeSession api n =
  first ApiError <$> getAPIEither api ("label-refiner/session/init/" <> showJS n)

initializeSession' :: API -> Int -> IO Session
initializeSession' api n =
  unsafeGetAPI api ("label-refiner/session/init/" <> showJS n)

getNewPage :: API -> Int -> IO (Either AppError SPageData)
getNewPage api n =
  first ApiError <$> getAPIEither api ("label-refiner/session/getpage/" <> showJS n)

getNewPage' :: API -> Int -> IO SPageData 
getNewPage' api n =
  unsafeGetAPI api ("label-refiner/session/getpage/" <> showJS n)

postSessionPage :: API -> SessionPage -> IO (Either AppError Ok)
postSessionPage api =
  fmap (first ApiError) . postAPIEither api "label-refiner/session/submit"
