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
  { sid :: Int
  , content :: SessionPage
  } deriving (GHC.Generic, Show, Eq)

instance FromJSON Session

data SessionPage = SessionPage
  { time_sent :: UTCTime
  , label :: IL.Label
  , imgs :: [I.Image]
  } deriving (GHC.Generic, Show, Eq)

instance FromJSON SessionPage

data SessionData = SessionData
  { session_data :: [SessionImage] }
  deriving (GHC.Generic)

instance ToJSON SessionData

data SessionImage = SessionImage
  { session_id :: Int
  , session_page :: Int
  , label_id :: Int
  , image_id :: Int
  , num_selected :: Maybe Int
  , time_selected :: Maybe UTCTime
  } deriving (GHC.Generic, Show, Eq)

instance ToJSON SessionImage

initializeSession' :: API -> Int -> IO Session
initializeSession' api n =
  unsafeGetAPI api ("label-refiner/session/new/" <> showJS n)

getSessionPage' :: API -> Int -> IO SessionPage
getSessionPage' api n =
  unsafeGetAPI api ("label-refiner/session/page/" <> showJS n)

postSessionData :: API -> SessionData -> IO (Either AppError Ok)
postSessionData api =
  fmap (first ApiError) . postAPIEither api "label-refiner/session/data"
