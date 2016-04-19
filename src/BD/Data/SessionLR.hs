{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BD.Data.SessionLR where

import           Control.Applicative

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Data
import           Data.Monoid
import           Data.JSString    (JSString)
import           Data.Time.Clock  (UTCTime)
import           Data.Bifunctor   (first)
import           Data.Text        (Text, unpack, pack)
import           Data.UUID        (UUID, fromString)
import           Prelude hiding   (id)

import qualified GHC.Generics     as GHC

import           BD.Api
import           Lubeck.Util      (showJS)
import           BD.Types hiding  (Text)

import qualified BD.Data.ImageLR as I
import qualified BD.Data.ImageLabel as IL

instance ToJSON UUID where
  toJSON = String . pack . show

instance FromJSON UUID where
  parseJSON json@(String t) = 
    let uuidString = unpack t
    in case fromString uuidString of
         Just uuid -> pure uuid 
         Nothing   -> typeMismatch "UUID" json 
  parseJSON unknown = typeMismatch "UUID" unknown

data SPageData = SPageData 
  { time_req :: UTCTime
  , label :: IL.Label
  , images :: [I.Image]
  } deriving (GHC.Generic, Eq)
instance FromJSON SPageData

data CPageData = CPageData
  { session_page :: SessionPage
  , session_images :: [SessionImage]
  } deriving (GHC.Generic, Eq)
instance ToJSON CPageData

data SessionPage = SessionPage
  { pageId :: Maybe Int -- Nothing by Default 
  , timeSent :: UTCTime
  , pageNumber :: Int
  , sessionId :: UUID 
  , labelId :: Int 
  } deriving (GHC.Generic, Eq)
instance ToJSON SessionPage

data SessionImage = SessionImage
  { sessionPageId :: Maybe Int -- Nothing by Default 
  , imageId :: Int
  , timeSelected :: Maybe UTCTime
  , numSelected :: Maybe Int
  } deriving (GHC.Generic, Eq) 
instance ToJSON SessionImage

initializeSession :: API -> Int -> IO (Either AppError (UUID, SPageData))
initializeSession api n =
  first ApiError <$> getAPIEither api ("session/init/" <> showJS n)

initializeSession' :: API -> Int -> IO (UUID, SPageData) 
initializeSession' api n =
  unsafeGetAPI api ("session/init/" <> showJS n)

getNewPage :: API -> Int -> IO (Either AppError SPageData)
getNewPage api n =
  first ApiError <$> getAPIEither api ("session/page/" <> showJS n)

getNewPage' :: API -> Int -> IO SPageData 
getNewPage' api n =
  unsafeGetAPI api ("session/page/" <> showJS n)

postCPageData :: API -> CPageData -> IO (Either AppError Ok)
postCPageData api =
  fmap (first ApiError) . postAPIEither api "session/submit"
