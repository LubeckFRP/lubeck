{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}

module BD.Data.Account
    ( Account(..)
    , getUser
    , getUserOrError
    ) where

import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson.Types
import           Data.Aeson.Types
import           Data.Data
import qualified Data.HashMap.Strict       as H
import           Data.Bifunctor (bimap, first)
import           Data.Foldable (asum)
import           Data.Monoid
import           Data.String      (fromString)
import           Data.Time.Clock  (UTCTime)
import qualified GHC.Generics     as GHC

import           GHCJS.Types      (JSString)

import           BD.Api
import           BD.Types

import           Lubeck.FRP

-- import BD.Data.Count
-- import BD.Data.Account
-- import BD.Data.SearchPost

data Account = Account
  { id              :: Int
  , username        :: Text
  , full_name       :: Text
  , profile_picture :: Maybe Text
  , website         :: Maybe Text
  , tier            :: Int
  , discovered      :: Bool
  , location        :: Maybe Text -- 3 letter country code ISO (+ US states)
  , bio             :: Maybe Text
  , sector_id       :: Maybe Int
  , is_private      :: Bool
  , numposts        :: Maybe Int
  , numfollowing    :: Maybe Int
  , p_is_male       :: Maybe Double
  , latest_count    :: Maybe Int
  } deriving (GHC.Generic, Show, Ord, Eq)

instance ToJSON Account where
  toJSON = Data.Aeson.Types.genericToJSON
    Data.Aeson.Types.defaultOptions { Data.Aeson.Types.omitNothingFields = True }

instance FromJSON Account

getUser :: JSString -> IO Account
getUser unm = fmap payload $ unsafeGetAPI BD.Api.defaultAPI $ unm <> "/account"

getUserOrError :: JSString -> IO (Either AppError Account)
getUserOrError unm = getAPIEither BD.Api.defaultAPI (unm <> "/account") >>= return . bimap ApiError payload
