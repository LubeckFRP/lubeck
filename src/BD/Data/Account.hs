{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, OverloadedStrings, GADTs, DeriveGeneric, DeriveDataTypeable #-}

module BD.Data.Account
    ( Account(..)
    , getUser
    , getUserOrError
    ) where

import Control.Monad
import Data.Aeson -- TODO proper
import Data.Data
import Data.Time.Clock (UTCTime)
import qualified Data.Aeson.Types
import qualified GHC.Generics as GHC
import Data.Monoid
import Data.String (fromString)

import GHCJS.Types (JSString)

import BD.Api
import BD.Types

import Lubeck.FRP

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
  } deriving (GHC.Generic)

instance ToJSON Account where
  toJSON = Data.Aeson.Types.genericToJSON
    Data.Aeson.Types.defaultOptions { Data.Aeson.Types.omitNothingFields = True }

instance FromJSON Account

getUser :: JSString -> IO Account
getUser unm = fmap payload $ unsafeGetAPI $ unm <> "/account"

showJS :: Show a => a -> JSString
showJS = fromString . show

getUserOrError :: Sink (Maybe AppError) -> JSString -> IO (Maybe Account)
getUserOrError errorSink username = do
  x <- getAPIEither $ username <> "/account"
  case x of
    Right y -> return $ Just $ payload y
    Left z -> do
      errorSink $ Just $ ApiError z
      return Nothing
