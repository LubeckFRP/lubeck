
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, OverloadedStrings, GADTs, DeriveGeneric, DeriveDataTypeable, CPP #-}

module BD.Data.Interaction
    ( loadShoutouts
    , Interaction(..)
    , InteractionSet(..)
    ) where

import Control.Monad
import Control.Monad.Except
import Data.Aeson -- TODO proper
import Data.Data
import Data.Time.Clock (UTCTime)
import qualified Data.Aeson.Types
import qualified GHC.Generics as GHC
import Data.ByteString(ByteString)
import Data.Maybe(fromMaybe)
import Data.Monoid

#ifdef __GHCJS__
import JavaScript.Web.XMLHttpRequest -- TODO
import GHCJS.Types(JSString)
#endif

import BD.Types
import BD.Data.Count
import BD.Data.Account
import BD.Data.SearchPost

#ifndef __GHCJS__
{-|
Faking JavaScript.Web.XMLHttpRequest for GHCI

TODO
- Implement this separately based on some client library
- Or the other way around, possibly using a servant-generated client
-}
type JSString = String
type Text = String
data Method = GET
data ReqData = NoData
data Request = Request {
  reqMethod :: Method,
  reqURI :: String,
  reqLogin :: (Maybe ()),
  reqHeaders:: [()],
  reqWithCredentials :: Bool,
  reqData :: ReqData
}
xhrText = undefined
xhrByteString :: Request -> IO (Response ByteString)
xhrByteString = undefined
data Response a = Response { contents              :: Maybe a
                           , status                :: Int
                           , getAllResponseHeaders :: IO JSString
                           , getResponseHeader     :: JSString -> IO (Maybe JSString)
                           }
#endif

data InteractionSet m = InteractionSet
  {
    from_account :: Maybe Account,
    to_account :: Maybe Account,
    interactions :: [Interaction m]
  } deriving (GHC.Generic)

data Interaction m = Interaction
  {
    target_counts :: [Count], -- the growth
    target_account :: Account,
    interaction_time :: UTCTime,
    medium :: m -- i.e. a post
  } deriving (GHC.Generic)

data InteractionMedia m where
  Shoutouts :: InteractionMedia SearchPost

instance ToJSON m => ToJSON (Interaction m)
instance ToJSON m => ToJSON (InteractionSet m)
instance FromJSON m => FromJSON (Interaction m)
instance FromJSON m => FromJSON (InteractionSet m)

-- | Monad for backend interaction. Currently same as IO, we should probably do some wrapping eventually.

data ResponseError = NoResponse String 
                   | ParseError String 
                  deriving (Show)

type DB = ExceptT ResponseError IO

interactionURI :: JSString -> JSString -> JSString
interactionURI fromAccName toAccName = baseURI <> fromAccName <> "/" <> toAccName <> "/shoutouts"
  where baseURI = "http://data.beautifuldestinations.com/api/v1/interactions/"

loadShoutouts :: Maybe JSString -> Maybe JSString -> DB (InteractionSet SearchPost)
loadShoutouts mFrom mTo = do
  r <- liftIO getFromAPI -- TODO params
  case contents r of
    Nothing          -> throwError $ NoResponse "API was unresponsive"
    Just byteString  -> case Data.Aeson.decodeStrict byteString of
      Nothing -> throwError $ ParseError "Could not decode API response as InteractionSet SearchPost" 
      Just x  -> return x
  where
    fromAccName = fromMaybe "any" mFrom
    toAccName   = fromMaybe "any" mTo
    getFromAPI = xhrByteString r
      where
        r = Request {
            reqMethod          = GET
          , reqURI             = interactionURI fromAccName toAccName
          , reqLogin           = Nothing
          , reqHeaders         = []
          , reqWithCredentials = False
          , reqData            = NoData
          }
