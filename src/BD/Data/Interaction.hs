
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, OverloadedStrings, GADTs, DeriveGeneric, DeriveDataTypeable, CPP #-}

module BD.Data.Interaction
    ( getFromAPI -- TODO
    , loadInteractionSetPosts
    ) where

import Control.Monad
import Data.Aeson -- TODO proper
import Data.Data
import Data.Text(Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Aeson.Types
import qualified GHC.Generics as GHC
import Data.ByteString(ByteString)

#ifdef __GHCJS__
import JavaScript.Web.XMLHttpRequest -- TODO
#endif

import BD.Data.Count
import BD.Data.Account
import BD.Data.SearchPost

#ifndef __GHCJS__
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
type JSString = ()
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
  } deriving (GHC.Generic,Show, Eq)

data Interaction m = Interaction
  {
    target_counts :: [Count],
    target_account :: Account,
    interaction_time :: UTCTime,
    medium :: m -- i.e. a post
  } deriving (GHC.Generic,Show, Eq)

data InteractionMedia m where
  Shoutouts :: InteractionMedia SearchPost

instance ToJSON m => ToJSON (Interaction m)
instance ToJSON m => ToJSON (InteractionSet m)
instance FromJSON m => FromJSON (Interaction m)
instance FromJSON m => FromJSON (InteractionSet m)



type DB = IO

loadInteractionSetPosts :: InteractionMedia SearchPost -> Maybe Account -> Maybe Account -> DB (InteractionSet SearchPost)
loadInteractionSetPosts media mFrom mTo = do
  r <- getFromAPI -- TODO params
  case contents r of
    Nothing          -> error "TODO no response"
    Just byteString  -> case Data.Aeson.decodeStrict byteString of
      Nothing -> error "TODO parse error"
      Just x  -> return x

getFromAPI :: DB (Response ByteString)
getFromAPI = xhrByteString r
  where
    r = Request {
        reqMethod          = GET
      , reqURI             = "http://data.beautifuldestinations.com/api/v1/interactions/tomjauncey/any/shoutouts"
      , reqLogin           = Nothing
      , reqHeaders         = []
      , reqWithCredentials = False
      , reqData            = NoData
      }
