
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, OverloadedStrings, GADTs, DeriveGeneric, DeriveDataTypeable, CPP #-}

module BD.Data.Interaction
    (
    ) where

import Control.Monad
import Data.Aeson -- TODO proper
import Data.Data
import Data.Text(Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Aeson.Types
import qualified GHC.Generics as GHC

#ifdef __GHCJS__
-- import JavaScript.Web.XMLHttpRequest -- TODO
#endif

import BD.Data.Count
import BD.Data.Account
import BD.Data.SearchPost

#ifndef __GHCJS__
data Response a
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



type DB = IO

loadInteractionSet :: InteractionMedia m -> Maybe Account -> Maybe Account -> DB (InteractionSet m)
loadInteractionSet = undefined

getFromAPI :: IO (Response Text)
getFromAPI = xhrText r
  where
    r = Request {
        reqMethod          = GET
      , reqURI             = "http://data.beautifuldestinations.com/api/v1/interactions/tomjauncey/tomjauncey/shoutouts"
      , reqLogin           = Nothing
      , reqHeaders         = []
      , reqWithCredentials = False
      , reqData            = NoData
      }
