
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, OverloadedStrings, GADTs, DeriveGeneric, DeriveDataTypeable #-}

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

import BD.Data.Count
import BD.Data.Account
import BD.Data.SearchPost

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
