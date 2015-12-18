
module Interactions where

import Data.Aeson -- TODO proper

data Account
data Count
data SearchPost

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
    medium :: m
  } deriving (GHC.Generic,Show, Eq)

data InteractionMedia m where
  Shoutouts :: InteractionMedia (SearchPost)
--  Likes :: InteractionMedia (UL.UserLike)
--  Comments :: InteractionMedia (Com.Comment)

instance ToJSON m => ToJSON (Interaction m)
instance ToJSON m => ToJSON (InteractionSet m)


loadInteractionSet :: InteractionMedia m -> Maybe Account -> Maybe Account -> DB (InteractionSet m)
loadInteractionSet mode (maccFrom) (maccTo) = do
  inters <- loadInteractions mode maccFrom maccTo
  return $ InteractionSet maccFrom maccTo inters
