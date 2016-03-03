
{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module BD.Query.AccountQuery (
  Query(..),
  AccountQuery(..),
  SimpleAccountQuery(..),
  AccountOrder(..),
  SortDirection(..),
  defSimpleAccountQuery,
  complexifyAccountQuery,
) where

import Data.Monoid
import Data.Aeson (ToJSON(..), Value(..), object)
import Data.Time.Calendar (Day(..))
import qualified Data.Vector as V
import Data.Interval (Interval, interval, whole, Extended(..), lowerBound, upperBound)

import qualified Data.JSString

import BD.Types
import Lubeck.Util (formatDateFromUTC)

data Query = AccountQuery AccountQuery
  deriving (Eq, Ord, Show)

instance ToJSON Query where
  toJSON x = case x of
    AccountQuery pq -> inObjectNamed "account" $ toJSON pq


data AccountQuery
  = AccountQueryKeyword Text
  | AccountQueryUsername Text
  | AccountQueryFollows Text
  | AccountQueryMentionedBy Text
  | AccountQueryMentions Text
  | AccountQueryFollowers Ordering Int
  | AccountQueryNumberPosts Ordering Int
  | AccountQueryTier Ordering Int

  | AccountQueryLimit Int
  | AccountQueryOffset Int
  | AccountQueryOrderBy AccountOrder
  | AccountQueryOrderDirection SortDirection

  | AccountQueryNot AccountQuery
  | AccountQueryAnd [AccountQuery]
  | AccountQueryOr [AccountQuery]
  deriving (Eq, Ord, Show)

instance ToJSON AccountQuery where
  toJSON x = case x of
    AccountQueryKeyword x          -> inObjectNamed "keyword" $ toJSON x
    AccountQueryUsername x         -> inObjectNamed "username" $ toJSON x
    AccountQueryFollows x          -> inObjectNamed "follows" $ toJSON x
    AccountQueryMentionedBy x      -> inObjectNamed "mentionedBy" $ toJSON x
    AccountQueryMentions x         -> inObjectNamed "mentions" $ toJSON x
    AccountQueryFollowers o x      -> inObjectNamed "followers" $ (Array . V.fromList) [orderEnc o, (Number . fromIntegral) x]
    AccountQueryNumberPosts o x    -> inObjectNamed "numPosts" $ (Array . V.fromList)  [orderEnc o, (Number . fromIntegral) x]
    AccountQueryTier o x           -> inObjectNamed "tier" $ (Array . V.fromList)      [orderEnc o, (Number . fromIntegral) x]

    AccountQueryOrderBy x          -> inObjectNamed "orderBy" $ searchPostOrderEnc x
    AccountQueryOrderDirection x   -> inObjectNamed "orderDirection" $ sortDirectionEnc x
    AccountQueryLimit x            -> inObjectNamed "limit" $ (Number . fromIntegral) x
    AccountQueryOffset x           -> inObjectNamed "offset" $ (Number . fromIntegral) x

    AccountQueryNot x              -> object [("not", toJSON x)]
    AccountQueryAnd xs             -> object [("and", (Array . V.fromList) (fmap toJSON xs))]
    AccountQueryOr xs              -> object [("or", (Array . V.fromList) (fmap toJSON xs))]
    _                              -> (Array . V.fromList) []

-- inObjectNamed :: Text -> Value -> Value
inObjectNamed n x = object [(n,x)]

orderEnc x = String $ case x of
  LT -> "<"
  EQ -> "="
  GT -> ">"
searchPostOrderEnc x = String $ case x of
  AccountByFollowers  -> "followers"
sortDirectionEnc x = String $ case x of
  Asc   -> "asc"
  Desc  -> "desc"
dateEnc = toJSON . (<> "T00:00:00.000Z") . formatDateFromUTC


-- | Non-recursive version of 'AccountQuery', suitable for use in forms.
data SimpleAccountQuery = SimpleAccountQuery
  { keyword     :: Text
  , username    :: Text
  , follows     :: Text
  , mentionedBy :: Text
  , mentions    :: Text

  , followers   :: Interval Int -- Nothing for inf
  , numPosts    :: Interval Int -- Nothing for inf
  , tier        :: Interval Int -- Nothing for inf

  , orderBy     :: AccountOrder
  , direction   :: SortDirection
  , limit       :: Int }
  deriving (Eq, Show)

data AccountOrder
  = AccountByFollowers
  deriving (Eq, Ord, Show)

data SortDirection = Asc | Desc
  deriving (Eq, Ord, Show)

defSimpleAccountQuery :: SimpleAccountQuery
defSimpleAccountQuery = SimpleAccountQuery
  { keyword     = ""
  , username    = ""
  , follows     = ""
  , mentionedBy = ""
  , mentions    = ""
  , followers   = whole
  , numPosts    = whole
  , tier        = whole
  , orderBy     = AccountByFollowers
  , direction   = Desc
  , limit       = 50 }


complexifyAccountQuery :: SimpleAccountQuery -> AccountQuery
complexifyAccountQuery (SimpleAccountQuery { keyword, username, follows, mentionedBy, mentions, followers, numPosts, tier, orderBy, direction, limit }) =
  AccountQueryAnd $
         (if keyword     == "" then [] else [AccountQueryKeyword keyword])
      ++ (if username    == "" then [] else [AccountQueryUsername username])
      ++ (if follows     == "" then [] else [AccountQueryFollows follows])
      ++ (if mentionedBy == "" then [] else [AccountQueryMentionedBy mentionedBy])
      ++ (if mentions    == "" then [] else [AccountQueryMentions mentions])

      ++ complexifyInterval 0       AccountQueryFollowers followers
      ++ complexifyInterval 0       AccountQueryNumberPosts numPosts
      ++ complexifyInterval 0       AccountQueryTier tier

      ++ [AccountQueryOrderBy orderBy, AccountQueryOrderDirection direction]
      ++ [AccountQueryLimit limit]
  where
    complexifyInterval z f x = fmap (uncurry f) (intervalToOrderings z x)

-- |
-- Convert an interval to a list of restrictions.
--  First argument is an arbitrary value of the type.
intervalToOrderings :: a -> Interval a -> [(Ordering, a)]
intervalToOrderings arbitrary i = case (a, b) of
  (NegInf,   PosInf)   -> [] -- full
  (NegInf,   Finite b) -> [(LT, b)] -- max
  (Finite a, PosInf)   -> [(GT, a)] -- min
  (Finite a, Finite b) -> [(GT, a), (LT, b)] -- min,max
  _                    -> [(GT, arbitrary), (LT, arbitrary)] -- empty
  where
    (a, b) = (lowerBound i, upperBound i)
-- TODO arguaby wrong behavior w.r.t. open/closed
