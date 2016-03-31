{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module BD.Query.PostQuery (
  Query(..),
  PostQuery(..),
  SimplePostQuery(..),
  PostOrder(..),
  SortDirection(..),
  defSimplePostQuery,
  complexifyPostQuery,
) where

import Data.Monoid
import Data.Aeson (ToJSON(..), Value(..), object)
import Data.Time.Calendar (Day(..))
import qualified Data.Vector as V
import Data.Interval (Interval, interval, whole, Extended(..), lowerBound, upperBound)

import qualified Data.JSString

import BD.Types
import Lubeck.Util (formatDateFromUTC, intervalToOrderings)

data Query
  = PostQuery PostQuery
  deriving (Eq, Ord, Show)

instance ToJSON Query where
  toJSON x = case x of
    PostQuery pq -> inObjectNamed "post" $ toJSON pq

data PostQuery
  = PostQueryInCaption Text
  | PostQueryHasComment Text
  | PostQueryHashtag Text
  | PostQueryUsername Text
  | PostQueryUsernames [Text]
  | PostQueryTrackedHashtag Text

  | PostQueryFollowers Ordering Int
  | PostQueryDate Ordering Day
  | PostQueryLocation Int
  | PostQueryHasLocation Bool

  | PostQueryLimit Int
  | PostQueryOffset Int
  | PostQueryOrderBy PostOrder
  | PostQueryOrderDirection SortDirection
  | PostQueryNot PostQuery
  | PostQueryAnd [PostQuery]
  | PostQueryOr [PostQuery]
  deriving (Eq, Ord, Show)

instance ToJSON PostQuery where
  toJSON x = case x of
    PostQueryInCaption x         -> inObjectNamed "inCaption" $ toJSON x
    PostQueryHasComment x        -> inObjectNamed "hasComment" $ toJSON x

    PostQueryFollowers o x       -> inObjectNamed "followers" $ (Array . V.fromList) [orderEnc o, (Number . fromIntegral) x]
    PostQueryDate o x            -> inObjectNamed "date" $ (Array . V.fromList) [orderEnc o, dateEnc x]

    PostQueryUsername x          -> inObjectNamed "username" $ toJSON x
    PostQueryLocation x          -> inObjectNamed "location" $ (Number . fromIntegral) x
    PostQueryHashtag x           -> inObjectNamed "hashtag" $ toJSON x
    PostQueryTrackedHashtag x    -> inObjectNamed "hashtag" $ toJSON x

    PostQueryOrderBy x           -> inObjectNamed "orderBy" $ searchPostOrderEnc x
    PostQueryOrderDirection x    -> inObjectNamed "orderDirection" $ sortDirectionEnc x
    PostQueryLimit x             -> inObjectNamed "limit" $ (Number . fromIntegral) x
    PostQueryOffset x            -> inObjectNamed "offset" $ (Number . fromIntegral) x
    PostQueryNot x               -> object [("not", toJSON x)]
    PostQueryAnd xs              -> object [("and", (Array . V.fromList) (fmap toJSON xs))]
    PostQueryOr xs               -> object [("or", (Array . V.fromList) (fmap toJSON xs))]
    _                     -> (Array . V.fromList) []

-- inObjectNamed :: Text -> Value -> Value
inObjectNamed n x = object [(n,x)]

orderEnc x = String $ case x of
  LT -> "<"
  EQ -> "="
  GT -> ">"
searchPostOrderEnc x = String $ case x of
  PostByCreated    -> "created"
  PostByFollowers  -> "followers"
  PostByLikes      -> "likes"
  PostByComments   -> "comments"
sortDirectionEnc x = String $ case x of
  Asc   -> "asc"
  Desc  -> "desc"
dateEnc = toJSON . (<> "T00:00:00.000Z") . formatDateFromUTC


-- | Non-recursive version of 'PostQuery', suitable for use in forms.
data SimplePostQuery = SimplePostQuery
  { caption        :: Text
  , comment        :: Text
  , hashTag        :: Text
  , userName       :: Text

  , followers      :: Interval Int -- Nothing for inf
  , date           :: Interval Day
  , location       :: Maybe Int

  , orderBy        :: PostOrder
  , direction      :: SortDirection

  , trackedHashTag :: Maybe Text
  , limit          :: Int
  }
  deriving (Eq, Show)

data PostOrder
  = PostByCreated
  | PostByFollowers
  | PostByLikes
  | PostByComments
  deriving (Eq, Ord, Show)

data SortDirection
  = Asc
  | Desc
  deriving (Eq, Ord, Show)

defSimplePostQuery :: SimplePostQuery
defSimplePostQuery = SimplePostQuery
  { caption        = ""
  , comment        = ""
  , hashTag        = ""
  , userName       = ""
  , followers      = whole
  , date           = whole
  , location       = Nothing
  , orderBy        = PostByLikes
  , direction      = Desc
  , trackedHashTag = Nothing
  , limit          = 50 }

-- | Convert a 'SimplePostQuery' to a 'PostQuery'.
complexifyPostQuery :: SimplePostQuery -> PostQuery
complexifyPostQuery (SimplePostQuery {caption, comment, hashTag, userName, followers, date, location, orderBy, direction, trackedHashTag, limit}) =
  case trackedHashTag of
    Just x  ->
      PostQueryAnd $
         [PostQueryTrackedHashtag x]
      ++ [PostQueryLimit limit]
    Nothing ->
      PostQueryAnd $
         (if caption  == "" then [] else [PostQueryInCaption caption])
      ++ (if comment  == "" then [] else [PostQueryHasComment comment])
      ++ (if hashTag  == "" then [] else [PostQueryHashtag hashTag])
      ++ (if userName == "" then [] else [PostQueryUsername userName])
      ++ complexifyInterval 0       PostQueryFollowers followers
      ++ complexifyInterval someDay PostQueryDate date
      ++ [PostQueryOrderBy orderBy, PostQueryOrderDirection direction]
      ++ [PostQueryLimit limit]
  where
    someDay = ModifiedJulianDay 0
    complexifyInterval z f x = fmap (uncurry f) (intervalToOrderings z x)
