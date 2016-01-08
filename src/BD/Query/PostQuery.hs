
{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module BD.Query.PostQuery (
  PostQuery(..),
  SimplePostQuery,
  defSimplePostQuery,
  complexifyPostQuery
) where

import Data.Time.Calendar (Day(..))
import Numeric.Interval (Interval, whole, (...))
import qualified Numeric.Interval as I
import Data.Aeson (ToJSON(..), Value(..), object)
import qualified Data.Vector as V
import qualified Data.Time.Format
import qualified Data.JSString
import BD.Types

data PostQuery
  = PostQueryInCaption Text
  | PostQueryHasComment Text
  | PostQueryHashtag Text
  | PostQueryUsername Text
  | PostQueryUsernames [Text]

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
dateEnc = toJSON . formatDateUTC

formatDateUTC :: Day -> String
formatDateUTC = Data.Time.Format.formatTime l f
  where
    l = Data.Time.Format.defaultTimeLocale
    f = Data.Time.Format.iso8601DateFormat Nothing


data SimplePostQuery = SimplePostQuery {
    caption      :: Text,
    comment      :: Text,
    hashTag      :: Text,
    userName     :: Text,

    followers    :: Interval (Maybe Int), -- Nothing for inf
    date         :: Interval (Maybe Day),
    location     :: Maybe Int,

    orderBy     :: PostOrder,
    direction   :: SortDirection
  }
  deriving (Eq, Ord, Show)

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
defSimplePostQuery = SimplePostQuery {
    caption   = "",
    comment   = "",
    hashTag   = "",
    userName  = "",
    followers = Nothing ... Nothing,
    date      = Nothing ... Nothing,
    location  = Nothing,
    orderBy   = PostByLikes,
    direction = Asc
  }

complexifyPostQuery :: SimplePostQuery -> PostQuery
complexifyPostQuery (SimplePostQuery {caption, comment, hashTag, userName, followers, date, location, orderBy, direction}) =
    PostQueryAnd $
       (if caption  == "" then [] else [PostQueryInCaption caption])
    ++ (if comment  == "" then [] else [PostQueryHasComment comment])
    ++ (if hashTag  == "" then [] else [PostQueryHashtag hashTag])
    ++ (if userName == "" then [] else [PostQueryUsername userName])
    ++ complexifyInterval 0       PostQueryFollowers followers
    ++ complexifyInterval someDay PostQueryDate date
    ++ [PostQueryOrderBy orderBy, PostQueryOrderDirection direction]
  where
    someDay = ModifiedJulianDay 0
    complexifyInterval z f x = fmap (uncurry f) (intervalToOrderings z x)

-- | Convert an interval to a list of restrictions.
-- First argument is an arbitrary value of the type.
intervalToOrderings :: a -> Interval (Maybe a) -> [(Ordering, a)]
intervalToOrderings arbitrary i
  | I.null i  = [(GT,arbitrary),(LT,arbitrary)] -- Not quite, should be orderings that denote to the empty set such as for any x [(GT,x),(LT,x)]
  | otherwise = case (I.inf i, I.sup i) of
    (Nothing, Nothing) -> []
    (Nothing, Just b)  -> [(LT, b)]
    (Just a,  Nothing) -> [(GT, a)]
    (Just a,  Just b)  -> [(GT, a), (LT, b)]
