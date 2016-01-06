
{-# LANGUAGE NamedFieldPuns #-}

module BD.Query.PostQuery (
  PostQuery(..),
  SimplePostQuery,
  defSimplePostQuery,
  complexifyPostQuery
) where

import Data.Time.Calendar (Day(..))
import Numeric.Interval (Interval, whole, (...))
import qualified Numeric.Interval as I

data PostQuery
  = PostQueryInCaption String
  | PostQueryHasComment String
  | PostQueryHashtag String
  | PostQueryUsername String
  | PostQueryUsernames [String]

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

data SimplePostQuery = SimplePostQuery {
    caption      :: String,
    comment      :: String,
    hashTag      :: String,
    userName     :: String,

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
