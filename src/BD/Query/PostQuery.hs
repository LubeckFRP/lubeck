
module BD.Query.PostQuery (
  PostQuery(..),
  SimplePostQuery
) where

import Data.Time.Calendar (Day)
import Numeric.Interval (Interval)

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

    followers    :: Interval Int,
    date         :: Interval Day,
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
