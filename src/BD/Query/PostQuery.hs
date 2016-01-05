
module BD.Data.Query.PostQuery (
  PostQuery(..),
  SimplePostQuery
) where

data PostQuery
  = PostQueryInCaption String
  | PostQueryHasComment String
  | PostQueryHashtag String
  | PostQueryUsername String
  | PostQueryUsernames (List String)

  | PostQueryFollowers Order Int
  | PostQueryDate Order Date
  | PostQueryLocation Int
  | PostQueryHasLocation Bool

  | PostQueryLimit Int
  | PostQueryOffset Int
  | PostQueryOrderBy PostOrder
  | PostQueryOrderDirection SortDirection
  | PostQueryNot PostQuery
  | PostQueryAnd (List PostQuery)
  | PostQueryOr (List PostQuery)

data SimplePostQuery = {
    caption      :: String,
    comment      :: String,
    hashTag      :: String,
    userName     :: String,

    followers    :: Interval Int,
    date         :: Interval Date,
    location     :: Maybe Int,

    orderBy     :: PostOrder,
    direction   :: SortDirection
  }
