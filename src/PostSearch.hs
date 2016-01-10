
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import qualified Data.Maybe
import qualified Data.List
import Data.Monoid
import Control.Applicative
import qualified Data.Map as Map
import Data.Map(Map)

import GHCJS.Types(JSString, jsval)
import qualified Data.JSString
import GHCJS.VDOM.Event (click, change, keyup, submit, stopPropagation, preventDefault, value)
import GHCJS.VDOM.Element (p, h1, div, text, form, button, img, hr, custom, a, table, tbody, th, tr, td, input, label)
import GHCJS.VDOM.Attribute (Attribute, src, width, class_, href, target, width, src)
import qualified GHCJS.VDOM.Element as E
import qualified GHCJS.VDOM.Attribute as A
import qualified GHCJS.VDOM.Event as Ev
import GHCJS.VDOM.Unsafe (unsafeToAttributes, Attributes')
import GHCJS.Foreign.QQ (js, jsu, jsu')

import Lubeck.FRP
import Lubeck.Forms
import Lubeck.App (Html, runAppReactive)
import Lubeck.Web.URI (getURIParameter)

import BD.Data.Account (Account)
import qualified BD.Data.Account as Ac
import BD.Data.SearchPost (SearchPost)
import qualified BD.Data.SearchPost as P
import BD.Query.PostQuery
import qualified BD.Query.PostQuery as PQ
import BD.Api


-- TODO finish
searchForm :: Widget SimplePostQuery (Submit SimplePostQuery)
searchForm output query = div (customAttrs $ Map.fromList [("style","form-vertical")]) $
  [ div () [text (showJS query)]
  -- , input () [text $ PQ.caption query]

  , longStringWidget "Caption"   (contramapSink (\new -> DontSubmit $ query { caption = new })  output) (PQ.caption query)
  , longStringWidget "Comment"   (contramapSink (\new -> DontSubmit $ query { comment = new })  output) (PQ.comment query)
  , longStringWidget "Hashtag"   (contramapSink (\new -> DontSubmit $ query { hashTag = new })  output) (PQ.hashTag query)
  , longStringWidget "User name" (contramapSink (\new -> DontSubmit $ query { userName = new }) output) (PQ.userName query)





  , button (click $ \e -> output $ Submit query) $ text "Search!" ]

{-
searchPost appEvents updateQuery query = [
  form_ "form-vertical" [Attr.style [("width", "80%"), ("margin", "15px")]] $ []
    ++ longStringWidget "Caption"          (Signal.forwardTo updateQuery (\new -> { query | caption   <- new })) query.caption
    ++ longStringWidget "Comment"          (Signal.forwardTo updateQuery (\new -> { query | comment   <- new })) query.comment
    ++ longStringWidget "Hashtag"          (Signal.forwardTo updateQuery (\new -> { query | hashTag   <- new })) query.hashTag
    ++ longStringWidget "User name"        (Signal.forwardTo updateQuery (\new -> { query | userName  <- new })) query.userName
    ++ integerIntervalWidget        "Poster followers" (Signal.forwardTo updateQuery (\new -> { query | followers <- new })) query.followers
    ++ dateIntervalWidget    "Posting date"     (Signal.forwardTo updateQuery (\new -> { query | date      <- new })) query.date

    -- TODO Location

    ++ [
    div [class_ "form-group form-inline"]
    [ div [class_ "form-group"] $ List.concat [

      [ label [] [text "Sort by"] ]

      , selectWidget
        [ (PostByFollowers, "Poster followers")
        , (PostByLikes,     "Likes")
        , (PostByComments,  "Comments")
        , (PostByCreated,   "Posting time")]
        (Signal.forwardTo updateQuery (\new -> { query | orderBy <- new })) query.orderBy

      , selectWidget
        [ (Asc,   "from lowest to highest")
        , (Desc,  "from highest to lowest")]
        (Signal.forwardTo updateQuery (\new -> { query | direction <- new })) query.direction ]]
          ]]



selectWidget : List (a, String) -> Widget' a
selectWidget xs = let
  (vals, names) = List.unzip xs
  (toInt, fromInt) = indexed vals
  count = List.map toString [0..1000]

  selectWidget' : (a -> String) -> (String -> b) -> List (String, String) -> Widget a b
  selectWidget' f g vs = mapInput f $ mapOutput g $ selectWidget'' vs

  selectWidget'' : List (String, String) -> Widget' String
  selectWidget'' valuesLabels s x = [select [class_ "form-control", onChange s, Attr.value x] $ List.map (\(v,l) -> option ([Attr.value v]
    ++ if v == x then [Attr.selected True] else []
    )  [text l]) valuesLabels]

  in selectWidget' (toString << toInt) (fromInt << unsafeToInt) (zip count names)

-}



longStringWidget :: JSString -> Widget' JSString
longStringWidget title update value = div
  [ class_ "form-group" ]
  [ label () [text title]
  , input
    [ A.type_ "search"
    -- TODO size
    , A.class_ "form-control"
    , A.value value
    , change  $ contramapSink Ev.value update
    , keyup $ contramapSink Ev.value update
    ] ()
  ]

type Post = SearchPost

data PostAction
  = CreateAd Post

-- | Non-interactive post table (for search results).
postSearchResult :: Widget [Post] PostAction
postSearchResult output posts = div () [
    h1 () [text "Search Results"]
    , div () [text $ Data.JSString.pack $ "Found " ++ show (length posts) ++ " posts"]
    , postTable output posts
  ]
  where
    postTable :: Widget [Post] PostAction
    postTable output posts =
      table [class_ "table table-striped table-hover"] $ tbody () $
        fmap (tr () . fmap (postTableCell output)) (divide 5 posts)

    postTableCell :: Widget Post PostAction
    postTableCell output post = td ()
      [ a [ target "_blank",
            href $ Data.Maybe.fromMaybe (P.url post) (P.ig_web_url post)
            -- , class_ "hh-brighten-image"
            ]
          [ imgFromWidthAndUrl' 150 (P.thumbnail_url post) [{-fixMissingImage-}] ],
        div () [
          a [href "#"
          ] [text $ "@" <> P.username post]
          ],
        div () [text $ "(l) " <> showWithThousandSeparator (P.like_count post)],
        div () [text $ "(c) " <> showWithThousandSeparator (P.comment_count post)],
        -- For uploading to marketing api
        div () [button (click $ \_ -> output (CreateAd post)) [text "Create Ad"]]
        ]

-- | Modify a widget to accept 'Maybe' and displays the text nothing on 'Nothing'.
maybeW :: Widget a b -> Widget (Maybe a) b
maybeW w s Nothing  = div () [text "(nothing)"]
maybeW w s (Just x) = w s x


-- MAIN
initPostQuery = defSimplePostQuery
-- initPostQuery = defSimplePostQuery {
--     caption   = "christmas",
--     comment   = "",
--     hashTag   = "",
--     userName  = ""
--     -- followers = Nothing ... Nothing,
--     -- date      = Nothing ... Nothing,
--     -- location  = Nothing,
--     -- orderBy   = PostByLikes,
--     -- direction = Asc
--   }

searchPage :: Behavior (Maybe JSString) -> IO (Signal Html)
searchPage mUserNameB = do

  -- Search event (from user)
  (searchView, searchRequested) <- formComponent initPostQuery searchForm

  -- Create ad event (from user)
  (createAd, adCreated) <- newEventOf (undefined :: PostAction)

  -- Search result event (from API)
  (receiveSearchResult, searchResultReceived) <- newEventOf (undefined :: Maybe [Post])
  -- Signal holding the results of the lastest search, or Nothing if no
  -- search has been performed yet
  results <- stepperS Nothing searchResultReceived :: IO (Signal (Maybe [Post]))
  let resultView = fmap ((maybeW postSearchResult) createAd) results  :: Signal Html


  let view = liftA2 (\x y -> div () [x,y]) searchView resultView        :: Signal Html

  -- API calls

  -- Create ad
  subscribeEvent adCreated $ \(CreateAd post) -> do
    -- print (userName, P.ig_web_url post)
    mUserName <- pollBehavior mUserNameB
    case mUserName of
      Nothing -> print "No account to upload post to"
      Just userName -> do
        res <- postAPIEither (userName <> "/upload-igpost-adlibrary/" <> showJS (P.post_id post)) ()
        case res of
          Left _   -> print "Failed to upload post to ad library"
          Right Ok -> print "Uploaded post"

  -- Fetch Posts
  subscribeEvent searchRequested $ \query -> do
    let complexQuery = PostQuery $ complexifyPostQuery query
    eQueryId <- postAPIEither "internal/queries" $ complexQuery
    case eQueryId of
      Left _ -> print "Failed posting query"
      Right queryId -> do
        -- print (queryId :: JSString)
        posts <- unsafeGetAPI $ "internal/queries/" <> queryId <> "/results"
        receiveSearchResult $ Just posts
    return ()

  return view

main :: IO ()
main = do
  mUserName <- getURIParameter "user"
  let mUserNameB = pure mUserName :: Behavior (Maybe JSString)
  searchPage mUserNameB >>= runAppReactive



-- UTILITY

-- @divide n @ separates a list into sublists of length n.
-- The last chunk may be shorter.
divide :: Int -> [a] -> [[a]]
divide n xs = case xs of
  [] -> []
  xs -> take n xs : divide n (drop n xs)

-- @divide n @ separates a list into sublists of length n.
-- The first chunk may be shorter.
divideFromEnd :: Int -> [a] -> [[a]]
divideFromEnd n = reverse . fmap reverse . divide n . reverse

imgFromWidthAndUrl' :: Int -> JSString -> [Attribute] -> Html
imgFromWidthAndUrl' w url attrs = img (attrs ++ [width w, src url]) ()

-- imgFromWidthAndUrlCircle' : Int -> String -> List Html.Attribute -> Html
-- imgFromWidthAndUrlCircle' width url attrs = img (attrs ++ [class_ "img-circle", width width, src url]) []

showWithThousandSeparator :: Int -> JSString
showWithThousandSeparator n = Data.JSString.pack $ concat $ Data.List.intersperse "," $ divideFromEnd 3 $ show n

-- | Like newEvent with a type hint.
newEventOf :: a -> IO (Sink a, Events a)
newEventOf _ = newEvent

showJS :: Show a => a -> JSString
showJS = Data.JSString.pack . show

-- | A limitation in ghcjs-vdom means that non-standard attributes aren't always defined properly.
-- This works around the issue. The value returned here should take the place of the standard
-- attribute definition, i.e. instead of (div () ..) or (div [..] ..), use (div (customAttrs []) ..).
--
-- If possible, use the functions exported by GHCJS.VDOM.Attribute instead.
customAttrs :: Map String String -> Attributes'
customAttrs attrs = let str = (Data.JSString.pack $ ("{"++) $ (++"}") $ drop 2 $ Map.foldWithKey (\k v s -> s++", "++show k++":"++show v) "" attrs) :: JSString
  in unsafeToAttributes [jsu'| {attributes:JSON.parse(`str)} |]
