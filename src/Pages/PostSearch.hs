
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module Pages.PostSearch
  ( searchPage
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import           Control.Lens                   (lens, over, set, view)
import qualified Data.List
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import qualified Data.Maybe
import           Data.Monoid

import qualified Data.JSString
import           GHCJS.Types                    (JSString)
import           Web.VirtualDom.Html            (Property, a, button, div, form,
                                                 h1, hr, img, input, label, p,
                                                 table, tbody, td, text, th, tr)
import qualified Web.VirtualDom.Html            as E
import           Web.VirtualDom.Html.Attributes (class_, href, src, src, target,
                                                 width, width)
import qualified Web.VirtualDom.Html.Attributes as A
import           Web.VirtualDom.Html.Events     (change, click, keyup,
                                                 preventDefault,
                                                 stopPropagation, submit, value)
import qualified Web.VirtualDom.Html.Events     as Ev

import           Lubeck.App                     (Html, runAppReactive)
import           Lubeck.Forms
import           Lubeck.Forms.Interval
import           Lubeck.Forms.Select
import           Lubeck.FRP
import           Lubeck.Util                    ()
import           Lubeck.Web.URI                 (getURIParameter)

import           BD.Api
import           BD.Data.Account                (Account)
import qualified BD.Data.Account                as Ac
import           BD.Data.SearchPost             (SearchPost)
import qualified BD.Data.SearchPost             as P
import           BD.Query.PostQuery
import qualified BD.Query.PostQuery             as PQ
import           BD.Utils
import           Components.BusyIndicator       (withBusy2)

import           Components.BusyIndicator       (BusyCmd (..),
                                                 busyIndicatorComponent)

import           Lib.Helpers


-- TODO finish
searchForm :: Widget SimplePostQuery (Submit SimplePostQuery)
searchForm output query =
  div [class_ "row"]
    [ div [class_ "col-md-4 col-lg-3"]
      [ div [class_ "form-group form-group-sm"]
        [ -- div [] [text (showJS query)]
          -- , rmapWidget DontSubmit $ subWidget (lens PQ.caption (\s b -> s {caption=b})) (longStringWidget "Caption") output query
          longStringWidget "Caption:"  (contramapSink (\new -> DontSubmit $ query { caption = new })  output) (PQ.caption query)
        , longStringWidget "Comment"   (contramapSink (\new -> DontSubmit $ query { comment = new })  output) (PQ.comment query)
        , longStringWidget "Hashtag"   (contramapSink (\new -> DontSubmit $ query { hashTag = new })  output) (PQ.hashTag query)
        , longStringWidget "User name" (contramapSink (\new -> DontSubmit $ query { userName = new }) output) (PQ.userName query)

        , integerIntervalWidget "Poster followers" (contramapSink (\new -> DontSubmit $ query { followers = new }) output) (PQ.followers query)
        , dateIntervalWidget    "Posting date"     (contramapSink (\new -> DontSubmit $ query { date = new }) output) (PQ.date query)

        , div [ class_ "form-group form-inline" ]
          [ div [ class_ "form-group"  ]
            [ label [] [text "Sort by" ]
            , selectWidget
              [ (PostByFollowers, "Poster followers")
              , (PostByLikes,     "Likes")
              , (PostByComments,  "Comments")
              , (PostByCreated,   "Posting time")
              ]
              (contramapSink (\new -> DontSubmit $ query { orderBy = new }) output) (PQ.orderBy query)
            , selectWidget
              [ (Asc,   "from lowest to highest")
              , (Desc,  "from highest to lowest")
              ]
              (contramapSink (\new -> DontSubmit $ query { direction = new }) output) (PQ.direction query)
            ]
          ]
        , button [A.class_ "btn btn-default btn-block", click $ \e -> output $ Submit query] [text "Search!"]
        ]
      ]
    ]

type Post = SearchPost

data PostAction
  = UploadImage Post

-- | Non-interactive post table (for search results).
postSearchResult :: Widget [Post] PostAction
postSearchResult output posts =
  div [class_ "row"]
    [ div [class_ "col-md-4 col-lg-3"]
      [ div []
        [ h1 [] [text "Search Results"]
        , div [] [text $ Data.JSString.pack $ "Found " ++ show (length posts) ++ " posts"]
        , postTable output posts
        ]
      ]
    ]

  where
    postTable :: Widget [Post] PostAction
    postTable output posts =
      table [class_ "table table-striped table-hover"] $
        pure $ tbody [] $
          fmap (tr [] . fmap (postTableCell output)) (divide 5 posts)

    postTableCell :: Widget Post PostAction
    postTableCell output post = td []
      [ a [ target "_blank",
            href $ Data.Maybe.fromMaybe (P.url post) (P.ig_web_url post)
            -- , class_ "hh-brighten-image"
            ]
          [ imgFromWidthAndUrl' 150 (P.thumbnail_url post) [{-fixMissingImage-}] ],
        div [] [
          a [href "#"
          ] [text $ "@" <> P.username post]
          ],
        div [] [text $ "(l) " <> showWithThousandSeparator (P.like_count post)],
        div [] [text $ "(c) " <> showWithThousandSeparator (P.comment_count post)],
        -- For uploading to marketing api
        div [] [button [A.class_ "btn btn-default btn-block", click $ \_ -> output (UploadImage post)] [text "Upload Image"]]
        ]

searchPage :: Sink BusyCmd -> Behavior (Maybe JSString) -> IO (Signal Html)
searchPage busySink mUserNameB = do
  let initPostQuery = defSimplePostQuery

  -- Search event (from user)
  (searchView, searchRequested) <- formComponent initPostQuery searchForm

  -- Create ad event (from user)
  (uploadImage, uploadedImage) <- newEventOf (undefined :: PostAction)

  -- Search result event (from API)
  (receiveSearchResult, searchResultReceived) <- newEventOf (undefined :: Maybe [Post])

  -- Signal holding the results of the lastest search, or Nothing if no
  -- search has been performed yet
  results <- stepperS Nothing searchResultReceived                   :: IO (Signal (Maybe [Post]))
  let resultView = fmap ((altW (text "") postSearchResult) uploadImage) results :: Signal Html

  let view = liftA2 (\x y -> div [] [x,y]) searchView resultView     :: Signal Html

  -- API calls

  -- Create ad
  subscribeEvent uploadedImage $ \(UploadImage post) -> do
    -- print (userName, P.ig_web_url post)
    mUserName <- pollBehavior mUserNameB
    case mUserName of
      Nothing -> print "No account to upload post to"
      Just userName -> do
        res <- (withBusy2 busySink postAPIEither) (userName <> "/upload-igpost-adlibrary/" <> showJS (P.post_id post)) ()
        case res of
          Left _   -> print "Failed to upload post to ad library"
          Right Ok -> print "Uploaded post"

  -- Fetch Posts
  subscribeEvent searchRequested $ \query -> do
    let complexQuery = PostQuery $ complexifyPostQuery query
    eQueryId <- (withBusy2 busySink postAPIEither) "internal/queries" $ complexQuery
    case eQueryId of
      Left _ -> print "Failed posting query"
      Right queryId -> do
        -- print (queryId :: JSString)
        posts <- unsafeGetAPI $ "internal/queries/" <> queryId <> "/results"
        receiveSearchResult $ Just posts
    return ()

  return view


-- MAIN

-- main :: IO ()
-- main = do
--   mUserName <- getURIParameter "user"
--   let mUserNameB = pure mUserName :: Behavior (Maybe JSString)
--   searchPage mUserNameB >>= runAppReactive



-- UTILITY

imgFromWidthAndUrl' :: Int -> JSString -> [Property] -> Html
imgFromWidthAndUrl' w url attrs = img (attrs ++ [width w, src url]) []

-- imgFromWidthAndUrlCircle' : Int -> String -> List Html.Attribute -> Html
-- imgFromWidthAndUrlCircle' width url attrs = img (attrs ++ [class_ "img-circle", width width, src url]) []

showWithThousandSeparator :: Int -> JSString
showWithThousandSeparator n = Data.JSString.pack $ concat $ Data.List.intersperse "," $ divideFromEnd 3 $ show n

-- | Like newEvent with a type hint.
newEventOf :: a -> IO (Sink a, Events a)
newEventOf _ = newEvent
