
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module AdPlatform.Pages.PostSearch
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
import           Data.Time.Calendar             (Day (..))
import           Data.Time.Clock                (UTCTime (..), getCurrentTime)

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
import           Lubeck.Util                    (divideFromEnd, showJS, contentPanel,
                                                 newEventOf,
                                                 showIntegerWithThousandSeparators)
import           Lubeck.Web.URI                 (getURIParameter)

import           BD.Api
import           BD.Data.Account                (Account)
import qualified BD.Data.Account                as Ac
import           BD.Data.SearchPost             (SearchPost)
import qualified BD.Data.SearchPost             as P
import           BD.Query.PostQuery
import qualified BD.Query.PostQuery             as PQ
import           BD.Types


import           AdPlatform.Types
import           Components.BusyIndicator       (BusyCmd (..), withBusy, withBusy2,
                                                 busyIndicatorComponent)


-- TODO finish
searchForm :: Day -> Widget SimplePostQuery (Submit SimplePostQuery)
searchForm dayNow output query =
  contentPanel $
    div [class_ "form-group form-group-sm"]
      [ -- div [] [text (showJS query)]
        -- , rmapWidget DontSubmit $ subWidget (lens PQ.caption (\s b -> s {caption=b})) (longStringWidget "Caption") output query
        longStringWidget "Caption"   (contramapSink (\new -> DontSubmit $ query { caption = new })  output) (PQ.caption query)
      , longStringWidget "Comment"   (contramapSink (\new -> DontSubmit $ query { comment = new })  output) (PQ.comment query)
      , longStringWidget "Hashtag"   (contramapSink (\new -> DontSubmit $ query { hashTag = new })  output) (PQ.hashTag query)
      , longStringWidget "User name" (contramapSink (\new -> DontSubmit $ query { userName = new }) output) (PQ.userName query)

      , integerIntervalWidget "Poster followers"    (contramapSink (\new -> DontSubmit $ query { followers = new }) output) (PQ.followers query)
      , dateIntervalWidget    dayNow "Posting date" (contramapSink (\new -> DontSubmit $ query { date = new }) output)      (PQ.date query)

      , div [ class_ "form-group form-inline" ]
        [ div [ class_ "form-group"  ]
          [ label [A.style "margin-right: 10px;"] [text "Sort by" ]
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
      , button [A.class_ "btn btn-default btn-block", click $ \e -> output $ Submit query]
          [ E.i [class_ "fa fa-instagram", A.style "margin-right: 5px"] []
          , text "Search!"]
      ]

type Post = SearchPost

data PostAction
  = UploadImage Post

-- | Non-interactive post table (for search results).
postSearchResult :: Widget [Post] PostAction
postSearchResult output posts =
  contentPanel $
    div []
      [ h1 [] [text "Search Results"]
      , div [] [text $ Data.JSString.pack $ "Found " ++ show (length posts) ++ " posts"]
      , postTable output posts
      ]



  where
    postTable :: Widget [Post] PostAction
    postTable output posts =
      div [] (map (postTableCell output) posts)

    postTableCell :: Widget Post PostAction
    postTableCell output post =
      div [ class_ "thumbnail custom-thumbnail-1 fit-text" ]
        [ a [ target "_blank"
            , href $ Data.Maybe.fromMaybe (P.url post) (P.ig_web_url post) ]
            [ imgFromWidthAndUrl (P.thumbnail_url post) [{-fixMissingImage-}] ]

        , p [] [ a [ target "_blank"
                   , href $ "https://www.instagram.com/" <> P.username post]
                   [text $ "@" <> P.username post] ]

        , p [class_ "text-center"]
               [ E.div [ class_ "fa fa-heart badge badge-info"
                       , A.style "margin: 0 3px;"
                       , A.title "Likes count" ]
                       [ E.span [class_ "xbadge"
                                , A.style "margin-left: 5px;"]
                                [text $ showIntegerWithThousandSeparators (P.like_count post)] ]

               , E.div [ class_ "fa fa-comments-o badge badge-info"
                       , A.title "Comments count"
                       , A.style "margin: 0 3px;" ]
                       [ E.span [class_ "xbadge"
                                , A.style "margin-left: 5px;"]
                                [text $ showIntegerWithThousandSeparators (P.comment_count post)] ]
               ]

        , p [] [ button [ A.class_ "btn btn-link btn-sm btn-block"
                        , click $ \_ -> output (UploadImage post) ]
                        [ E.i [class_ "fa fa-cloud-upload", A.style "margin-right: 5px;"] []
                        , text "Upload" ] ]
        ]

    imgFromWidthAndUrl url attrs = img ([class_ "img-thumbnail", src url] ++ attrs) []


searchPage :: Sink BusyCmd -> Sink (Maybe AppError) -> Sink IPCMessage -> Behavior (Maybe JSString) -> IO (Signal Html)
searchPage busySink errorSink ipcSink mUserNameB = do
  let initPostQuery = defSimplePostQuery


  now <- getCurrentTime

  -- Search event (from user)
  (searchView, searchRequested) <- formComponent initPostQuery (searchForm $ utctDay now)

  -- Create ad event (from user)
  (uploadImage, uploadedImage) <- newEventOf (undefined :: PostAction)

  -- Search result event (from API)
  (receiveSearchResult, searchResultReceived) <- newEventOf (undefined :: Maybe [Post])

  -- Signal holding the results of the lastest search, or Nothing if no
  -- search has been performed yet
  results <- stepperS Nothing searchResultReceived                              :: IO (Signal (Maybe [Post]))
  let resultView = fmap ((altW (text "") postSearchResult) uploadImage) results :: Signal Html

  let view = liftA2 (\x y -> div [] [x,y]) searchView resultView                :: Signal Html

  -- API calls

  -- Create ad
  subscribeEvent uploadedImage $ \(UploadImage post) -> do
    -- print (userName, P.ig_web_url post)
    mUserName <- pollBehavior mUserNameB
    case mUserName of
      Nothing -> errorSink . Just . BLError $ "No account to upload post to"
      Just userName -> do
        res <- (withBusy2 busySink postAPIEither) (userName <> "/upload-igpost-adlibrary/" <> showJS (P.post_id post)) ()
        case res of
          Left e   -> errorSink . Just . ApiError $ "Failed to upload post to ad library" <> showJS e
          Right Ok -> ipcSink ImageLibraryUpdated

  -- Fetch Posts
  subscribeEvent searchRequested $ \query -> do
    receiveSearchResult Nothing -- reset previous search results

    let complexQuery = PostQuery $ complexifyPostQuery query
    eQueryId <- (withBusy2 busySink postAPIEither) "internal/queries" $ complexQuery
    case eQueryId of
      Left e        -> errorSink . Just . ApiError $ "Failed posting query: " <> showJS e
      Right queryId -> do
        eitherPosts <- (withBusy busySink getAPIEither) $ "internal/queries/" <> queryId <> "/results"
        case eitherPosts of
          Left e   -> errorSink . Just . ApiError $ "Failed getting query results: " <> showJS e
          Right ps -> receiveSearchResult $ Just ps
    return ()

  return view
