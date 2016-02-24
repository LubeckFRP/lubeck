
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
import           Control.Monad                  (void)
import qualified Data.List
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import qualified Data.Maybe
import           Data.Monoid
import           Data.String                    (fromString)
import qualified Data.Text                      as T
import           Data.Time.Calendar             (Day (..))
import           Data.Time.Clock                (UTCTime (..), getCurrentTime)

import           Control.Concurrent             (forkIO, threadDelay)
import qualified Data.JSString
import           GHCJS.Concurrent               (synchronously)
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
import           Lubeck.Types
import           Lubeck.Forms.Interval
import           Lubeck.Forms.Select
import           Lubeck.FRP
import           Lubeck.Util                    (contentPanel, divideFromEnd,
                                                 newEventOf, showIntegerWithThousandSeparators,
                                                 showJS, which)
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
import           Components.Map
import           Components.BusyIndicator       (BusyCmd (..),
                                                 busyIndicatorComponent,
                                                 withBusy, withBusy2)


-- TODO finish
searchForm :: Day -> Widget SimplePostQuery (Submit SimplePostQuery)
searchForm dayNow output query =
  contentPanel $
    div [ class_ "form-horizontal"
        , keyup $ \e -> if which e == 13 then output (Submit query) else return ()
        ]  -- event delegation
      [ longStringWidget "Caption"   True  (contramapSink (\new -> DontSubmit $ query { caption = new })  output) (PQ.caption query)
      , longStringWidget "Comment"   False (contramapSink (\new -> DontSubmit $ query { comment = new })  output) (PQ.comment query)
      , longStringWidget "Hashtag"   False (contramapSink (\new -> DontSubmit $ query { hashTag = new })  output) (PQ.hashTag query)
      , longStringWidget "User name" False (contramapSink (\new -> DontSubmit $ query { userName = new }) output) (PQ.userName query)

      , integerIntervalWidget "Poster followers"    (contramapSink (\new -> DontSubmit $ query { followers = new }) output) (PQ.followers query)
      , dateIntervalWidget    dayNow "Posting date" (contramapSink (\new -> DontSubmit $ query { date = new }) output)      (PQ.date query)

      , div [ class_ "form-group"  ]
        [ label [class_ "control-label col-xs-2"] [text "Sort by" ]
        , div [class_ "col-xs-10 form-inline"]
            [ selectWidget
                [ (PostByFollowers, "Poster followers")
                , (PostByLikes,     "Likes")
                , (PostByComments,  "Comments")
                , (PostByCreated,   "Posting time") ]
                (contramapSink (\new -> DontSubmit $ query { orderBy = new }) output) (PQ.orderBy query)
            , selectWidget
                [ (Desc,  "from highest to lowest")
                , (Asc,   "from lowest to highest") ]
                (contramapSink (\new -> DontSubmit $ query { direction = new }) output) (PQ.direction query)
            ]
        ]

      , div [class_ "form-group"]
          [ div [class_ "col-xs-offset-2 col-xs-10"]
              [ button [A.class_ "btn btn-success", click $ \e -> output $ Submit query]
                  [ E.i [class_ "fa fa-instagram", A.style "margin-right: 5px"] []
                  , text "Search!"
                  ] ]
          ]
      ]

type Post = SearchPost

data PostAction
  = UploadImage Post

postSearchResultW :: Widget [Post] PostAction
postSearchResultW output posts = postTable output posts
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

data ResultsViewMode = ResultsGrid | ResultsMap deriving (Show, Eq)

resultsLayout :: Sink ResultsViewMode -> Html -> Html -> ResultsViewMode -> Maybe [Post] -> Html
resultsLayout sink gridH mapH mode posts = case mode of
  ResultsGrid -> wrapper sink gridH True  False posts
  ResultsMap  -> wrapper sink mapH  False True  posts
  where
    wrapper sink x asel bsel posts =
      contentPanel $
        div []
          [ div [class_ "page-header"]
              [ h1 [] [ text "Search Results "
                      , E.small [] [text $ Data.JSString.pack $ "Found " ++ show (Data.Maybe.fromMaybe 0 (length <$> posts)) ++ " posts"]
                      ] ]
          , div [A.style "text-align: center;"]
              [ div [class_ "btn-group", A.style "margin-bottom: 15px;"]
                  [ button [ class_ ("btn " <> if asel then "btn-primary" else "btn-default")
                           , click $ \e -> sink ResultsGrid]
                              [ E.i [class_ "fa fa-th", A.style "margin-right: 5px;"] []
                              , text "Grid"]
                  , button [ class_ ("btn " <> if bsel then "btn-primary" else "btn-default")
                           , click $ \e -> sink ResultsMap]
                              [ E.i [class_ "fa fa-map-o", A.style "margin-right: 5px;"] []
                              , text "Map"] ]
              , x ]
          ]



mapLifecycle :: (Nav, ResultsViewMode) -> Maybe MapLifecycle
mapLifecycle (NavSearch, ResultsMap)  = Just MapInit
mapLifecycle (_, _)                   = Just MapDestroy

postToMarker :: Post -> Maybe Marker
postToMarker p = Marker <$> (Point <$> (P.latitude p) <*> (P.longitude p)) <*> (Just . Just $ markerInfo p)

markerInfo :: Post -> JSString
markerInfo post =
    -- TODO virtual-dom string renderer
     "<div class='thumbnail custom-thumbnail-1 fit-text' style='padding: 0px;'>"
  <>   "<a target='_blank' href='" <> Data.Maybe.fromMaybe (P.url post) (P.ig_web_url post)  <> "'>"
  <>     "<img class='img-thumbnail' src='" <> P.thumbnail_url post <> "'>"
  <>   "</a>"
  <>   "<p style='margin: 0px;'>"
  <>     "<a target='_blank' href='https://www.instagram.com/" <> P.username post <> "'>@" <> P.username post <> "</a>"
  <>   "</p>"
  <>   "<p class='text-center'>"
  <>     "<div class='fa fa-heart badge badge-info' style='margin: 0 3px;' title='Likes count'>"
  <>       "<span class='xbadge' style='margin-left: 5px;'>" <> showIntegerWithThousandSeparators (P.like_count post) <> "</span>"
  <>     "</div>"
  <>     "<div class='fa fa-comments-o badge badge-info' title='Comments count' style='margin: 0 3px;'>"
  <>       "<span class='xbadge' style='margin-left: 5px;'>" <> showIntegerWithThousandSeparators (P.comment_count post) <> "</span>"
  <>     "</div>"
  <>   "</p>"
  <>   "<p>"
  <>     "<button class='btn btn-link btn-sm btn-block'> <i class='fa fa-cloud-upload' style='margin-right: 5px;'></i>Upload</button>"
  <>   "</p>"
  <> "</div>"


showResultsOnMap mapSink mbPosts =
  mapSink $ ShowMarker $ Data.Maybe.fromMaybe [] $ fmap (Data.Maybe.catMaybes . fmap postToMarker) mbPosts

searchPage :: Sink BusyCmd
           -> Sink (Maybe Notification)
           -> Sink IPCMessage
           -> Behavior (Maybe JSString)
           -> Signal Nav
           -> IO (Signal Html)
searchPage busySink notifSink ipcSink mUserNameB navS = do
  let initPostQuery                = defSimplePostQuery

  (viewModeSink, viewModeEvents)   <- newEventOf (undefined                                        :: ResultsViewMode)
  resultsViewModeS                 <- stepperS ResultsGrid viewModeEvents

  now                              <- getCurrentTime

  (searchView, searchRequested)    <- formComponent initPostQuery (searchForm (utctDay now))
  (uploadImage, uploadedImage)     <- newEventOf (undefined                                        :: PostAction)
  (srchResSink, srchResEvents)     <- newEventOf (undefined                                        :: Maybe [Post])

  (mapView, mapSink, _)            <- mapComponent []

  results                          <- stepperS Nothing srchResEvents                               :: IO (Signal (Maybe [Post]))
  let gridView                     = fmap ((altW (text "") postSearchResultW) uploadImage) results :: Signal Html
  let resultsViewS                 = (resultsLayout viewModeSink) <$> gridView <*> mapView <*> resultsViewModeS <*> results :: Signal Html
  let view                         = liftA2 (\x y -> div [] [x,y]) searchView resultsViewS         :: Signal Html

  -- This will try to destroy the map on any navigation
  -- What we need is to destroy the map just the first time a user navigates out of the search page
  -- TODO history-aware signal needed for this
  let fullNavS                     = liftA2 (,) navS resultsViewModeS                              :: Signal (Nav, ResultsViewMode)
  let resetMapS                    = fmap mapLifecycle fullNavS                                    :: Signal (Maybe MapLifecycle)

  subscribeEvent (updates results) (showResultsOnMap mapSink)

  subscribeEvent (updates resetMapS) $ \x -> void $ forkIO $ case x of
    Nothing -> return ()
    Just x  -> do
      threadDelay 50 -- give DOM a litle time
      synchronously . mapSink $ x

      threadDelay 1000 -- give map a little time

      curRes <- pollBehavior (current results)
      (synchronously . showResultsOnMap mapSink)  curRes

  -- Create ad
  subscribeEvent uploadedImage $ \(UploadImage post) -> void $ forkIO $ do
    mUserName <- pollBehavior mUserNameB
    case mUserName of
      Nothing -> synchronously . notifSink . Just . blError $ "No account to upload post to"
      Just userName -> do
        res <- (withBusy2 (synchronously . busySink) (postAPIEither BD.Api.defaultAPI)) (userName <> "/upload-igpost-adlibrary/" <> showJS (P.post_id post)) ()
        case res of
          Left e        -> synchronously  . notifSink . Just . apiError $ "Failed to upload post to ad library : " <> showJS e
          Right (Ok s)  -> (synchronously . notifSink . Just . NSuccess $ "Image uploaded successfully! :-)") >> (synchronously . ipcSink $ ImageLibraryUpdated)
          Right (Nok s) -> synchronously  . notifSink . Just . apiError $ "Failed to upload post to ad library : " <> s

  -- Fetch Posts
  subscribeEvent searchRequested $ \query -> void $ forkIO $ do
    srchResSink Nothing -- reset previous search results

    -- mapSink $ ShowMarker [(Marker (Point 0 (-0.09))  (Just "<b>Hello</b> <i>world</i> 2"))]

    let complexQuery = PostQuery $ complexifyPostQuery query
    eQueryId <- (withBusy2 (synchronously . busySink) (postAPIEither BD.Api.defaultAPI)) "internal/queries" $ complexQuery
    case eQueryId of
      Left e        -> synchronously . notifSink . Just . apiError $ "Failed posting query: " <> showJS e
      Right queryId -> void $ forkIO $ do
        eitherPosts <- (withBusy (synchronously . busySink) (getAPIEither BD.Api.defaultAPI)) $ "internal/queries/" <> queryId <> "/results"
        case eitherPosts of
          Left e   -> synchronously . notifSink . Just . apiError $ "Failed getting query results: " <> showJS e
          Right ps -> synchronously . srchResSink $ Just ps
    return ()

  return view
