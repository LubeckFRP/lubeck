
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module BDPlatform.Pages.PostSearch
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

import           Web.VirtualDom                 (renderToString, createElement, DOMNode)
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
                                                 showJS, which, withErrorIO)
import           Lubeck.Web.URI                 (getURIParameter)

import           BD.Api
import           BD.Data.Account                (Account)
import qualified BD.Data.Account                as Ac
import           BD.Data.SearchPost             (SearchPost)
import qualified BD.Data.SearchPost             as P
import           BD.Query.PostQuery
import qualified BD.Query.PostQuery             as PQ
import           BD.Types


import           BDPlatform.Types
import           Components.Map
import           Components.BusyIndicator       (BusyCmd (..),
                                                 busyIndicatorComponent,
                                                 withBusy, withBusy2)


-- TODO finish
searchForm :: Day -> Widget ([P.TrackedHashtag], SimplePostQuery) (Submit SimplePostQuery)
searchForm dayNow outputSink (trackedHTs, query) =
  contentPanel $
    div [ class_ "form-horizontal"
        , keyup $ \e -> if which e == 13 then outputSink (Submit query) else return ()
        ]  -- event delegation
      [ longStringWidget "Caption"   True  (contramapSink (\new -> DontSubmit $ query { caption = new })  outputSink) (PQ.caption query)
      , longStringWidget "Comment"   False (contramapSink (\new -> DontSubmit $ query { comment = new })  outputSink) (PQ.comment query)
      , longStringWidget "Hashtag"   False (contramapSink (\new -> DontSubmit $ query { hashTag = new })  outputSink) (PQ.hashTag query)
      , longStringWidget "User name" False (contramapSink (\new -> DontSubmit $ query { userName = new }) outputSink) (PQ.userName query)

      , integerIntervalWidget "Poster followers"    (contramapSink (\new -> DontSubmit $ query { followers = new }) outputSink) (PQ.followers query)
      , dateIntervalWidget    dayNow "Posting date" (contramapSink (\new -> DontSubmit $ query { date = new }) outputSink)      (PQ.date query)

      , div [ class_ "form-group"  ]
        [ label [class_ "control-label col-xs-2"] [text "Sort by" ]
        , div [class_ "col-xs-10 form-inline"]
            [ selectWidget
                [ (PostByFollowers, "Poster followers")
                , (PostByLikes,     "Likes")
                , (PostByComments,  "Comments")
                , (PostByCreated,   "Posting time") ]
                (contramapSink (\new -> DontSubmit $ query { orderBy = new }) outputSink) (PQ.orderBy query)
            , selectWidget
                [ (Desc,  "from highest to lowest")
                , (Asc,   "from lowest to highest") ]
                (contramapSink (\new -> DontSubmit $ query { direction = new }) outputSink) (PQ.direction query)
            ]
        ]

      , div [ class_ "form-group"  ]
        [ label [class_ "control-label col-xs-2"] [text "Tracked hashtags" ]
        , div [class_ "col-xs-10 form-inline"]
            [ selectWithPromptWidget
                (zip (P.tag <$> trackedHTs) (P.tag <$> trackedHTs)) -- FIXME
                (contramapSink (\new -> DontSubmit $ query { trackedHashTag = new }) outputSink) (Data.Maybe.fromMaybe "" $ PQ.trackedHashTag query) ]
        ]

      , div [ class_ "form-group"  ]
        [ label [class_ "control-label col-xs-2"] [text "Max number of posts returned" ]
        , div [class_ "col-xs-10 form-inline"]
            [ selectWidget
                [ (50,  "50")
                , (100, "100")
                , (200, "200")
                , (400, "400") ]
                (contramapSink (\new -> DontSubmit $ query { limit = new }) outputSink) (PQ.limit query)
            ]
        ]

      , div [class_ "form-group"]
          [ div [class_ "col-xs-offset-2 col-xs-10"]
              [ button [A.class_ "btn btn-success", click $ \e -> outputSink $ Submit query]
                  [ E.i [class_ "fa fa-instagram", A.style "margin-right: 5px"] []
                  , text "Search!"
                  ] ]
          ]
      ]

type Post = SearchPost

data PostAction
  = UploadImage Post

itemMarkup :: Widget Post PostAction
itemMarkup output post =
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
  where
    imgFromWidthAndUrl url attrs = img ([class_ "img-thumbnail", src url] ++ attrs) []

postSearchResultW :: Widget [Post] PostAction
postSearchResultW output posts = postTable output posts
  where
    postTable :: Widget [Post] PostAction
    postTable output posts =
      div [] (map (itemMarkup output) posts)

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
              [ div [class_ "btn-group", A.style "margin-bottom: 20px;"]
                  [ button [ class_ ("btn " <> if asel then "btn-primary" else "btn-default")
                           , click $ \e -> if asel then return () else sink ResultsGrid]
                              [ E.i [class_ "fa fa-th", A.style "margin-right: 5px;"] []
                              , text "Grid"]
                  , button [ class_ ("btn " <> if bsel then "btn-primary" else "btn-default")
                           , click $ \e -> if bsel then return () else sink ResultsMap]
                              [ E.i [class_ "fa fa-map-o", A.style "margin-right: 5px;"] []
                              , text "Map"] ]
              , x ]
          ]


renderToString' :: Html -> IO JSString
renderToString' n = renderToString (div [] [n])

renderToDOMNode :: Html -> IO DOMNode
renderToDOMNode n = createElement n -- TODO move to virtual-dom

mapLifecycle :: (Nav, ResultsViewMode) -> Maybe MapCommand
mapLifecycle (NavSearch, ResultsMap)  = Just MapInit
mapLifecycle (_, _)                   = Just MapDestroy

postToMarkerIO :: Sink PostAction -> Post -> IO (Maybe Marker)
postToMarkerIO uploadImage p = do
  -- minfo <- renderToString' $ itemMarkup uploadImage p
  -- return $ Marker <$> (Point <$> (P.latitude p) <*> (P.longitude p)) <*> (Just . Just $ BalloonString minfo)
  minfo <- renderToDOMNode $ itemMarkup uploadImage p
  return $ Marker <$> (Point <$> (P.latitude p) <*> (P.longitude p)) <*> (Just . Just $ BalloonDOMNode minfo)

showResultsOnMap mapSink uploadImage mbPosts = do
  mapSink $ ClearMap
  mbms <- mapM (postToMarkerIO uploadImage) (Data.Maybe.fromMaybe [] mbPosts)
  mapSink $ AddClusterLayer $ Data.Maybe.catMaybes mbms

-- UX notice: not using `withBusy` here because this is not a user provoked action,
-- so there is no need to notify a user, just do the job in background
loadTrackedHashtags = P.getTrackedHashtags                                                         :: IO (Either AppError [P.TrackedHashtag])

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

  -- load tracked hash tags initially (at the very start of the app, before login)
  (thtsInitSink, thtsInitE)        <- newEventOf (undefined                                        :: [P.TrackedHashtag])
  void . forkIO $ do
    z <- loadTrackedHashtags
    case z of
      Left e     -> notifSink . Just . apiError $ "Failed to load tracked hashtags"
      Right thts -> synchronously . thtsInitSink $ thts

  -- update tracked hash tags heuristics. Better: use websockets and FRP to push updates directly from the server
  let n                            = Lubeck.FRP.filter (NavSearch ==) (updates navS)
  thtsReloadE                      <- withErrorIO notifSink $ fmap (\_ -> loadTrackedHashtags) n   :: IO (Events [P.TrackedHashtag])
  thtsB                            <- stepper [] (thtsInitE <> thtsReloadE)                        :: IO (Behavior [P.TrackedHashtag])

  (searchView, searchRequested)    <- formComponentExtra1 thtsB initPostQuery (searchForm (utctDay now))
  (uploadImage', uploadedImage)    <- newEventOf (undefined                                        :: PostAction)
  (srchResSink', srchResEvents)    <- newEventOf (undefined                                        :: Maybe [Post])

  let uploadImage                  = synchronously . uploadImage'
  let srchResSink                  = synchronously . srchResSink'

  (mapView, mapSink, _)            <- mapComponent []

  results                          <- stepperS Nothing srchResEvents                               :: IO (Signal (Maybe [Post]))
  let gridView                     = fmap ((altW (text "") postSearchResultW) uploadImage) results :: Signal Html
  let resultsViewS                 = (resultsLayout (synchronously . viewModeSink)) <$> gridView <*> mapView <*> resultsViewModeS <*> results :: Signal Html
  let view                         = liftA2 (\x y -> div [] [x,y]) searchView resultsViewS         :: Signal Html

  -- This will try to destroy the map on any navigation
  -- What we need is to destroy the map just the first time a user navigates out of the search page
  -- TODO history-aware signal
  let fullNavS                     = liftA2 (,) navS resultsViewModeS                              :: Signal (Nav, ResultsViewMode)
  let resetMapS                    = fmap mapLifecycle fullNavS                                    :: Signal (Maybe MapCommand)

  subscribeEvent (updates results) (showResultsOnMap mapSink uploadImage)

  subscribeEvent (updates resetMapS) $ \x -> void . forkIO $ case x of
    Nothing -> return ()
    Just x  -> do
      threadDelay 20000 -- give DOM a litle time?
      mapSink $ x

      threadDelay 100000 -- give map a little time?
      curRes <- pollBehavior (current results)
      showResultsOnMap mapSink uploadImage curRes

  subscribeEvent uploadedImage $ \(UploadImage post) -> void . forkIO $ do
    mUserName <- pollBehavior mUserNameB
    case mUserName of
      Nothing       -> notifSink . Just . blError $ "No account to upload post to"
      Just userName -> do
        res <- (withBusy2 busySink (postAPIEither BD.Api.defaultAPI)) (userName <> "/upload-igpost-adlibrary/" <> showJS (P.post_id post)) ()
        case res of
          Left e        -> notifSink . Just . apiError $ "Failed to upload post to ad library : " <> showJS e
          Right (Ok s)  -> (notifSink . Just . NSuccess $ "Image uploaded successfully! :-)") >> (ipcSink ImageLibraryUpdated)
          Right (Nok s) -> notifSink . Just . apiError $ "Failed to upload post to ad library : " <> s

  subscribeEvent searchRequested $ \query -> void . forkIO $ do
    srchResSink $ Nothing -- reset previous search results

    let complexQuery = PostQuery $ complexifyPostQuery query
    eQueryId <- (withBusy2 busySink (postAPIEither BD.Api.defaultAPI)) "internal/queries" $ complexQuery
    case eQueryId of
      Left e        -> notifSink . Just . apiError $ "Failed posting query: " <> showJS e
      Right queryId -> void . forkIO $ do
        eitherPosts <- (withBusy busySink (getAPIEither BD.Api.defaultAPI)) $ "internal/queries/" <> queryId <> "/results"
        case eitherPosts of
          Left e   -> notifSink . Just . apiError $ "Failed getting query results: " <> showJS e
          Right ps -> srchResSink $ Just ps
    return ()

  return view
