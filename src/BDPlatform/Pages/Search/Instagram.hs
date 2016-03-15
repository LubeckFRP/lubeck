{-# LANGUAGE OverloadedStrings          #-}

module BDPlatform.Pages.Search.Instagram
  ( searchInstagram
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import           Control.Monad                  (void)
import           Data.Either.Validation
import qualified Data.List
import qualified Data.Maybe
import           Data.Maybe
import           Data.Monoid
import           Data.Time.Calendar             (Day (..))
import           Data.Time.Clock                (UTCTime (..), getCurrentTime)

import           Control.Concurrent             (forkIO, threadDelay)
import qualified Data.JSString
import           GHCJS.Types                    (JSString)

import qualified Web.VirtualDom                 as VD
import           Web.VirtualDom                 (createElement, DOMNode)
import qualified Web.VirtualDom.Html            as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events     as Ev

import           Lubeck.App                     (Html)
import           Lubeck.Forms
import           Lubeck.Types
import           Lubeck.Forms.Interval
import           Lubeck.Forms.Select
import           Lubeck.FRP
import           Lubeck.Util                    (newSyncEventOf,
                                                 showIntegerWithThousandSeparators,
                                                 showJS, which, withErrorIO)

import           BD.Api
import           BD.Data.Account                (Account)
import           BD.Data.SearchPost             (SearchPost)
import qualified BD.Data.SearchPost             as P
import           BD.Query.PostQuery
import qualified BD.Query.PostQuery             as PQ
import           BD.Types
import           BDPlatform.HTMLCombinators
import           BDPlatform.Validators


import           BDPlatform.Types
import           Components.Grid
import           Components.Map
import           Components.BusyIndicator       (BusyCmd (..), withBusy, withBusy2)


type Post = SearchPost

data PageAction = UploadImage Post | ShowCreateHTagDialog | HideCreateHTagDialog deriving (Eq)

data ResultsViewMode = ResultsGrid | ResultsMap deriving (Show, Eq)

searchForm :: Sink PageAction -> Day -> Widget ([P.TrackedHashtag], SimplePostQuery) (Submit SimplePostQuery)
searchForm pageActionSink dayNow outputSink (trackedHTs, query) =
  panel' $ formPanel_ [Ev.keyup $ \e -> if which e == 13 then outputSink (Submit query) else return () ]  -- event delegation
      [ longStringWidget "Caption"   True  (contramapSink (\new -> DontSubmit $ query { caption = new })  outputSink) (PQ.caption query)
      , longStringWidget "Comment"   False (contramapSink (\new -> DontSubmit $ query { comment = new })  outputSink) (PQ.comment query)
      , longStringWidget "Hashtag"   False (contramapSink (\new -> DontSubmit $ query { hashTag = new })  outputSink) (PQ.hashTag query)
      , longStringWidget "User name" False (contramapSink (\new -> DontSubmit $ query { userName = new }) outputSink) (PQ.userName query)

      , integerIntervalWidget "Poster followers"    (contramapSink (\new -> DontSubmit $ query { followers = new }) outputSink) (PQ.followers query)
      , dateIntervalWidget    dayNow "Posting date" (contramapSink (\new -> DontSubmit $ query { date = new }) outputSink)      (PQ.date query)

      , formRowWithLabel "Sort by"
          [ selectWidget
              [ (PostByFollowers, "Poster followers")
              , (PostByLikes,     "Likes")
              , (PostByComments,  "Comments")
              , (PostByCreated,   "Posting time") ]
              (contramapSink (\new -> DontSubmit $ query { orderBy = new }) outputSink) (PQ.orderBy query)
          , selectWidget
              [ (Desc,  "from highest to lowest")
              , (Asc,   "from lowest to highest") ]
              (contramapSink (\new -> DontSubmit $ query { direction = new }) outputSink) (PQ.direction query) ]

      , formRowWithLabel "Tracked hashtags"
          [ selectWithPromptWidget
              (zip (P.tag <$> trackedHTs) (P.tag <$> trackedHTs)) -- FIXME
              (contramapSink (\new -> DontSubmit $ query { trackedHashTag = new }) outputSink) (fromMaybe "" $ PQ.trackedHashTag query)

          , buttonIcon "New hashtag" "plus" False [Ev.click $ \e -> pageActionSink ShowCreateHTagDialog] ]

      , formRowWithLabel "Max number of posts returned"
          [ selectWidget
              [ (50,  "50")
              , (100, "100")
              , (200, "200")
              , (400, "400") ]
              (contramapSink (\new -> DontSubmit $ query { limit = new }) outputSink) (PQ.limit query) ]

      , formRowWithNoLabel' . toolbarLeft' . buttonGroupLeft $
          [ buttonOkIcon "Search!" "instagram" False [Ev.click $ \e -> outputSink $ Submit query] ]
      ]

itemMarkup :: Widget Post PageAction
itemMarkup output post =
  E.div [ A.class_ "thumbnail custom-thumbnail-1 fit-text" ]
    [ E.a [ A.target "_blank"
          , A.href $ fromMaybe (P.url post) (P.ig_web_url post) ]
          [ E.img [ A.class_ "img-thumbnail", A.src (P.thumbnail_url post)] [] ]

    , E.p [] [ E.a [ A.target "_blank"
                   , A.href $ "https://www.instagram.com/" <> P.username post]
                   [ E.text $ "@" <> P.username post] ]

    , E.p [ A.class_ "text-center"]
            [ E.div [ A.class_ "fa fa-heart badge badge-info"
                    , A.style "margin: 0 3px;"
                    , A.title "Likes count" ]
                    [ E.span [ A.class_ "xbadge"
                             , A.style "margin-left: 5px;"]
                             [ E.text $ showIntegerWithThousandSeparators (P.like_count post)] ]

            , E.div [ A.class_ "fa fa-comments-o badge badge-info"
                    , A.title "Comments count"
                    , A.style "margin: 0 3px;" ]
                    [ E.span [ A.class_ "xbadge"
                             , A.style "margin-left: 5px;"]
                             [ E.text $ showIntegerWithThousandSeparators (P.comment_count post)] ]
            ]

    , E.p [] [ buttonIcon_ "btn-link btn-sm btn-block" "Upload" "cloud-upload" False [Ev.click $ \_ -> output (UploadImage post) ] ]
    ]

resultsLayout :: Sink ResultsViewMode -> Html -> Html -> ResultsViewMode -> Maybe [Post] -> Html
resultsLayout sink gridH mapH mode posts = case mode of
  ResultsGrid -> wrapper sink gridH True  False posts
  ResultsMap  -> wrapper sink mapH  False True  posts
  where
    wrapper sink x asel bsel posts =
      let gridTab = if asel then "btn-primary" else "btn-default"
          mapTab  = if bsel then "btn-primary" else "btn-default"
      in panel
        [ header1' "Search Results " (Data.JSString.pack $ "Found " ++ show (fromMaybe 0 (length <$> posts)) ++ " posts")
        , toolbar' . buttonGroup $ -- TODO tabs widget
              [ buttonIcon_ gridTab "Grid" "th"    False [Ev.click $ \e -> if asel then return () else sink ResultsGrid]
              , buttonIcon_ mapTab  "Map"  "map-o" False [Ev.click $ \e -> if bsel then return () else sink ResultsMap] ]
          , x
        ]

renderToDOMNode :: Html -> IO DOMNode
renderToDOMNode n = createElement n -- TODO move to virtual-dom

mapLifecycle :: (Nav, ResultsViewMode) -> Maybe MapCommand
mapLifecycle (NavSearch, ResultsMap)  = Just MapInit
mapLifecycle (_, _)                   = Just MapDestroy

postToMarkerIO :: Sink PageAction -> Post -> IO (Maybe Marker)
postToMarkerIO pageActionsSink p = do
  minfo <- renderToDOMNode $ itemMarkup pageActionsSink p
  return $ Marker <$> (Point <$> (P.latitude p) <*> (P.longitude p)) <*> (Just . Just $ BalloonDOMNode minfo)

showResultsOnMap mapSink pageActionsSink mbPosts = do
  mapSink $ ClearMap
  mbms <- mapM (postToMarkerIO pageActionsSink) (fromMaybe [] mbPosts)
  mapSink $ AddClusterLayer $ catMaybes mbms

--------------------------------------------------------------------------------

-- UX notice: not using `withBusy` here because this is not a user provoked action,
-- so there is no need to notify a user, just do the job in background
loadTrackedHashtags notifSink thtsInitSink = do
  z <- P.getTrackedHashtags                                                         :: IO (Either AppError [P.TrackedHashtag])
  case z of
    Left e     -> notifSink . Just . apiError $ "Failed to load tracked hashtags"
    Right thts -> thtsInitSink $ thts

validateHTag :: JSString -> FormValid VError
validateHTag newHTag =
  let validationResult = runValidation1 <$> longString "Hashtag" 1 80 newHTag :: Validation VError VSuccess
  in case validationResult of
        Success _  -> FormValid
        Failure es -> FormNotValid es

createHTagW :: Sink PageAction -> Widget (FormValid VError, JSString) (Submit JSString)
createHTagW actionsSink sink (isValid, val) =
  let (canSubmitAttr, cantSubmitMsg) = case isValid of
                                         FormValid       -> ([Ev.click $ \e -> sink $ Submit val], "")
                                         FormNotValid es -> ([(VD.attribute "disabled") "true"], showValidationErrors es)
  in modalPopup' $ formPanel
      [ longStringWidget "New hashtag" True (contramapSink (\new -> DontSubmit new ) sink) val

      , formRowWithNoLabel' . toolbarLeft' . buttonGroupLeft $
          [ buttonOkIcon "Submit" "hashtag" False canSubmitAttr
          , button       "Cancel"           False [ Ev.click $ \e -> actionsSink HideCreateHTagDialog ]
          , inlineMessage cantSubmitMsg ]
      ]

--------------------------------------------------------------------------------

layout :: Bool -> Html -> Html -> Html -> Html
layout showHT sv rv chtv =
  E.div [] ([sv, rv] <> (if showHT then [chtv] else []))

searchInstagram :: Sink BusyCmd
                -> Sink (Maybe Notification)
                -> Sink IPCMessage
                -> Behavior (Maybe JSString)
                -> Signal Nav
                -> IO (Signal Html)
searchInstagram busySink notifSink ipcSink mUserNameB navS = do
  let initPostQuery                = defSimplePostQuery

  (viewModeSink, viewModeEvents)   <- newSyncEventOf (undefined                                        :: ResultsViewMode)
  resultsViewModeS                 <- stepperS ResultsGrid viewModeEvents

  now                              <- getCurrentTime

  -- load tracked hash tags initially (at the very start of the app, before login)
  (thtsInitSink, thtsInitE)        <- newSyncEventOf (undefined                                        :: [P.TrackedHashtag])
  void . forkIO $ loadTrackedHashtags notifSink thtsInitSink

  -- update tracked hash tags heuristics. Better: use websockets and FRP to push updates directly from the server
  let n                            = Lubeck.FRP.filter (NavSearch ==) (updates navS)
  thtsReloadE                      <- withErrorIO notifSink $ fmap (\_ -> P.getTrackedHashtags) n   :: IO (Events [P.TrackedHashtag])
  thtsB                            <- stepper [] (thtsInitE <> thtsReloadE)                        :: IO (Behavior [P.TrackedHashtag])

  (pageActionsSink, pageActionsEvents) <- newSyncEventOf (undefined                                   :: PageAction)
  (srchResSink, srchResEvents)     <- newSyncEventOf (undefined                                        :: Maybe [Post])

  (searchView, searchRequested)    <- formComponentExtra1 thtsB initPostQuery (searchForm pageActionsSink (utctDay now))

  (mapView, mapSink, _)            <- mapComponent []

  results                          <- stepperS Nothing srchResEvents                               :: IO (Signal (Maybe [Post]))

  (createHTagView, createHTagE)    <- formWithValidationComponent validateHTag "" (createHTagW pageActionsSink) :: IO (Signal Html, Events JSString)
  createHTagS                      <- stepperS False (fmap (\x -> if x == ShowCreateHTagDialog then True else False)
                                                           (Lubeck.FRP.filter (\x -> (x == ShowCreateHTagDialog)
                                                                                  || (x == HideCreateHTagDialog))
                                                                              pageActionsEvents))

  (gridView, gridCmdsSink, gridActionE, gridItemsE) <- gridComponent gridOptions initialItems itemMarkup
  subscribeEvent srchResEvents $ gridCmdsSink . Replace . fromMaybe []
  subscribeEvent gridItemsE    pageActionsSink
  subscribeEvent gridActionE   $ \x -> print $ "Got grid action in parent : " <> showJS x

  let resultsViewS                 = resultsLayout viewModeSink <$> gridView <*> mapView <*> resultsViewModeS <*> results :: Signal Html
  let view                         = layout <$> createHTagS <*> searchView <*> resultsViewS <*> createHTagView         :: Signal Html

  -- This will try to destroy the map on any navigation
  -- What we need is to destroy the map just the first time a user navigates out of the search page
  -- TODO history-aware signal
  let fullNavS                     = liftA2 (,) navS resultsViewModeS                              :: Signal (Nav, ResultsViewMode)
  let resetMapS                    = fmap mapLifecycle fullNavS                                    :: Signal (Maybe MapCommand)

  subscribeEvent (updates results) (showResultsOnMap mapSink pageActionsSink)

  subscribeEvent (updates resetMapS) $ \x -> void . forkIO $ case x of
    Nothing -> return ()
    Just x  -> do
      threadDelay 20000 -- give DOM a litle time?
      mapSink $ x

      threadDelay 100000 -- give map a little time?
      curRes <- pollBehavior (current results)
      showResultsOnMap mapSink pageActionsSink curRes

  subscribeEvent pageActionsEvents $ \act -> void . forkIO $ case act of
    (UploadImage post) -> do
      mUserName <- pollBehavior mUserNameB
      case mUserName of
        Nothing       -> notifSink . Just . blError $ "No account to upload post to"
        Just userName -> do
          res <- (withBusy2 busySink (postAPIEither BD.Api.defaultAPI)) (userName <> "/upload-igpost-adlibrary/" <> showJS (P.post_id post)) ()
          case res of
            Left e        ->  notifSink . Just . apiError $ "Failed to upload post to ad library : " <> showJS e
            Right (Ok s)  -> (notifSink . Just . NSuccess $ "Image uploaded successfully! :-)") >> (ipcSink ImageLibraryUpdated)
            Right (Nok s) ->  notifSink . Just . apiError $ "Failed to upload post to ad library : " <> s
    _ -> return ()

  subscribeEvent createHTagE $ \tag -> void. forkIO $ do
      res <- (withBusy2 busySink (postAPIEither BD.Api.defaultAPI)) ("fetch-tag/" <> tag) ()
      case res of
        Left e        ->  notifSink . Just . apiError $ "Failed to submit a new hashtag : " <> showJS e
        Right (Ok s)  -> (notifSink . Just . NSuccess $ "Hashtag added :-) The server will start fetching it within an hour.")
                      >> (void. forkIO $ loadTrackedHashtags notifSink thtsInitSink)
                      >> (pageActionsSink HideCreateHTagDialog)
        Right (Nok s) ->  notifSink . Just . apiError $ "Failed to submit a new hashtag : " <> s

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

  where
    initialItems = []
    gridOptions   = Just (defaultGridOptions {deleteButton = False, otherButton = False})
