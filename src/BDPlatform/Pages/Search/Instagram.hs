{-# LANGUAGE OverloadedStrings          #-}

module BDPlatform.Pages.Search.Instagram
  ( searchInstagram
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import           Control.Monad                  (void, when, unless)
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
import           Lubeck.Util                    (newSyncEventOf, newSyncEvent,
                                                 showIntegerWithThousandSeparators,
                                                 showJS, which, withErrorIO)

import           BD.Api
import           BD.Data.Account                (Account)
import           BD.Data.SearchPost             (SearchPost)
import qualified BD.Data.SearchPost             as P
import qualified BD.Data.Group                  as DG
import           BD.Query.PostQuery
import qualified BD.Query.PostQuery             as PQ
import           BD.Types
import           BDPlatform.HTMLCombinators
import           BDPlatform.Validators


import           BDPlatform.Types
import           Components.Grid
import           Components.Map
import           Components.BusyIndicator       (BusyCmd (..), withBusy, withBusy2)
import           Components.Layout


type Post = SearchPost

data PageAction = UploadImage Post deriving (Eq)

data HTFormViewMode  = HTFormVisible | HTFormHidden
data FormViewMode    = FormVisible | FormHidden
data ResultsViewMode = ResultsGrid | ResultsMap | ResultsHidden deriving (Show, Eq)


searchFormW :: Sink HTFormViewMode -> Sink PageAction -> Day -> Widget (Maybe DG.GroupsNamesList, ([P.TrackedHashtag], SimplePostQuery)) (Submit SimplePostQuery)
searchFormW hViewModeSink pageActionSink dayNow outputSink (groups, (trackedHTs, query)) =
  panel' $ formPanel_ [Ev.keyup $ \e -> when (which e == 13) (outputSink $ Submit query) ]  -- event delegation
      [ longStringWidget "Caption"   True  (contramapSink (\new -> DontSubmit $ query { caption = new })  outputSink) (PQ.caption query)
      , longStringWidget "Comment"   False (contramapSink (\new -> DontSubmit $ query { comment = new })  outputSink) (PQ.comment query)
      , longStringWidget "Hashtag"   False (contramapSink (\new -> DontSubmit $ query { hashTag = new })  outputSink) (PQ.hashTag query)
      , longStringWidget "User name" False (contramapSink (\new -> DontSubmit $ query { userName = new }) outputSink) (PQ.userName query)

      , integerIntervalWidget "Poster followers"    (contramapSink (\new -> DontSubmit $ query { followers = new }) outputSink) (PQ.followers query)
      , dateIntervalWidget    dayNow "Posting date" (contramapSink (\new -> DontSubmit $ query { date = new }) outputSink)      (PQ.date query)

      , formRowWithLabel "Account group"
          [ selectWithPromptWidget
              (let x = fromMaybe [] groups in zip x x)
              (contramapSink (\new -> DontSubmit $ query { accountgroup = new }) outputSink)
                             (fromMaybe "474435ed33c2aae0a145fc62d2083963ab537336" $ PQ.accountgroup query) ]
                             -- for Nothing we should provide some unique value, not matching any other choices
                             -- empty string won't work as there is a group with empty name
                             -- TODO better way - some indexes.

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

          , buttonIcon "New hashtag" "plus" False [Ev.click $ \e -> hViewModeSink HTFormVisible] ]

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
itemMarkup = wireframe
  where
    wireframe output post =
      E.div [ A.class_ "thumbnail custom-thumbnail-1 fit-text" ]
        [ thumbnailLink post
        , E.p []                        [ usernameLink post ]
        , E.p [ A.class_ "text-center"] [ likesBadge post, commentsBadge post ]
        , E.p []                        [ uploadButton output post ] ]
    thumbnailLink post =
      E.a [ A.target "_blank"
          , A.href $ fromMaybe (P.url post) (P.ig_web_url post) ]
          [ thumbnailImage post ]
    thumbnailImage post = E.img [ A.class_ "img-thumbnail", A.src (P.thumbnail_url post)] []
    usernameLink post =
      E.a [ A.target "_blank"
          , A.href $ "https://www.instagram.com/" <> P.username post]
          [ E.text $ "@" <> P.username post]
    uploadButton output post = buttonIcon_ "btn-link btn-sm btn-block" "Upload" "cloud-upload" False [Ev.click $ \_ -> output (UploadImage post) ]
    likesBadge    post = badge "heart"      "Likes count"    (showIntegerWithThousandSeparators (P.like_count post))
    commentsBadge post = badge "comments-o" "Comments count" (showIntegerWithThousandSeparators (P.comment_count post))
    badge typ title val =
      E.div [ A.class_ $ "fa fa-" <> typ <> " badge badge-info"
            , A.title title
            , A.style "margin: 0 3px;" ]
            [ E.span [ A.class_ "xbadge"
                     , A.style "margin-left: 5px;"]
                     [ E.text val] ]

renderToDOMNode :: Html -> IO DOMNode
renderToDOMNode = createElement -- TODO move to virtual-dom

postToMarkerIO :: Sink PageAction -> Post -> IO (Maybe Marker)
postToMarkerIO pageActionsSink p = do
  minfo <- renderToDOMNode $ itemMarkup pageActionsSink p
  return $ Marker <$> (Point <$> P.latitude p <*> P.longitude p) <*> (Just . Just $ BalloonDOMNode minfo)

showResultsOnMap mapSink pageActionsSink mbPosts = do
  mapSink ClearMap
  mbms <- mapM (postToMarkerIO pageActionsSink) (fromMaybe [] mbPosts)
  mapSink $ AddClusterLayer $ catMaybes mbms

--------------------------------------------------------------------------------

-- UX notice: not using `withBusy` here because this is not a user provoked action,
-- so there is no need to notify a user, just do the job in background
loadTrackedHashtags notifSink thtsInitSink = do
  z <- P.getTrackedHashtags                                                         :: IO (Either AppError [P.TrackedHashtag])
  case z of
    Left e     -> notifSink . Just . apiError $ "Failed to load tracked hashtags"
    Right thts -> thtsInitSink thts

validateHTag :: JSString -> FormValid VError
validateHTag newHTag =
  let validationResult = runValidation1 <$> longString "Hashtag" 1 80 newHTag :: Validation VError VSuccess
  in case validationResult of
        Success _  -> FormValid
        Failure es -> FormNotValid es

createHTagW :: Sink HTFormViewMode -> Widget (FormValid VError, JSString) (Submit JSString)
createHTagW hViewModeSink sink (isValid, val) =
  let (canSubmitAttr, cantSubmitMsg) = case isValid of
                                         FormValid       -> ([Ev.click $ \e -> sink $ Submit val], "")
                                         FormNotValid es -> ([A.disabled True], showValidationErrors es)
  in modalPopup' $ formPanel
      [ longStringWidget "New hashtag" True (contramapSink DontSubmit sink) val

      , formRowWithNoLabel' . toolbarLeft' . buttonGroupLeft $
          [ buttonOkIcon "Submit" "hashtag" False canSubmitAttr
          , button       "Cancel"           False [ Ev.click $ \e -> hViewModeSink HTFormHidden ]
          , inlineMessage cantSubmitMsg ]
      ]

--------------------------------------------------------------------------------

formPlaceholder :: Widget () FormViewMode
formPlaceholder fViewModeSink _ = panel' . toolbar' . buttonGroupLeft' $
  buttonOk "Edit search" False [Ev.click $ \e -> fViewModeSink FormVisible]

newSignalWithDefault x = do
  (sink, evs) <- newSyncEvent
  sig         <- stepperS x evs
  return (sink, sig)

searchInstagram :: Sink BusyCmd
                -> Sink (Maybe Notification)
                -> Sink IPCMessage
                -> Behavior (Maybe JSString)
                -> Signal (Maybe DG.GroupsNamesList)
                -> Signal Nav
                -> IO (Signal Html)
searchInstagram busySink notifSink ipcSink mUserNameB groupsListS navS = do
  (fViewModeSink, fViewModeS) <- newSignalWithDefault FormVisible
  (rViewModeSink, rViewModeS) <- newSignalWithDefault ResultsHidden
  (hViewModeSink, hViewModeS) <- newSignalWithDefault HTFormHidden

  -- load tracked hash tags initially (at the very start of the app, before login)
  (thtsInitSink, thtsInitE)        <- newSyncEventOf (undefined                                        :: [P.TrackedHashtag])
  void . forkIO $ loadTrackedHashtags notifSink thtsInitSink

  -- update tracked hash tags heuristics. Better: use websockets and FRP to push updates directly from the server
  let n                            = Lubeck.FRP.filter (NavSearch ==) (updates navS)
  thtsReloadE                      <- withErrorIO notifSink $ fmap (const P.getTrackedHashtags) n      :: IO (Events [P.TrackedHashtag])
  thtsB                            <- stepper [] (thtsInitE <> thtsReloadE)                            :: IO (Behavior [P.TrackedHashtag])

  (pageActionsSink, pageActionsEvents) <- newSyncEventOf (undefined                                    :: PageAction)
  (srchResSink, srchResEvents)     <- newSyncEventOf (undefined                                        :: Maybe [Post])

  results                          <- stepperS Nothing srchResEvents                                   :: IO (Signal (Maybe [Post]))

  now                              <- getCurrentTime
  (formView, searchRequested)      <- formComponentExtra2 thtsB (current groupsListS) initPostQuery (searchFormW hViewModeSink pageActionsSink (utctDay now))
  (htFormView, createHTagE)        <- formWithValidationComponent validateHTag "" (createHTagW hViewModeSink) :: IO (Signal Html, Events JSString)

  (mapView, mapSink, _)            <- mapComponent []
  mapSink MapInit

  (gridView, gridCmdsSink, gridActionE, gridItemsE, _) <- gridComponent gridOptions initialItems itemMarkup

  subscribeEvent srchResEvents        $ gridCmdsSink . Replace . fromMaybe []
  subscribeEvent gridItemsE           pageActionsSink
  subscribeEvent gridActionE          $ \x -> print $ "Got grid action in parent : " <> showJS x
  subscribeEvent (updates results)    $ showResultsOnMap                                mapSink pageActionsSink
  subscribeEvent pageActionsEvents    $ void . forkIO . doPageActions busySink notifSink mUserNameB
  subscribeEvent createHTagE          $ void . forkIO . doAddHTag     busySink notifSink thtsInitSink hViewModeSink
  subscribeEvent searchRequested      $ void . forkIO . doSearch      busySink notifSink srchResSink
  subscribeEvent searchRequested      $ \_ -> fViewModeSink FormHidden >> rViewModeSink ResultsGrid

  let editView  = fmap (formPlaceholder fViewModeSink) (pure ())

  formView''    <- overlayLayout (fmap htFormViewModeToIdx hViewModeS) (mkLayoutPure formView)   (mkLayoutPure htFormView)
  formView'     <- toggleLayout2 (fmap formViewModeToIdx fViewModeS)   formView'' (mkLayoutPure editView)

  (resultsView', switchSignal)  <- fullsizeLayout2' (fmap resultsViewModeToIdx rViewModeS) (mkLayoutPure' gridView "Grid") (mkLayoutPure' mapView "Map")
  resultsView'' <- toggleLayout2 (fmap resultsToResultsViewIdx results) resultsView' (mkLayoutPure (pure mempty))
  topL          <- verticalStackLayout2 formView' resultsView''

  subscribeEvent (updates switchSignal) $ invalidateMapSize mapSink

  return (view topL)

  where
    invalidateMapSize mapSink 1 = void . forkIO $ do
      -- give map element some time to appear in the DOM and obtain pixel size so map could resize itself accordingly
      threadDelay 100000
      mapSink InvalidateSize

    invalidateMapSize _       _ = return ()

    resultsViewModeToIdx ResultsGrid  = 0
    resultsViewModeToIdx ResultsMap   = 1
    resultsViewModeToIdx _            = 0

    resultsToResultsViewIdx Nothing   = 1
    resultsToResultsViewIdx _         = 0

    formViewModeToIdx FormVisible     = 0
    formViewModeToIdx FormHidden      = 1

    htFormViewModeToIdx HTFormVisible = True
    htFormViewModeToIdx HTFormHidden  = False

    initialItems                      = []
    initPostQuery                     = defSimplePostQuery
    gridOptions                       = Just (defaultGridOptions {deleteButton = False, otherButton = False, height = 250})

    doPageActions busySink notifSink mUserNameB act = case act of
      (UploadImage post) -> do
        mUserName <- pollBehavior mUserNameB
        case mUserName of
          Nothing       -> notifSink . Just . blError $ "No account to upload post to"
          Just userName -> do
            res <- withBusy2 busySink (postAPIEither BD.Api.defaultAPI) (userName <> "/upload-igpost-adlibrary/" <> showJS (P.post_id post)) ()
            case res of
              Left e        ->  notifSink . Just . apiError $ "Failed to upload post to ad library : " <> showJS e
              Right (Ok s)  -> (notifSink . Just . NSuccess $ "Image uploaded successfully! :-)")
                            >>  ipcSink ImageLibraryUpdated
              Right (Nok s) ->  notifSink . Just . apiError $ "Failed to upload post to ad library : " <> s
      _ -> return ()

    doAddHTag busySink notifSink thtsInitSink hViewModeSink tag = do
      res <- withBusy2 busySink (postAPIEither BD.Api.defaultAPI) ("fetch-tag/" <> tag) ()
      case res of
        Left e        ->  notifSink . Just . apiError $ "Failed to submit a new hashtag : " <> showJS e
        Right (Ok s)  -> (notifSink . Just . NSuccess $ "Hashtag added :-) The server will start fetching it within an hour.")
                      >> (void. forkIO $ loadTrackedHashtags notifSink thtsInitSink)
                      >>  hViewModeSink HTFormHidden
        Right (Nok s) ->  notifSink . Just . apiError $ "Failed to submit a new hashtag : " <> s

    doSearch busySink notifSink srchResSink query = do
      srchResSink Nothing -- reset previous search results
      let complexQuery = PostQuery $ complexifyPostQuery query
      eQueryId <- withBusy2 busySink (postAPIEither BD.Api.defaultAPI) "internal/queries" complexQuery
      case eQueryId of
        Left e        -> notifSink . Just . apiError $ "Failed posting query: " <> showJS e
        Right queryId -> void . forkIO $ do
          eitherPosts <- withBusy busySink (getAPIEither BD.Api.defaultAPI) $ "internal/queries/" <> queryId <> "/results"
          case eitherPosts of
            Left e   -> notifSink . Just . apiError $ "Failed getting query results: " <> showJS e
            Right ps -> srchResSink $ Just ps
