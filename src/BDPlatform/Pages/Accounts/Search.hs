
{-# LANGUAGE OverloadedStrings          #-}

module BDPlatform.Pages.Accounts.Search
  ( accountSearch
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import           Control.Monad                  (void)
import qualified Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as Set
import           Data.Time.Calendar             (Day (..))
import           Data.Time.Clock                (UTCTime (..), getCurrentTime)

import           Control.Concurrent             (forkIO)
import qualified Data.JSString
import           GHCJS.Types                    (JSString)

import qualified Web.VirtualDom                 as VD
import qualified Web.VirtualDom.Html            as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events     as Ev

import           Lubeck.App                     (Html)
import           Lubeck.Forms
import           Lubeck.Types
import           Lubeck.Forms.Interval
import           Lubeck.Forms.Select
import           Lubeck.FRP
import           Lubeck.Util                    (showIntegerWithThousandSeparators,
                                                 showJS, which, newSyncEventOf)

import           BD.Api
import           BD.Data.Account                (Account)
import qualified BD.Data.Account                as Ac
import           BD.Query.AccountQuery
import qualified BD.Query.AccountQuery          as AQ
import           BD.Types

import           BDPlatform.Types
import           BDPlatform.HTMLCombinators
import           Components.Grid
import           Components.BusyIndicator       (BusyCmd (..), withBusy, withBusy2)


data SearchResults = Pending | Empty | Found [Ac.Account]
data ViewMode = ViewMode { form :: FormViewMode
                         , results :: ResultsViewMode }
data FormViewMode = FormVisible | FormHidden
data ResultsViewMode = AllResults | DetailsView Ac.Account | ResultsHidden

searchFormW :: Day -> Widget SimpleAccountQuery (Submit SimpleAccountQuery)
searchFormW dayNow outputSink query =
  panel' $ formPanel_ [Ev.keyup $ \e -> if which e == 13 then outputSink (Submit query) else return () ]
      [ longStringWidget "Keyword"        True  (contramapSink (\new -> DontSubmit $ query { keyword = new     }) outputSink) (AQ.keyword query)
      , longStringWidget "User name"      False (contramapSink (\new -> DontSubmit $ query { username = new    }) outputSink) (AQ.username query)
      , longStringWidget "Follows"        False (contramapSink (\new -> DontSubmit $ query { follows = new     }) outputSink) (AQ.follows query)
      , longStringWidget "Mentioned by"   False (contramapSink (\new -> DontSubmit $ query { mentionedBy = new }) outputSink) (AQ.mentionedBy query)
      , longStringWidget "Mentions"       False (contramapSink (\new -> DontSubmit $ query { mentions = new    }) outputSink) (AQ.mentions query)

      , integerIntervalWidget "Followers"       (contramapSink (\new -> DontSubmit $ query { followers = new   }) outputSink) (AQ.followers query)
      , integerIntervalWidget "Number of posts" (contramapSink (\new -> DontSubmit $ query { numPosts = new    }) outputSink) (AQ.numPosts query)
      , integerIntervalWidget "Tracking status" (contramapSink (\new -> DontSubmit $ query { tier = new        }) outputSink) (AQ.tier query)

      , formRowWithLabel "Sort by"
          [ selectWidget
              [ (AccountByFollowers, "Account followers") ]
              (contramapSink (\new -> DontSubmit $ query { orderBy = new }) outputSink) (AQ.orderBy query)
          , selectWidget
              [ (Desc,  "from highest to lowest")
              , (Asc,   "from lowest to highest") ]
              (contramapSink (\new -> DontSubmit $ query { direction = new }) outputSink) (AQ.direction query)
          ]

      , formRowWithLabel "Max number of accounts returned"
          [ selectWidget
              [ (50,  "50")
              , (100, "100")
              , (200, "200")
              , (400, "400") ]
              (contramapSink (\new -> DontSubmit $ query { limit = new }) outputSink) (AQ.limit query)
          ]

      , formRowWithNoLabel' . toolbarLeft' . buttonGroupLeft $
          [ buttonOkIcon "Search!" "instagram" False [Ev.click $ \e -> outputSink $ Submit query] ]
      ]

itemMarkup :: Widget Ac.Account ResultsViewMode
itemMarkup sink account =
  E.div [A.class_ "", Ev.click $ \e -> sink $ DetailsView account]
    [ E.div [A.class_ "acc-pic"] [ E.img [A.src (Data.Maybe.fromMaybe "defaultPic" (Ac.profile_picture account))] [] ]
    , E.div [A.class_ "acc-username"] [ E.a [ A.class_ "acc-username"
                                          , Ev.click $ \e -> Ev.stopPropagation e
                                          , A.target "blank_"
                                          , A.href ("https://instagram.com/" <> Ac.username account)]
                                          [E.text $ "@" <> Ac.username account]
                                   , E.div [ A.class_ "acc-fullname"] [E.text $ Ac.full_name account]
                                   , E.div [ A.class_ "acc-bio"
                                           , A.style "display: block;"]
                                           [ E.text $ Data.Maybe.fromMaybe " " (Ac.bio account) ]]
    , E.div [A.class_ "acc-num badge badge-info"] [ E.text $ Data.Maybe.fromMaybe "N/A" $ showIntegerWithThousandSeparators <$> Ac.numposts account ]
    , E.div [A.class_ "acc-num badge badge-info"] [ E.text $ Data.Maybe.fromMaybe "N/A" $ showIntegerWithThousandSeparators <$> Ac.latest_count account ]
    , E.div [A.class_ "acc-num badge badge-info"] [ E.text $ Data.Maybe.fromMaybe "N/A" $ showIntegerWithThousandSeparators <$> Ac.numfollowing account ]
    ]

formPlaceholder :: Widget () FormViewMode
formPlaceholder sink _ = panel' . toolbar' . buttonGroupLeft' $ buttonOk "Edit search" False [Ev.click $ \e -> sink FormVisible]

detailsW :: Widget ViewMode ResultsViewMode
detailsW sink (ViewMode f r) = case r of
  DetailsView acc ->
    panel
      [ toolbar' . buttonGroupLeft' $ button "Back" True [Ev.click $ \e -> sink AllResults ]
      , panel [E.text $ showJS acc]]

  _  -> panel [E.text "No account"]

wrapResults :: Html -> SearchResults -> Maybe (Set.Set Ac.Account, GridAction Ac.Account) -> Html
wrapResults resultsV results sel =
  let sel'    = fst $ fromMaybe (Set.empty, Components.Grid.Noop) sel
      btnAttr = if Set.size sel' > 0
                  then [Ev.click $ \e -> addToGroup sel']
                  else [(VD.attribute "disabled") "true"]
      msg     = case Set.size sel' of
                  0 -> ""
                  1 -> "1 item selected"
                  x -> showJS x <> " items selected"
      resultsMsg Pending    = "Search in progress..."
      resultsMsg Empty      = "Nothing found"
      resultsMsg (Found xs) = "Found " <> showJS (length xs) <> " accounts"

  in panel [ header1' "Search Results " (resultsMsg results)
           , toolbarLeft' . buttonGroupLeft $
               [ buttonOk "Add to group" False btnAttr
               , inlineMessage msg ]
           , resultsV ]

addToGroup :: Set.Set Ac.Account -> IO ()
addToGroup sel = do
  print $ "Going to add to group " <> showJS (Set.size sel) <> " accounts"

searchRequest busySink notifSink srchResSink query = do
  srchResSink Pending -- reset previous search results

  let complexQuery = AccountQuery $ complexifyAccountQuery query
  eQueryId <- (withBusy2 busySink (postAPIEither BD.Api.defaultAPI)) "internal/queries" $ complexQuery
  case eQueryId of
    Left e        -> notifSink . Just . apiError $ "Failed posting query: " <> showJS e
    Right queryId -> void . forkIO $ do
      eitherPosts <- (withBusy busySink (getAPIEither BD.Api.defaultAPI)) $ "internal/queries/" <> queryId <> "/results"
      case eitherPosts of
        Left e   -> notifSink . Just . apiError $ "Failed getting query results: " <> showJS e
        Right ps -> srchResSink $ Found ps

layout fViewModeSink viewMode formV resultsV detailsV = case viewMode of
  ViewMode FormVisible AllResults      -> panel [formV, resultsV]
  ViewMode FormVisible ResultsHidden   -> panel [formV]
  ViewMode FormVisible (DetailsView _) -> panel [formV, detailsV]
  ViewMode FormHidden  AllResults      -> panel [formPlaceholder fViewModeSink (), resultsV]
  ViewMode FormHidden  ResultsHidden   -> panel [formPlaceholder fViewModeSink ()]
  ViewMode FormHidden  (DetailsView _) -> panel [formPlaceholder fViewModeSink (), detailsV]

accountSearch :: Sink BusyCmd
              -> Sink (Maybe Notification)
              -> Sink IPCMessage
              -> Behavior (Maybe JSString)
              -> Signal Nav
              -> IO (Signal Html)
accountSearch busySink notifSink ipcSink mUserNameB navS = do
  (fViewModeSink, fViewModeEvents) <- newSyncEventOf (undefined                                           :: FormViewMode)
  (rViewModeSink, rViewModeEvents) <- newSyncEventOf (undefined                                           :: ResultsViewMode)
  (srchResSink, srchResEvents)     <- newSyncEventOf (undefined                                           :: SearchResults)

  fViewModeS                       <- stepperS FormVisible fViewModeEvents                                :: IO (Signal FormViewMode)
  rViewModeS                       <- stepperS ResultsHidden rViewModeEvents                              :: IO (Signal ResultsViewMode)
  let viewModeS                    = ViewMode <$> fViewModeS <*> rViewModeS                               :: Signal ViewMode

  now                              <- getCurrentTime
  (formView, searchRequested)      <- formComponent initPostQuery (searchFormW (utctDay now))
  results                          <- stepperS Empty srchResEvents                                        :: IO (Signal SearchResults)

  (gridView, gridCmdsSink, gridActionE, gridItemsE, selectedB) <- gridComponent gridOptions initialItems itemMarkup
  -- XXX is it too imperative?
  subscribeEvent srchResEvents $ gridCmdsSink . searchResultsToGridCmd
  subscribeEvent gridItemsE    rViewModeSink
  subscribeEvent gridActionE   $ \x -> print $ "Got grid action in parent : " <> showJS x

  selectionSnapshotS               <- stepperS Nothing (fmap Just (snapshot selectedB gridActionE))       :: IO (Signal (Maybe (Set.Set Ac.Account, GridAction Ac.Account)))

  let detailsView                  = fmap (detailsW rViewModeSink) viewModeS                              :: Signal Html
  let resultsView                  = wrapResults <$> gridView <*> results <*> selectionSnapshotS          :: Signal Html
  let view                         = layout fViewModeSink <$> viewModeS <*> formView <*> resultsView <*> detailsView :: Signal Html

  subscribeEvent searchRequested $ void . forkIO . searchRequest busySink notifSink srchResSink
  subscribeEvent searchRequested $ \_ -> fViewModeSink FormHidden >> rViewModeSink AllResults

  return view

  where
    initPostQuery = defSimpleAccountQuery
    initViewMode  = ViewMode FormVisible ResultsHidden
    gridOptions   = Just (defaultGridOptions {deleteButton = False, otherButton = False})
    initialItems  = []
    searchResultsToGridCmd Pending    = Replace []
    searchResultsToGridCmd Empty      = Replace []
    searchResultsToGridCmd (Found xs) = Replace xs
