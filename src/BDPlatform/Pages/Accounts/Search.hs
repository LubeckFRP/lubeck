
{-# LANGUAGE OverloadedStrings          #-}

module BDPlatform.Pages.Accounts.Search
  ( accountSearch
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import           Control.Monad                  (void, when)
import           Data.Either.Validation
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
import qualified Lubeck.FRP                     as FRP
import           Lubeck.Util                    (showIntegerWithThousandSeparators,
                                                 showJS, which, newSyncEventOf)

import           BD.Api
import           BD.Data.Account                (Account)
import qualified BD.Data.Account                as Ac
import qualified BD.Data.Group                  as DG
import           BD.Query.AccountQuery
import qualified BD.Query.AccountQuery          as AQ
import           BD.Types

import           BDPlatform.Types
import           BDPlatform.Validators
import           BDPlatform.HTMLCombinators
import           Components.Layout
import           Components.Grid
import           Components.BusyIndicator       (BusyCmd (..), withBusy, withBusy2)

import           BDPlatform.Pages.Accounts.Common
import           BDPlatform.Pages.Accounts.Types


data SearchResults = Pending | Empty | Found [Ac.Account]
data FormViewMode  = FormVisible | FormHidden
data AddToGroupViewMode = ATGVisible | ATGHidden


--------------------------------------------------------------------------------
-- TODO move to Common
selectCreateGroupW :: Widget (FormValid VError, (DG.GroupsNamesList, Maybe DG.GroupName)) (Submit (Maybe DG.GroupName))
selectCreateGroupW outputSink (isValid, (gnl, val)) =
  let (canSubmitAttr, cantSubmitMsg) = case isValid of
                                         FormValid       -> ([Ev.click $ \e -> outputSink $ Submit val], "")
                                         FormNotValid es -> ([A.disabled True], showValidationErrors es)
  in modalPopup' $ formPanel_ [Ev.keyup $ \e -> when (which e == 13) $ outputSink (Submit val) ]
    [ formRowWithLabel "Select a group"
        [ selectWithPromptWidget
            (makeOpts gnl)
            (contramapSink (toAction . filterGroup gnl) outputSink)
            (firstGroupName gnl)
        ]
    , longStringWidget "or create one" False (contramapSink (\new -> DontSubmit (Just new)) outputSink) (fromMaybe "" val)

    , formRowWithNoLabel' . toolbarLeft' . buttonGroupLeft $
        [ buttonOkIcon "Submit" "group" False canSubmitAttr
        , button       "Cancel"         False [ Ev.click $ \e -> outputSink (Submit Nothing) ]
        , inlineMessage cantSubmitMsg ]
    ]
  where
    makeOpts gnl = zip gnl gnl

    filterGroup _ Nothing = []
    filterGroup gnl (Just grpname) = Data.List.filter (byName grpname) gnl

    byName name x = name == x

    toAction []     = DontSubmit Nothing
    toAction [x]    = DontSubmit $ Just x
    toAction (x:xs) = DontSubmit $ Just x -- XXX ???

    -- firstGroupName [] = ""
    -- firstGroupName xs = head xs
    firstGroupName _ = "474435ed33c2aae0a145fc62d2083963ab537336" -- select no group
--------------------------------------------------------------------------------

searchFormW :: Day -> Widget SimpleAccountQuery (Submit SimpleAccountQuery)
searchFormW dayNow outputSink query =
  panel' $ formPanel_ [Ev.keyup $ \e -> when (which e == 13) $ outputSink (Submit query) ]
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

formPlaceholder :: Widget () FormViewMode
formPlaceholder sink _ = panel' . toolbar' . buttonGroupLeft' $ buttonOk "Edit search" False [Ev.click $ \e -> sink FormVisible]

detailsW :: Widget ResultsViewMode ResultsViewMode
detailsW sink r = case r of
  AccountDetails acc ->
    panel
      [ toolbar' . buttonGroupLeft' $ button "Back" True [Ev.click $ \e -> sink ResultsGrid ]
      , panel [E.text $ showJS acc]]

  _  -> panel [E.text "No account"]

doSearchRequest busySink notifSink srchResSink query = do
  srchResSink Pending -- reset previous search results

  let complexQuery = AccountQuery $ complexifyAccountQuery query
  eQueryId <- withBusy2 busySink (postAPIEither BD.Api.defaultAPI) "internal/queries" complexQuery
  case eQueryId of
    Left e        -> notifSink . Just . apiError $ "Failed posting query: " <> showJS e
    Right queryId -> void . forkIO $ do
      eitherPosts <- withBusy busySink (getAPIEither BD.Api.defaultAPI) $ "internal/queries/" <> queryId <> "/results"
      case eitherPosts of
        Left e   -> notifSink . Just . apiError $ "Failed getting query results: " <> showJS e
        Right ps -> srchResSink $ Found ps

searchFormComp :: IO (Layout, Events SimpleAccountQuery)
searchFormComp = do
  now                                      <- getCurrentTime
  (localViewModeSink, localViewModeEvents) <- newSyncEventOf (undefined :: FormViewMode)
  localViewModeSignal                      <- stepperS FormVisible localViewModeEvents
  (formView, searchRequested)              <- formComponent initPostQuery (searchFormW (utctDay now))
  let placeholderView                      = fmap (formPlaceholder localViewModeSink) (pure ())

  subscribeEvent searchRequested $ \_ -> localViewModeSink FormHidden -- >> rViewModeSink AllResults

  l <- toggleLayout2 (fmap f localViewModeSignal) (mkLayoutPure formView) (mkLayoutPure placeholderView)

  return (l, searchRequested)

  where
    f FormHidden  = 1
    f FormVisible = 0

    initPostQuery = defSimpleAccountQuery

doAddToGroup :: Sink AddToGroupViewMode
             -> Signal (Maybe (Set.Set Ac.Account, GridAction Ac.Account))
             -> Maybe DG.GroupName
             -> IO ()
doAddToGroup _ _ {-busySink notifSink-} Nothing = return ()
doAddToGroup popupSink selectionSnapshotS {-busySink notifSink-} (Just groupName) = do
  x <- pollBehavior (current selectionSnapshotS)
  let sel = fst $ fromMaybe (Set.empty, Components.Grid.Noop) x
  let as  = fmap Ac.id (Set.toList sel)
  r <- DG.addAccountsToGroup groupName as
  -- when no errors r -- ?
  popupSink ATGHidden
  return ()


gridAndAddToGroupComp :: Layout -> Signal (Maybe (Set.Set Ac.Account, GridAction Ac.Account)) -> Ctx -> IO Layout
gridAndAddToGroupComp gridL selectionSnapshotS ctx = do
  (popupSink, popupEvnt) <- newSyncEventOf (undefined :: AddToGroupViewMode)
  popupSig               <- stepperS ATGHidden popupEvnt -- TODO helper newSyncSignalOf

  let groupsListS = fmap (fromMaybe []) (_groupsList ctx)

  (popupV, groupNameSelected) <- formWithValidationComponentExtra1 (current groupsListS)
                                                                   validateGroupname
                                                                   Nothing
                                                                   selectCreateGroupW -- :: IO (Signal Html, Events DG.GroupName)

  let wrappedV = wrapper popupSink <$> selectionSnapshotS <*> (view gridL)

  subscribeEvent groupNameSelected $ \x -> void . forkIO $ do
    doAddToGroup popupSink selectionSnapshotS x
    _pageIPC ctx $ ReloadGroupsList
  subscribeEvent (FRP.filter (== Nothing) groupNameSelected) $ const . popupSink $ ATGHidden

  let popupL       = mkLayoutPure popupV
  let wrappedGridL = mkLayoutPure wrappedV
  l <- overlayLayout (fmap f popupSig) wrappedGridL popupL
  return l

  where
    f ATGVisible = True
    f ATGHidden  = False

    wrapper :: Sink AddToGroupViewMode -> Maybe (Set.Set Ac.Account, GridAction Ac.Account) -> Html -> Html
    wrapper popupSink sel x =
      let sel'    = fst $ fromMaybe (Set.empty, Components.Grid.Noop) sel
          btnAttr = if Set.size sel' > 0
                      then [Ev.click $ \e -> popupSink ATGVisible {-sel'-}]
                      else [A.disabled True]
          msg     = case Set.size sel' of
                      0 -> "Select some items first"
                      1 -> "1 item selected"
                      x -> showJS x <> " items selected"

      in panel [ header1 "Search Results "
               , toolbarLeft' . buttonGroupLeft $
                   [ buttonOk "Add to group" False btnAttr
                   , inlineMessage msg ]
               , x ]


gridOrDetailsComp :: Layout -> Events ResultsViewMode -> Signal (Maybe (Set.Set Ac.Account, GridAction Ac.Account)) -> Ctx -> IO Layout
gridOrDetailsComp gridL gridItemsE selectionSnapshotS ctx = do
  (rvmSink, rvmEvts) <- newSyncEventOf (undefined :: ResultsViewMode)
  rvmSig <- stepperS ResultsGrid rvmEvts -- TODO helper newSyncSignalOf

  subscribeEvent gridItemsE rvmSink

  gridAndPopupL <- gridAndAddToGroupComp gridL selectionSnapshotS ctx

  let detailsV = fmap (detailsW rvmSink) rvmSig
  let detailsL = mkLayoutPure detailsV
  l <- toggleLayout2 (fmap f rvmSig) gridAndPopupL detailsL

  return l

  where
    f ResultsGrid        = 0
    f (AccountDetails _) = 1

resultsOrNoneComp :: Signal SearchResults -> Ctx -> IO Layout
resultsOrNoneComp results ctx = do
  (gridView, gridCmdsSink, gridActionE, gridItemsE, selectedB) <- gridComponent gridOptions initialItems itemMarkup

  subscribeEvent (updates results) $ gridCmdsSink . searchResultsToGridCmd
  selectionSnapshotS <- stepperS Nothing (fmap Just (snapshot selectedB gridActionE))       :: IO (Signal (Maybe (Set.Set Ac.Account, GridAction Ac.Account)))

  let emptyL = mkLayoutPure (pure mempty)
  let gridL  = mkLayoutPure gridView
  gridL'     <- gridOrDetailsComp gridL gridItemsE selectionSnapshotS ctx
  resL       <- toggleLayout2 (fmap f results) gridL' emptyL

  return resL

  where
    f Pending   = 1
    f Empty     = 1
    f (Found _) = 0

    gridOptions   = Just (defaultGridOptions {deleteButton = False, otherButton = False})
    initialItems  = []

    searchResultsToGridCmd Pending    = Replace []
    searchResultsToGridCmd Empty      = Replace []
    searchResultsToGridCmd (Found xs) = Replace xs


accountSearch :: Ctx -> IO (Signal Html)
accountSearch (Ctx busySink notifSink pageIPCSink groupsListS) = do
  (srchResSink, srchResEvents) <- newSyncEventOf (undefined    :: SearchResults)
  results                      <- stepperS Empty srchResEvents :: IO (Signal SearchResults)

  (srchFormL, searchRequested) <- searchFormComp
  resultsL                     <- resultsOrNoneComp results (Ctx busySink notifSink pageIPCSink groupsListS)

  subscribeEvent searchRequested $ void . forkIO . doSearchRequest busySink notifSink srchResSink

  topL <- verticalStackLayout2 srchFormL resultsL

  return (view topL)
