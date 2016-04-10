{-# LANGUAGE OverloadedStrings #-}

module BDPlatform.Pages.Accounts.Manage (manageAccouns) where

import           Prelude                          hiding (div)
import qualified Prelude

import           Control.Applicative
import           Control.Monad                    (void, when)
import           Data.Foldable                    (forM_)
import           Data.Either                      (isLeft)
import           Data.Either.Validation
import qualified Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                         as Set
import           Data.Time.Calendar               (Day (..))
import           Data.Time.Clock                  (UTCTime (..), getCurrentTime)

import           Control.Concurrent               (forkIO)
import qualified Data.JSString
import           GHCJS.Types                      (JSString)

import qualified Web.VirtualDom                   as VD
import qualified Web.VirtualDom.Html              as E
import qualified Web.VirtualDom.Html.Attributes   as A
import qualified Web.VirtualDom.Html.Events       as Ev

import           Lubeck.App                       (Html)
import           Lubeck.Forms
import           Lubeck.Forms.Interval
import           Lubeck.Forms.Select
import           Lubeck.FRP
import qualified Lubeck.FRP                       as FRP
import           Lubeck.Types
import           Lubeck.Util

import           BD.Api
import           BD.Data.Account                  (Account)
import qualified BD.Data.Account                  as Ac
import qualified BD.Data.Group                    as DG
import           BD.Query.AccountQuery
import qualified BD.Query.AccountQuery            as AQ
import           BD.Types

import           BDPlatform.HTMLCombinators
import           BDPlatform.Validators
import           BDPlatform.Types
import           Components.BusyIndicator         (BusyCmd (..), withBusy,
                                                   withBusy0, withBusy2)
import           Components.Grid
import           Components.Layout

import           BDPlatform.Pages.Accounts.Common
import           BDPlatform.Pages.Accounts.Types

data Action = ShowGroup DG.Group
            | ActionNoop
            | CreateNewGroup DG.GroupName
            | DeleteGroup DG.Group
            | SaveAs DG.GroupName (Set.Set Ac.Account)
            | DeleteMembers DG.Group [Ac.Account]
            -- | LoadGroup DG.GroupName

data ToolbarPopupActions = ShowSaveAs | ShowCreate | ShowNone | ShowDeleteConfirm

reloadGroups sink = sink ReloadGroupsList

-- loadGroup busySink notifSink gridCmdsSink groupname = do
--   gridCmdsSink $ Replace [] -- reset grid
--   (group, errors) <- withBusy busySink DG.loadGroup groupname
--   mapM_ (\e -> notifSink . Just . apiError $ "Error during loading group members for group " <> groupname ) errors
--   gridCmdsSink $ Replace (Set.toList (DG.members group))

handleActions busySink notifSink ipcSink gridCmdsSink act = case act of
  -- LoadGroup groupname -> loadGroup busySink notifSink gridCmdsSink groupname

  CreateNewGroup name -> do
    withBusy busySink DG.undeleteGroup' name >>= eitherToError notifSink
    reloadGroups ipcSink

  SaveAs name as      -> do
    withBusy2 busySink DG.addAccountsToGroup name (fmap Ac.id (Set.toList as))
      >>= mapM_ (eitherToError notifSink) . Data.List.filter isLeft
    reloadGroups ipcSink

  DeleteGroup grp  -> case Set.null $ DG.members grp of
      True  -> do
        withBusy busySink DG.deleteGroup grp >>= eitherToError notifSink
        reloadGroups ipcSink
        gridCmdsSink $ Replace [] -- reset grid TODO select other group

      False -> notifSink . Just . apiError $ "Can't delete non-empty group"

  ShowGroup g -> gridCmdsSink $ Replace (Set.toList (DG.members g))

  DeleteMembers g xs -> do
    withBusy2 busySink DG.removeAccountsFromGroup (DG.name g) (fmap Ac.id xs)
      >>= mapM_ (eitherToError notifSink) . Data.List.filter isLeft
    reloadGroups ipcSink
    -- loadGroup busySink notifSink gridCmdsSink (DG.name g) -- TODO push changes form groupSelector

  _              -> print "other act"

confirmDialogComponent :: JSString -> IO (Signal Html, Events Bool)
confirmDialogComponent prompt = do
  (sink, ev) <- newSyncEventOf (undefined :: Bool)
  evS <- stepperS False ev
  let v = fmap (widget sink) evS
  return (v, ev)
  where
    widget sink _ =
      modalPopup' $ formPanel
        [ E.div [A.class_ "confirm-dialog-body"] [E.text prompt]
        , toolbar' . buttonGroup $
            [ buttonOkIcon "Ok"     "ok" False [ Ev.click $ \e -> sink True ]
            , button       "Cancel"      False [ Ev.click $ \e -> sink False ]
            ]
        ]

groupSelector :: Sink BusyCmd -> Sink (Maybe Notification) -> Signal (Maybe DG.GroupsNamesList) -> IO (Signal Html, Signal (Maybe DG.Group))
groupSelector busySink notifSink groupsListS = do
  (nameSink, nameEv) <- newSyncEventOf (undefined :: Maybe DG.GroupName)
  (grpSink, grpEv)   <- newSyncEventOf (undefined :: DG.Group)
  gS                 <- stepperS Nothing (fmap Just grpEv)
  let v              = fmap (widget nameSink) groupsListS

  subscribeEvent (FRP.filterJust nameEv) $ void . forkIO . load grpSink
  subscribeEvent (updates groupsListS) $ const $ pollBehavior (current gS) >>= nameSink . fmap DG.name

  return (v, gS)

  where
    load sink groupname = do
      (group, errors) <- withBusy busySink DG.loadGroup groupname
      mapM_ (\e -> notifSink . Just . apiError $ "Error during loading group members for group " <> groupname ) errors
      sink group

    groupSelectW :: Widget (Maybe DG.GroupsNamesList) (Maybe DG.GroupName)
    groupSelectW sink gnl' =
      toolbar $
        [ buttonGroup' $
            selectWithPromptWidget
              (makeOpts gnl)
              (contramapSink (toAction . filterGroup gnl) sink)
              "select no group (firstGroupName gnl)" ]

      where
        gnl = fromMaybe [] gnl'

        makeOpts gnl = zip gnl gnl

        filterGroup _ Nothing = []
        filterGroup gnl (Just grpname) = Data.List.filter (byName grpname) gnl

        byName name x = name == x

        toAction []     = Nothing
        toAction [x]    = Just x
        toAction (x:xs) = Just x -- XXX ???

        firstGroupName [] = ""
        firstGroupName xs = head xs

    widget :: Widget (Maybe DG.GroupsNamesList) (Maybe DG.GroupName)
    widget x y = panel' . groupSelectW x $ y

actionsToolbar :: Sink Action -> Signal (Maybe DG.Group) -> Signal (Maybe (Set.Set Ac.Account, GridAction Ac.Account)) -> IO Layout
actionsToolbar actionsSink sgS selectionSnapshotS = do
  (popupSink, popupEvnt) <- newSyncEventOf (undefined :: ToolbarPopupActions)
  popupSig               <- stepperS ShowNone popupEvnt -- TODO helper newSyncSignalOf

  (newGroupV, submitNGe) <- formWithValidationComponent validateGroupname Nothing inputGroupNameW :: IO (Signal Html, Events (Maybe JSString))
  (saveAsV, submitSAe)   <- formWithValidationComponent validateGroupname Nothing inputGroupNameW :: IO (Signal Html, Events (Maybe JSString))
  (confirmDeleteV, delEv) <- confirmDialogComponent "Are you sure?"

  subscribeEvent (FRP.filterJust submitNGe) $ actionsSink . CreateNewGroup
  subscribeEvent (FRP.filterJust submitSAe) $ \n -> do
    sel' <- pollBehavior (current selectionSnapshotS)
    let sel = fst $ fromMaybe (Set.empty, Components.Grid.Noop) sel'
    actionsSink $ SaveAs n sel
  subscribeEvent (FRP.filter (== True) delEv) $ const $ do -- TODO first only
    curGrp <- pollBehavior (current sgS) -- XXX ???
    case curGrp of
      Just g  -> actionsSink $ DeleteGroup g
      Nothing -> return ()

  subscribeEvent submitNGe $ const . popupSink $ ShowNone
  subscribeEvent submitSAe $ const . popupSink $ ShowNone
  subscribeEvent delEv     $ const . popupSink $ ShowNone

  let toolbarV = fmap (toolbarW popupSink actionsSink) toolbarDataS
  topL         <- overlayLayout3 (fmap f popupSig) (mkLayoutPure toolbarV)
                                                   (mkLayoutPure newGroupV)
                                                   (mkLayoutPure saveAsV)
                                                   (mkLayoutPure confirmDeleteV)
  return topL

  where
    f ShowNone          = 0
    f ShowCreate        = 1
    f ShowSaveAs        = 2
    f ShowDeleteConfirm = 3


    toolbarDataS = (,) <$> sgS <*> selectionSnapshotS :: Signal (Maybe DG.Group, Maybe (Set.Set Ac.Account, GridAction Ac.Account))

    inputGroupNameW :: Widget (FormValid VError, Maybe DG.GroupName) (Submit (Maybe DG.GroupName))
    inputGroupNameW outputSink (isValid, val) =
      let (canSubmitAttr, cantSubmitMsg) = case isValid of
                                             FormValid       -> ([Ev.click $ \e -> outputSink $ Submit val], "")
                                             FormNotValid es -> ([A.disabled True], showValidationErrors es)
      in modalPopup' $ formPanel_ [Ev.keyup $ \e -> when (which e == 13) $ outputSink (Submit val) ]
        [ longStringWidget "Group name" False (contramapSink (\new -> DontSubmit (Just new)) outputSink) (fromMaybe "" val)

        , formRowWithNoLabel' . toolbarLeft' . buttonGroupLeft $
            [ buttonOkIcon "Submit" "group" False canSubmitAttr
            , button       "Cancel"         False [ Ev.click $ \e -> outputSink (Submit Nothing) ]
            , inlineMessage cantSubmitMsg ]
        ]

    toolbarW :: Sink ToolbarPopupActions -> Widget (Maybe DG.Group, Maybe (Set.Set Ac.Account, GridAction Ac.Account)) Action
    toolbarW popupSink sink (grp, x) =
      let sel        = fst $ fromMaybe (Set.empty, Components.Grid.Noop) x
          saveAtr    = if Set.size sel > 0
                         then [Ev.click $ \e -> popupSink ShowSaveAs]
                         else [A.disabled True]
          deleteAttr = case grp of
                         Nothing -> [A.disabled True]
                         Just g  -> [Ev.click $ \e -> popupSink ShowDeleteConfirm]
          msg        = case Set.size sel of
                         0 -> "No accounts selected"
                         1 -> "1 account selected"
                         x -> showJS x <> " accounts selected"
      in toolbar
          [ buttonGroup
              [ buttonIcon   "Save as new"   "save" False saveAtr
              , buttonDanger "Delete group"         False deleteAttr ]
          , buttonGroup
              [ buttonOkIcon "New group"     "plus" False [Ev.click $ \e -> popupSink ShowCreate] ]
          , buttonGroup
              [ inlineMessage msg ]
          ]

manageAccouns :: Ctx -> IO (Signal Html)
manageAccouns (Ctx busySink notifSink pageIPCSink groupsListS) = do
  (actionsSink, actionsE)                                      <- newSyncEventOf (undefined                                     :: Action)
  (gridView, gridCmdsSink, gridActionE, gridItemsE, selectedB) <- gridComponent (Just gridOptions) [] itemMarkup
  selectionSnapshotS                                           <- stepperS Nothing (fmap Just (snapshot selectedB gridActionE)) :: IO (Signal (Maybe (Set.Set Ac.Account, GridAction Ac.Account)))
  (selectGroupV, sgS)                                          <- groupSelector busySink notifSink groupsListS

  let showGroupE = fmap ShowGroup (FRP.filterJust $ updates sgS)
  subscribeEvent (actionsE <> showGroupE) $ void . forkIO . handleActions busySink notifSink pageIPCSink gridCmdsSink

  subscribeEvent (fmap fromDelete (FRP.filter isDelete gridActionE)) $ \xs -> do
    g <- pollBehavior (current sgS)
    case g of
      Just g -> actionsSink $ DeleteMembers g xs
      Nothing -> return ()

  toolbarL         <- actionsToolbar actionsSink sgS selectionSnapshotS
  let selectGroupL = mkLayoutPure selectGroupV
  let gridL        = mkLayoutPure gridView
  headerL          <- verticalStackLayout2 selectGroupL toolbarL
  topL             <- verticalStackLayout2 headerL gridL

  return $ view topL

  where
    gridOptions = defaultGridOptions{otherButton = False}

    isDelete :: GridAction a -> Bool
    isDelete (Delete _)   = True
    isDelete _            = False

    -- fromDelete :: GridAction a -> a
    fromDelete (Delete x) = x
