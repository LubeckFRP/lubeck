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

data Action = ShowGroup (Maybe DG.Group)
            | ActionNoop
            | CreateNewGroup DG.GroupName
            | DeleteGroup DG.Group
            | SaveAs DG.GroupName (Set.Set Ac.Account)
            | DeleteMembers DG.Group [Ac.Account]

data ToolbarPopupActions = ShowSaveAs | ShowCreate | ShowNone | ShowDeleteConfirm
  deriving Eq

reloadGroups sink = sink ReloadGroupsList

handleActions busySink notifSink ipcSink gridCmdsSink act = case act of
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

      False -> notifSink . Just . apiError $ "Can't delete non-empty group"

  ShowGroup g' -> case g' of
    Just g  -> gridCmdsSink $ Replace (Set.toList (DG.members g))
    Nothing -> gridCmdsSink $ Replace []

  DeleteMembers g xs -> do
    withBusy2 busySink DG.removeAccountsFromGroup (DG.name g) (fmap Ac.id xs)
      >>= mapM_ (eitherToError notifSink) . Data.List.filter isLeft
    reloadGroups ipcSink

  _              -> print "other act"

-- TODO make overlay layout to work with signal of maybe html for overlay
-- this will remove a need for showDeleteConfirmSig
confirmDialogComponent :: IO (Signal Html, Signal Bool, Sink (JSString, (Bool -> IO ())))
confirmDialogComponent = do
  (askSink, askEv)           <- newSyncEventOf (undefined :: (JSString, (Bool -> IO ())))
  (showHideSink, showHideEv) <- newSyncEventOf (undefined :: Bool)
  showDeleteConfirmSig       <- stepperS False showHideEv

  subscribeEvent askEv $ const $ showHideSink True -- show dialog

  asksS <- stepperS Nothing (fmap Just askEv)
  let v = fmap (widget showHideSink) asksS
  return (v, showDeleteConfirmSig, askSink)

  where
    widget showHideSink mbq =
      let (q, f) = fromMaybe ("huh?", (const . return $ ())) mbq
      in modalPopup' $ formPanel
        [ E.div [A.class_ "confirm-dialog-body"] [E.text q]
        , toolbar' . buttonGroup $
            [ buttonOkIcon "Ok"     "ok" False [ Ev.click $ \e -> f True  >> showHideSink False ] -- hide dialog
            , button       "Cancel"      False [ Ev.click $ \e -> f False >> showHideSink False ]
            ]
        ]


groupSelector :: Sink BusyCmd -> Sink (Maybe Notification) -> Signal (Maybe DG.GroupsNamesList) -> IO (Signal Html, Signal (Maybe DG.Group))
groupSelector busySink notifSink groupsListS = do
  (nameSink, nameEv) <- newSyncEventOf (undefined :: Maybe DG.GroupName)
  (grpSink, grpEv)   <- newSyncEventOf (undefined :: Maybe DG.Group)
  gS                 <- stepperS Nothing grpEv
  let xxx            = (,) <$> groupsListS <*> gS :: Signal (Maybe DG.GroupsNamesList, Maybe DG.Group)
  let v              = fmap (widget nameSink) xxx

  subscribeEvent (FRP.filterJust nameEv) $ void . forkIO . load grpSink
  subscribeEvent (updates groupsListS) $ \gl -> do
    curGrp <- pollBehavior (current gS)
    case Data.List.elem <$> (DG.name <$> curGrp) <*> gl of
      Just True  -> nameSink $ DG.name <$> curGrp
      Just False -> grpSink Nothing
      Nothing    -> return ()

  return (v, gS)

  where
    load sink groupname = do
      (group, errors) <- withBusy busySink DG.loadGroup groupname
      mapM_ (\e -> notifSink . Just . apiError $ "Error during loading group members for group " <> groupname ) errors
      sink $ Just group

    groupSelectW :: Widget (Maybe DG.GroupsNamesList, Maybe DG.Group) (Maybe DG.GroupName)
    groupSelectW sink (gnl', curGrp') =
      toolbar' $
        buttonGroup' $
          selectWithPromptWidget
            (makeOpts gnl)
            (contramapSink (toAction . filterGroup gnl) sink)
            curGrp

      where
        gnl = fromMaybe [] gnl'

        curGrp = fromMaybe "non-existing-option" (DG.name <$> curGrp')

        makeOpts gnl = let x = Data.List.sort gnl in zip x x

        filterGroup _ Nothing = []
        filterGroup gnl (Just grpname) = Data.List.filter (byName grpname) gnl

        byName name x = name == x

        toAction []     = Nothing
        toAction [x]    = Just x
        toAction (x:xs) = Just x -- XXX ???

        firstGroupName [] = ""
        firstGroupName xs = head xs

    widget :: Widget (Maybe DG.GroupsNamesList, Maybe DG.Group) (Maybe DG.GroupName)
    widget x y = panel' . groupSelectW x $ y

actionsToolbar :: Sink Action -> Signal (Maybe DG.Group) -> Signal (Maybe (Set.Set Ac.Account, GridAction Ac.Account)) -> IO Layout
actionsToolbar actionsSink sgS selectionSnapshotS = do
  (popupSink, popupEvnt)  <- newSyncEventOf (undefined :: ToolbarPopupActions)
  popupSig                <- stepperS ShowNone popupEvnt -- TODO helper newSyncSignalOf

  (newGroupV, submitNGe)  <- formWithValidationComponent validateGroupname Nothing inputGroupNameW :: IO (Signal Html, Events (Maybe JSString))
  (saveAsV, submitSAe)    <- formWithValidationComponent validateGroupname Nothing inputGroupNameW :: IO (Signal Html, Events (Maybe JSString))
  (confirmDeleteV, toggleShowS, confirm) <- confirmDialogComponent

  subscribeEvent (FRP.filterJust submitNGe) $ actionsSink . CreateNewGroup

  subscribeEvent (FRP.filterJust submitSAe) $ \n -> do
    sel' <- pollBehavior (current selectionSnapshotS)
    let sel = fst $ fromMaybe (Set.empty, Components.Grid.Noop) sel'
    actionsSink $ SaveAs n sel

  subscribeEvent (FRP.filter (== ShowDeleteConfirm) popupEvnt) $ const $ do -- TODO first only
    curGrp <- pollBehavior (current sgS) -- XXX ???
    case curGrp of
      Just g  -> confirm ( "Are you sure you want to delete group " <> DG.name g <> "?"
                         , \r -> if r then actionsSink (DeleteGroup g) else return ())
      Nothing -> return ()

  subscribeEvent submitNGe $ const . popupSink $ ShowNone
  subscribeEvent submitSAe $ const . popupSink $ ShowNone

  let toolbarV = fmap (toolbarW popupSink actionsSink) toolbarDataS
  toolbarL     <- overlayLayout toggleShowS (mkLayoutPure toolbarV) (mkLayoutPure confirmDeleteV)
  topL         <- overlayLayout2 (fmap f popupSig) toolbarL
                                                   (mkLayoutPure newGroupV)
                                                   (mkLayoutPure saveAsV)
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

  (confirmDialogV, toggleConfigmDialogS, confirm)              <- confirmDialogComponent


  let showGroupE = fmap ShowGroup (updates sgS)
  subscribeEvent (actionsE <> showGroupE) $ void . forkIO . handleActions busySink notifSink pageIPCSink gridCmdsSink

  subscribeEvent (fmap fromDelete (FRP.filter isDelete gridActionE)) $ \xs ->
    pollBehavior (current sgS) >>= \g -> case g of
      Just g' -> confirm ( "Do you really want to delete " <> formatAccountsToDelete xs <> " from the group " <> DG.name g' <> "?"
                         , \r -> if r then actionsSink (DeleteMembers g' xs) else return () )
      Nothing -> return ()

  toolbarL         <- actionsToolbar actionsSink sgS selectionSnapshotS
  let selectGroupL = mkLayoutPure selectGroupV
  gridL            <- overlayLayout toggleConfigmDialogS (mkLayoutPure gridView) (mkLayoutPure confirmDialogV)
  headerL          <- verticalStackLayout2 selectGroupL toolbarL
  topL             <- verticalStackLayout2 headerL gridL

  return $ view topL

  where
    gridOptions = defaultGridOptions{otherButton = False}

    formatAccountsToDelete as | length as == 0 = "no account"
                              | length as == 1 = "account " <> Ac.username (head as)
                              | length as < 5  = "accounts " <> joinWith ", " (fmap Ac.username (take 4 as))
                              | length as > 4  = "accounts " <> joinWith ", " (fmap Ac.username (take 3 as))
                                                             <> " and " <> showJS (length as - 3) <> " other accounts"

    joinWith = Data.JSString.intercalate

    isDelete :: GridAction a -> Bool
    isDelete (Delete _)   = True
    isDelete _            = False

    -- fromDelete :: GridAction a -> a
    fromDelete (Delete x) = x
