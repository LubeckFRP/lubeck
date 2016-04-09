{-# LANGUAGE OverloadedStrings #-}

module BDPlatform.Pages.Accounts.Manage (manageAccouns) where

import           Prelude                          hiding (div)
import qualified Prelude

import           Control.Applicative
import           Control.Monad                    (void)
import           Data.Foldable                    (forM_)
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
import           BDPlatform.Types
import           Components.BusyIndicator         (BusyCmd (..), withBusy,
                                                   withBusy0, withBusy2)
import           Components.Grid
import           Components.Layout

import           BDPlatform.Pages.Accounts.Common
import           BDPlatform.Pages.Accounts.Types

data Action = LoadGroup DG.GroupName
            | ShowGroup DG.Group
            | ActionNoop
            | CreateNewGroup --DG.GroupName
            | DeleteGroup DG.Group
            | SaveAs {-DG.GroupName-} (Set.Set Ac.Account)

reloadGroups sink = sink ReloadGroupsList

handleActions busySink notifSink ipcSink gridCmdsSink act = case act of
  LoadGroup groupname -> do
    gridCmdsSink $ Replace [] -- reset grid
    (group, errors) <- withBusy busySink DG.loadGroup groupname
    mapM_ (\e -> notifSink . Just . apiError $ "Error during loading group members for group " <> groupname ) errors
    gridCmdsSink $ Replace (Set.toList (DG.members group))

  -- CreateNewGroup name -> do
  --   withErrorIO notifSink $ pure $ withBusy2 busySink DG.addAccountsToGroup groupName []
  --   reloadGroups ipcSink
  --
  -- SaveAs name as      -> do
  --   withErrorIO notifSink $ pure $ withBusy2 busySink DG.addAccountsToGroup name (fmap Ac.id (Set.toList as))
  --   reloadGroups ipcSink
  --
  -- DeleteGroup grp  -> do
  --   withErrorIO notifSink $ pure $ withBusy busySink DG.deleteGroup grp
  --   reloadGroups ipcSink

  ShowGroup g    -> gridCmdsSink $ Replace (Set.toList (DG.members g))

  _              -> print "other act"


groupSelector :: Sink BusyCmd -> Sink (Maybe Notification) -> Signal (Maybe DG.GroupsNamesList) -> IO (Signal Html, Signal (Maybe DG.Group))
groupSelector busySink notifSink groupsListS = do
  (nameSink, nameEv) <- newSyncEventOf (undefined :: Maybe DG.GroupName)
  (grpSink, grpEv)   <- newSyncEventOf (undefined :: DG.Group)
  gS                 <- stepperS Nothing (fmap Just grpEv)
  let v              = fmap (widget nameSink) groupsListS

  subscribeEvent (FRP.filterJust nameEv) $ void . forkIO . load grpSink

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
              (firstGroupName gnl) ]

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

actionsToolbar :: Sink Action -> Signal (Maybe DG.Group) -> Signal (Maybe (Set.Set Ac.Account, GridAction Ac.Account)) -> Signal Html
actionsToolbar actionsSink sgS selectionSnapshotS = toolbarV
  where
    toolbarV = fmap (toolbarW actionsSink) toolbarDataS
    toolbarDataS = (,) <$> sgS <*> selectionSnapshotS :: Signal (Maybe DG.Group, Maybe (Set.Set Ac.Account, GridAction Ac.Account))

    toolbarW :: Widget (Maybe DG.Group, Maybe (Set.Set Ac.Account, GridAction Ac.Account)) Action
    toolbarW sink (grp, x) =
      let sel        = fst $ fromMaybe (Set.empty, Components.Grid.Noop) x
          saveAtr    = if Set.size sel > 0
                         then [Ev.click $ \e -> sink $ SaveAs sel]
                         else [A.disabled True]
          deleteAttr = case grp of
                         Nothing -> [A.disabled True]
                         Just g  -> [Ev.click $ \e -> sink $ DeleteGroup g]
          msg        = case Set.size sel of
                         0 -> "No accounts selected"
                         1 -> "1 account selected"
                         x -> showJS x <> " accounts selected"
      in toolbar
          [ buttonGroup
              [ buttonIcon   "Save as new"   "save" False saveAtr
              , buttonDanger "Delete group"         False deleteAttr ]
          , buttonGroup
              [ buttonOkIcon "New group"     "plus" False [Ev.click $ \e -> sink CreateNewGroup] ]
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

  let toolbarL     = mkLayoutPure $ actionsToolbar actionsSink sgS selectionSnapshotS
  let selectGroupL = mkLayoutPure selectGroupV
  let gridL        = mkLayoutPure gridView
  headerL          <- verticalStackLayout2 selectGroupL toolbarL
  topL             <- verticalStackLayout2 headerL gridL

  return $ view topL

  where
    gridOptions = defaultGridOptions{otherButton = False}
