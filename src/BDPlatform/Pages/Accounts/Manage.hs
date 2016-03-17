{-# LANGUAGE OverloadedStrings          #-}

module BDPlatform.Pages.Accounts.Manage (manageAccouns) where

import qualified Data.Set as Set
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
import qualified Lubeck.FRP                     as FRP
import           Lubeck.Util

import           BD.Api
import           BD.Data.Account                (Account)
import qualified BD.Data.Account                as Ac
import qualified BD.Data.Group                  as DG
import           BD.Query.AccountQuery
import qualified BD.Query.AccountQuery          as AQ
import           BD.Types

import           BDPlatform.Types
import           BDPlatform.HTMLCombinators
import           Components.Grid
import           Components.BusyIndicator       (BusyCmd (..), withBusy0, withBusy, withBusy2)

import           BDPlatform.Pages.Accounts.Common

data Action = LoadGroup DG.GroupName | ActionNoop | CreateNewGroup | DeleteGroup DG.Group

headerW :: Widget (Maybe DG.GroupsNamesList) Action
headerW actionsSink x = case x of
  Nothing -> go []
  Just gnl -> go gnl
  where
    go gnl =
      panel $
        [ toolbar
            [ buttonGroup' $
                selectWithPromptWidget
                  (makeOpts gnl)
                  (contramapSink (g . f gnl) actionsSink)
                  (firstGroupName gnl)

            , buttonGroup' $
                buttonOkIcon "New group" "plus" False [Ev.click $ \e -> actionsSink CreateNewGroup]
            ]
        ]

    makeOpts gnl = zip gnl gnl

    f _ Nothing = []
    f gnl (Just grpname) = Data.List.filter (byName grpname) gnl

    byName name x = name == x

    g []     = ActionNoop
    g [x]    = LoadGroup x
    g (x:xs) = LoadGroup x -- XXX ???

    firstGroupName [] = ""
    firstGroupName xs = head xs

layout header grid = panel [header, grid]

handleActions busySink notifSink gridCmdsSink act = case act of
  LoadGroup groupname -> do
    (group, errors) <- (withBusy busySink DG.loadGroup) groupname
    mapM_ (\e -> notifSink . Just . apiError $ "Error during loading group members for group " <> groupname ) errors
    gridCmdsSink $ Replace (Set.toList (DG.members group))

  _              -> print "other act"

loadGroupsNames busySink notifSink groupsListSink = do
  -- XXX do not use withBusy here?
  res  <- withBusy0 busySink DG.loadGroupsNames >>= eitherToError notifSink
  case res of
    Nothing -> return ()
    Just xs -> groupsListSink xs

manageAccouns :: Sink BusyCmd
              -> Sink (Maybe Notification)
              -> Sink IPCMessage
              -> Behavior (Maybe JSString)
              -> Signal Nav
              -> IO (Signal Html)
manageAccouns busySink notifSink ipcSink mUserNameB navS = do
  (groupsListSink, groupsListE)                                <- newSyncEventOf (undefined :: DG.GroupsNamesList)
  (actionsSink, actionsE)                                      <- newSyncEventOf (undefined :: Action)

  (gridView, gridCmdsSink, gridActionE, gridItemsE, selectedB) <- gridComponent (Just gridOptions) [] itemMarkup

  subscribeEvent actionsE $ void . forkIO . handleActions busySink notifSink gridCmdsSink
  void . forkIO $ loadGroupsNames busySink notifSink groupsListSink

  groupsListS                                                  <- stepperS Nothing (fmap Just groupsListE)
  let headerView                                               = fmap (headerW actionsSink) groupsListS
  let view                                                     = layout <$> headerView <*> gridView

  return view

  where
    gridOptions = defaultGridOptions{otherButton = False}
