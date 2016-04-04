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

import           BDPlatform.Pages.Accounts.Common
import           BDPlatform.Pages.Accounts.Types

data Action = LoadGroup DG.GroupName | ActionNoop | CreateNewGroup | DeleteGroup DG.Group

groupSelectW :: Widget (Maybe DG.GroupsNamesList) Action
groupSelectW actionsSink gnl' =
  toolbar $
    [ buttonGroup' $
        selectWithPromptWidget
          (makeOpts gnl)
          (contramapSink (toAction . filterGroup gnl) actionsSink)
          (firstGroupName gnl)

    -- , buttonGroup' $ -- XXX create new group only when some account selected, the same as in search results
        -- buttonOkIcon "New group" "plus" False [Ev.click $ \e -> actionsSink CreateNewGroup]
    ]

  where
    gnl = fromMaybe [] gnl'

    makeOpts gnl = zip gnl gnl

    filterGroup _ Nothing = []
    filterGroup gnl (Just grpname) = Data.List.filter (byName grpname) gnl

    byName name x = name == x

    toAction []     = ActionNoop
    toAction [x]    = LoadGroup x
    toAction (x:xs) = LoadGroup x -- XXX ???

    firstGroupName [] = ""
    firstGroupName xs = head xs

headerW :: Widget (Maybe DG.GroupsNamesList) Action
headerW x y = panel' . groupSelectW x $ y

layout header grid = panel [header, grid]

handleActions busySink notifSink gridCmdsSink act = case act of
  LoadGroup groupname -> do
    gridCmdsSink $ Replace [] -- reset grid
    (group, errors) <- withBusy busySink DG.loadGroup groupname
    mapM_ (\e -> notifSink . Just . apiError $ "Error during loading group members for group " <> groupname ) errors
    gridCmdsSink $ Replace (Set.toList (DG.members group))

  _              -> print "other act"

manageAccouns :: Sink BusyCmd
              -> Sink (Maybe Notification)
              -> Sink IPCMessage
              -> Sink AccountsPageAction
              -> Behavior (Maybe JSString)
              -> Signal (Maybe DG.GroupsNamesList)
              -> Signal Nav
              -> IO (Signal Html)
manageAccouns busySink notifSink ipcSink pageIPCSink mUserNameB groupsListS navS = do
  (actionsSink, actionsE)                                      <- newSyncEventOf (undefined :: Action)

  (gridView, gridCmdsSink, gridActionE, gridItemsE, selectedB) <- gridComponent (Just gridOptions) [] itemMarkup

  subscribeEvent actionsE $ void . forkIO . handleActions busySink notifSink gridCmdsSink

  let headerView                                               = fmap (headerW actionsSink) groupsListS
  let view                                                     = layout <$> headerView <*> gridView

  return view

  where
    gridOptions = defaultGridOptions{otherButton = False}
