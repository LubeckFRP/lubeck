{-# LANGUAGE OverloadedStrings  #-}

module BDPlatform.Pages.Accounts.Index (accountsIndexPage) where

import           Prelude                        hiding (div)
import qualified Prelude

import qualified Data.List
import qualified Data.Maybe
import           Data.Monoid
import           Control.Concurrent             (forkIO)
import           Control.Monad                  (void)

import qualified Data.JSString
import           GHCJS.Types                    (JSString)

import qualified Web.VirtualDom.Html            as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events     as Ev

import           Lubeck.App                     (Html)
import           Lubeck.Forms
import           Lubeck.Types
import           Lubeck.FRP
import           Lubeck.Util                    (newSyncEventOf)


import           BD.Types
import qualified BD.Data.Group                  as DG

import           BDPlatform.Types
import           Components.BusyIndicator       (BusyCmd (..))
import           Components.Layout

import           BDPlatform.Pages.Accounts.Search (accountSearch)
import           BDPlatform.Pages.Accounts.Manage (manageAccouns)
import           BDPlatform.Pages.Accounts.Types
import           BDPlatform.Pages.Accounts.Common
import           BDPlatform.HTMLCombinators

handlePageIPC busySink notifSink groupsListSink x = case x of
  ReloadGroupsList -> loadGroupsNames busySink notifSink groupsListSink
  _                -> return ()

accountsIndexPage :: Sink BusyCmd
                  -> Sink (Maybe Notification)
                  -> Sink IPCMessage
                  -> Behavior (Maybe JSString)
                  -> Signal Nav
                  -> IO (Signal Html)
accountsIndexPage busySink notifSink ipcSink usernameB navS = do
  (groupsListSink, groupsListE) <- newSyncEventOf (undefined :: DG.GroupsNamesList)
  groupsListS                   <- stepperS Nothing (fmap Just groupsListE)

  (pageIPCSink, pageIPCEvent) <- newSyncEventOf (undefined :: AccountsPageAction)
  subscribeEvent pageIPCEvent $ void . forkIO . handlePageIPC busySink notifSink groupsListSink

  pageIPCSink ReloadGroupsList -- XXX maybe run this on Nav to the current page only?

  accountSearchView <- accountSearch busySink notifSink ipcSink pageIPCSink usernameB groupsListS navS
  manageAccounsView <- manageAccouns busySink notifSink ipcSink pageIPCSink usernameB groupsListS navS
  compositeL        <- fullsizeLayout2 (pure 0)
                                       (mkLayoutPure' accountSearchView "Find accounts")
                                       (mkLayoutPure' manageAccounsView "Manage groups")

  return $ view compositeL
