{-# LANGUAGE OverloadedStrings  #-}

module BDPlatform.Pages.Accounts.Index (accountsIndexPage) where

import           Prelude                        hiding (div)
import qualified Prelude

import qualified Data.List
import qualified Data.Maybe
import           Data.Monoid

import qualified Data.JSString
import           GHCJS.Types                    (JSString)

import qualified Web.VirtualDom.Html            as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events     as Ev

import           Lubeck.App                     (Html)
import           Lubeck.Forms
import           Lubeck.Types
import           Lubeck.FRP

import           BD.Types

import           BDPlatform.Types
import           Components.BusyIndicator       (BusyCmd (..))
import           Components.Layout

import           BDPlatform.Pages.Accounts.Search (accountSearch)
import           BDPlatform.Pages.Accounts.Manage (manageAccouns)
import           BDPlatform.HTMLCombinators


accountsIndexPage :: Sink BusyCmd
                  -> Sink (Maybe Notification)
                  -> Sink IPCMessage
                  -> Behavior (Maybe JSString)
                  -> Signal Nav
                  -> IO (Signal Html)
accountsIndexPage busySink notifSink ipcSink usernameB navS = do
  accountSearchView <- accountSearch busySink notifSink ipcSink usernameB navS
  manageAccounsView <- manageAccouns busySink notifSink ipcSink usernameB navS
  compositeL        <- fullsizeLayout2 (pure 0)
                                       (mkLayoutPure' accountSearchView "Find accounts")
                                       (mkLayoutPure' manageAccounsView "Manage groups")

  return $ view compositeL
