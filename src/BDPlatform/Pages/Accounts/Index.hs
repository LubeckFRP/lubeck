{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module BDPlatform.Pages.Accounts.Index (accountsIndexPage) where

import           Prelude                        hiding (div)
import qualified Prelude

import qualified Data.List
import qualified Data.Maybe
import           Data.Monoid

import qualified Data.JSString
import           GHCJS.Concurrent               (synchronously)
import           GHCJS.Types                    (JSString)

import qualified Web.VirtualDom.Html            as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events     as Ev

import           Lubeck.App                     (Html)
import           Lubeck.Forms
import           Lubeck.Types
import           Lubeck.FRP
import           Lubeck.Util                    (contentPanel, newEventOf,
                                                 showJS, withErrorIO)

import           BD.Types

import           BDPlatform.Types
import           Components.BusyIndicator       (BusyCmd (..))

import           BDPlatform.Pages.Accounts.Search (accountSearch)
import           BDPlatform.HTMLCombinators

data AccountsAction = FindAccounts | MagageGroups
  deriving (Show, Eq)

indexW :: Widget (Maybe AccountsAction) AccountsAction
indexW sink action = mconcat
  [ toolbar' $ buttonGroup
      [ button "Find accounts" (action ~== FindAccounts) [Ev.click $ \e -> sink FindAccounts]
      , button "Manage groups" (action ~== MagageGroups) [Ev.click $ \e -> sink MagageGroups] ]
  ]

layout action toolbar accountsearch =
  contentPanel $ mconcat [ toolbar, body ]
  where
    body = case action of
             Just FindAccounts -> accountsearch
             Just MagageGroups -> E.text "manage groups"
             Nothing           -> E.text "Select an option"

accountsIndexPage :: Sink BusyCmd
                  -> Sink (Maybe Notification)
                  -> Sink IPCMessage
                  -> Behavior (Maybe JSString)
                  -> Signal Nav
                  -> IO (Signal Html)
accountsIndexPage busySink notifSink ipcSink usernameB navS = do
  (actionsSink', actionEvents) <- newEventOf (undefined                     :: AccountsAction)
  let actionsSink              = synchronously . actionsSink'
  actionsS                     <- stepperS (Just FindAccounts) (fmap Just actionEvents) :: IO (Signal (Maybe AccountsAction))

  accountSearchView            <- accountSearch busySink notifSink ipcSink usernameB navS
  let toolbarView              = fmap (indexW actionsSink) actionsS

  let view                     = layout <$> actionsS <*> toolbarView <*> accountSearchView

  return view
