{-# LANGUAGE OverloadedStrings          #-}

module BDPlatform.Pages.Accounts.Types where

import           Data.Maybe
import           Lubeck.FRP

import qualified BD.Data.Group                  as DG
import           Components.BusyIndicator       (BusyCmd (..))
import           BDPlatform.Types
import           Lubeck.Types

data AccountsPageAction = ReloadGroupsList

data Ctx = Ctx
  { _busySink   :: Sink BusyCmd
  , _notifSink  :: Sink (Maybe Notification)
  , _pageIPC    :: Sink AccountsPageAction
  , _groupsList :: Signal (Maybe DG.GroupsNamesList)
  }
