{-# LANGUAGE OverloadedStrings  #-}

module BDPlatform.Pages.Search.Index (searchIndexPage) where

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
import           Lubeck.Util                    (contentPanel, newSyncEventOf,
                                                 showJS, withErrorIO)

import           BD.Types

import           BDPlatform.Types
import           Components.BusyIndicator       (BusyCmd (..))
import           Components.Layout              (fullsizeLayout4)

import           BDPlatform.Pages.Search.Instagram (searchInstagram)
import           BDPlatform.Pages.Search.Upload (uploadPage)
import           BDPlatform.HTMLCombinators


searchIndexPage :: Sink BusyCmd
                -> Sink (Maybe Notification)
                -> Sink IPCMessage
                -> Behavior (Maybe JSString)
                -> Signal Nav
                -> IO (Signal Html)
searchIndexPage busySink notifSink ipcSink usernameB navS = do
  searchInstagramView <- searchInstagram busySink notifSink ipcSink usernameB navS
  uploadView          <- uploadPage      busySink notifSink ipcSink usernameB navS
  compositeView       <- fullsizeLayout4 3 ("Upload", uploadView)
                                           ("BD Content",   pure (E.text "Nothing here"))
                                           ("BD Community", pure (E.text "Nothing here"))
                                           ("Instagram",    searchInstagramView)
  return compositeView
