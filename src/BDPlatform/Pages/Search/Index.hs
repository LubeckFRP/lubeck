{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module BDPlatform.Pages.Search.Index (searchIndexPage) where

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

import           BDPlatform.Pages.Search.Instagram (searchInstagram)
import           BDPlatform.HTMLCombinators

data SearchAction = SearchUpload | SearchBDContent | SearchBDCommunity | SearchInstagram
  deriving (Show, Eq)

indexW :: Widget (Maybe SearchAction) SearchAction
indexW sink action = mconcat
  [ toolbar' $ buttonGroup
      [ button "Upload"       [markActive action SearchUpload,      Ev.click $ \e -> sink SearchUpload]
      , button "BD Content"   [markActive action SearchBDContent,   Ev.click $ \e -> sink SearchBDContent]
      , button "BD Community" [markActive action SearchBDCommunity, Ev.click $ \e -> sink SearchBDCommunity]
      , button "Instagram"    [markActive action SearchInstagram,   Ev.click $ \e -> sink SearchInstagram] ]
  ]

layout action toolbar instagrambody =
  contentPanel $ mconcat [ toolbar, body ]
  where
    body = case action of
             Just SearchUpload      -> E.text "upload"
             Just SearchBDContent   -> E.text "bd content"
             Just SearchBDCommunity -> E.text "bd community"
             Just SearchInstagram   -> instagrambody
             Nothing                -> E.text "Select an option"

searchIndexPage :: Sink BusyCmd
                -> Sink (Maybe Notification)
                -> Sink IPCMessage
                -> Behavior (Maybe JSString)
                -> Signal Nav
                -> IO (Signal Html)
searchIndexPage busySink notifSink ipcSink usernameB navS = do
  (actionsSink', actionEvents) <- newEventOf (undefined                     :: SearchAction)
  let actionsSink              = synchronously . actionsSink'
  actionsS                     <- stepperS Nothing (fmap Just actionEvents) :: IO (Signal (Maybe SearchAction))

  searchInstagramView          <- searchInstagram busySink notifSink ipcSink usernameB navS
  let toolbarView              = fmap (indexW actionsSink) actionsS

  let view                     = layout <$> actionsS <*> toolbarView <*> searchInstagramView

  return view
