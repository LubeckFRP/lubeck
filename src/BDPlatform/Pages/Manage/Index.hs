{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module BDPlatform.Pages.Manage.Index (manageIndexPage) where

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

import           Lubeck.App                     (Html, KbdEvents(..))
import           Lubeck.Forms
import           Lubeck.Types
import           Lubeck.FRP
import           Lubeck.Util                    (contentPanel, newSyncEventOf,
                                                 showJS, withErrorIO)

import           BD.Types
import qualified BD.Data.Account                as Account
import qualified BD.Data.Image                  as Im

import           BDPlatform.Types
import           Components.BusyIndicator       (BusyCmd (..))

import           BDPlatform.Pages.Manage.ImageLibrary (imageLibrary)
import           BDPlatform.HTMLCombinators

data ManageAction = PredictForPagePost | PredictForAd
  deriving (Show, Eq)

indexW :: Widget (Maybe ManageAction) ManageAction
indexW sink action = mconcat
  [ toolbar' $ buttonGroup
      [ button "Predict for Page Post" (action ~== PredictForPagePost) [Ev.click $ \e -> sink PredictForPagePost]
      , button "Predict for Ad"        (action ~== PredictForAd)       [Ev.click $ \e -> sink PredictForAd] ]
  ]

layout action toolbar libview =
  contentPanel $ mconcat [ toolbar, body, libview ]
  where
    body = case action of
             Just PredictForPagePost -> E.text "Predict for Post"
             Just PredictForAd       -> E.text "Predict for Ad"
             Nothing                 -> E.text "Select predict option"


manageIndexPage :: Sink BusyCmd
                -> Sink (Maybe Notification)
                -> Sink IPCMessage
                -> Events IPCMessage
                -> Events Account.Account
                -> IO (Signal Html, Behavior (Maybe [Im.Image]), Sink KbdEvents)
manageIndexPage busySink notifSink ipcSink ipcEvents userE = do
  (actionsSink, actionEvents)        <- newSyncEventOf (undefined                     :: ManageAction)
  actionsS                           <- stepperS Nothing (fmap Just actionEvents) :: IO (Signal (Maybe ManageAction))

  (imageLibView, imsB, imlibKbdSink) <- imageLibrary busySink notifSink ipcSink ipcEvents userE

  let toolbarView                    = fmap (indexW actionsSink) actionsS

  let view                           = layout <$> actionsS <*> toolbarView <*> imageLibView

  return (view, imsB, imlibKbdSink)
