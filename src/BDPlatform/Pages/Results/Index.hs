{-# LANGUAGE OverloadedStrings  #-}

module BDPlatform.Pages.Results.Index (resultsIndexPage) where

import           Prelude                        hiding (div)
import qualified Prelude

import           GHCJS.Types                    (JSString)

import qualified Web.VirtualDom.Html            as E

import           Lubeck.App                     (Html)
import           Lubeck.Types
import           Lubeck.FRP
import           BD.Types
import           BDPlatform.Types
import           Components.BusyIndicator       (BusyCmd (..))
import           Components.Layout

import           BDPlatform.Pages.Results.Interactions



resultsIndexPage :: Sink BusyCmd
                 -> Sink (Maybe Notification)
                 -> Sink IPCMessage
                 -> Behavior (Maybe JSString)
                 -> Signal Nav
                 -> IO (Signal Html)
resultsIndexPage busySink notifSink ipcSink usernameB navS = do
  interactionsV <- interactionsPage busySink notifSink
  compositeL    <- fullsizeLayout2 (pure 0)
                                   (mkLayoutPure' (pure (E.text "Hello")) "Hello")
                                   (mkLayoutPure' interactionsV "Interactions")
  return $ view compositeL
