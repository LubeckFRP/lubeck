{-# LANGUAGE OverloadedStrings          #-}


module Components.ConfirmDialog
  ( confirmDialogComponent
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Data.Maybe                       (fromMaybe)

import qualified Web.VirtualDom.Html              as E
import qualified Web.VirtualDom.Html.Attributes   as A
import qualified Web.VirtualDom.Html.Events       as Ev

import           Lubeck.FRP
import qualified Lubeck.FRP                       as FRP
import           Lubeck.App                       (Html)
import qualified Data.JSString
import           GHCJS.Types                      (JSString)
import           Lubeck.Util
import           BDPlatform.HTMLCombinators


-- TODO make overlay layout to work with signal of maybe html for overlay
-- this will remove a need for showDeleteConfirmSig
confirmDialogComponent :: IO (Signal Html, Signal Bool, Sink (JSString, (Bool -> IO ())))
confirmDialogComponent = do
  (askSink, askEv)           <- newSyncEventOf (undefined :: (JSString, (Bool -> IO ())))
  (showHideSink, showHideEv) <- newSyncEventOf (undefined :: Bool)
  showDeleteConfirmSig       <- stepperS False showHideEv

  subscribeEvent askEv $ const $ showHideSink True -- show dialog

  asksS <- stepperS Nothing (fmap Just askEv)
  let v = fmap (widget showHideSink) asksS
  return (v, showDeleteConfirmSig, askSink)

  where
    widget showHideSink mbq =
      let (q, f) = fromMaybe ("huh?", (const . return $ ())) mbq
      in modalPopup' $ formPanel
        [ E.div [A.class_ "confirm-dialog-body"] [E.text q]
        , toolbar' . buttonGroup $
            [ buttonOkIcon "Ok"     "ok" False [ Ev.click $ \e -> f True  >> showHideSink False ] -- hide dialog
            , button       "Cancel"      False [ Ev.click $ \e -> f False >> showHideSink False ]
            ]
        ]
