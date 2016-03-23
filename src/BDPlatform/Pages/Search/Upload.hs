{-# LANGUAGE OverloadedStrings  #-}

module BDPlatform.Pages.Search.Upload (uploadPage) where

import           Prelude                           hiding (div)
import qualified Prelude

import           Control.Concurrent                (forkIO)
import           Control.Monad                     (void)

import qualified Data.List
import qualified Data.Maybe
import           Data.Monoid

import qualified Data.JSString
import           GHCJS.Types                       (JSString)
import           JavaScript.Web.XMLHttpRequest     (FormDataVal (..))

import qualified Web.VirtualDom.Html               as E
import qualified Web.VirtualDom.Html.Attributes    as A
import qualified Web.VirtualDom.Html.Events        as Ev

import           Lubeck.App                        (Html)
import           Lubeck.Forms
import           Lubeck.Forms.File
import           Lubeck.FRP
import           Lubeck.Types
import           Lubeck.Util                       (contentPanel, newSyncEventOf,
                                                    showJS, withErrorIO)

import           BD.Api
import qualified BD.Data.Image                     as Im
import           BD.Types

import           BDPlatform.Types
import           Components.BusyIndicator          (BusyCmd (..), withBusy2)
import           Components.Layout              (fullsizeLayout4)

import           BDPlatform.HTMLCombinators
import           BDPlatform.Pages.Search.Instagram (searchInstagram)

type UploadFiles = [(JSString, FormDataVal)]
newtype UploadImg = UploadImg UploadFiles

uploadFromComputerW :: Widget () UploadImg
uploadFromComputerW sink _ =
  panel' $ filesSelectWidget "images[]" (Just "image/*") True (contramapSink UploadImg sink) []

handleUpload busySink notifSink ipcSink usernameB x = case x of
  UploadImg formfiles -> do
    mbUsrname <- pollBehavior usernameB
    case mbUsrname of
      Nothing      -> notifSink . Just . blError $ "can't upload an image: no user."
      Just usrname -> do
        res <- withBusy2 busySink Im.uploadImagesOrError usrname formfiles
        case res of
          Left e      ->  notifSink . Just . NError $ e
          Right imgId -> (notifSink . Just . NSuccess $ "Image uploaded successfully! :-)")
                      >> ipcSink ImageLibraryUpdated

  _ -> return ()

uploadPage :: Sink BusyCmd
           -> Sink (Maybe Notification)
           -> Sink IPCMessage
           -> Behavior (Maybe JSString)
           -> Signal Nav
           -> IO (Signal Html)
uploadPage busySink notifSink ipcSink usernameB navS = do
  (uploadSink, uploadEvents)   <- newSyncEventOf (undefined                     :: UploadImg)
  let uploadFromComputer       = fmap (uploadFromComputerW uploadSink) (pure ())

  subscribeEvent uploadEvents $ void . forkIO . handleUpload busySink notifSink ipcSink usernameB

  compositeView <- fullsizeLayout4 3 ("Computer",  uploadFromComputer)
                                     ("Facebook",  pure (E.text "Upload from Facebook here"))
                                     ("Instagram", pure (E.text "Upload from Instagram here"))
                                     ("Dropbox",   pure (E.text "Upload from Dropbox here"))
  return compositeView
