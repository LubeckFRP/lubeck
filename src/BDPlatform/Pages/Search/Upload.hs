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

import           BDPlatform.HTMLCombinators
import           BDPlatform.Pages.Search.Instagram (searchInstagram)

type UploadFiles = [(JSString, FormDataVal)]

data UploadAction = FromComputer | FromFacebook | FromInstagram | FromDropbox
  deriving Eq

newtype UploadImg = UploadImg UploadFiles

toolbarW :: Widget (Maybe UploadAction) UploadAction
toolbarW sink action = mconcat
  [ toolbar' $ buttonGroup
      [ inlineMessage "Choose location of images:"
      , button "Computer"  (action ~== FromComputer)  [Ev.click $ \e -> sink FromComputer]
      , button "Facebook"  (action ~== FromFacebook)  [Ev.click $ \e -> sink FromFacebook]
      , button "Instagram" (action ~== FromInstagram) [Ev.click $ \e -> sink FromInstagram]
      , button "Dropbox"   (action ~== FromDropbox)   [Ev.click $ \e -> sink FromDropbox] ]
  ]

uploadFromComputerW :: Widget () UploadImg
uploadFromComputerW sink _ =
  panel' $ filesSelectWidget "images[]" (Just "image/*") True (contramapSink UploadImg sink) []

uploadImages :: JSString -> [(JSString, FormDataVal)] -> IO (Either AppError Ok)
uploadImages = Im.uploadImagesOrError

layout action toolbar uploadFromComputer  =
  contentPanel $ mconcat [ toolbar, body ]
  where
    body = case action of
             Just FromComputer  -> uploadFromComputer
             Just FromFacebook  -> E.text "from facebook"
             Just FromInstagram -> E.text "from instagram"
             Just FromDropbox   -> E.text "from dropbox"
             Nothing            -> E.text "Select an option"

uploadPage :: Sink BusyCmd
           -> Sink (Maybe Notification)
           -> Sink IPCMessage
           -> Behavior (Maybe JSString)
           -> Signal Nav
           -> IO (Signal Html)
uploadPage busySink notifSink ipcSink usernameB navS = do
  (uploadSink, uploadEvents)   <- newSyncEventOf (undefined                     :: UploadImg)

  (actionsSink, actionEvents)  <- newSyncEventOf (undefined                     :: UploadAction)
  actionsS                     <- stepperS Nothing (fmap Just actionEvents) :: IO (Signal (Maybe UploadAction))

  let uploadFromComputer       = fmap (uploadFromComputerW uploadSink) (pure ())
  let toolbarView              = fmap (toolbarW actionsSink) actionsS

  subscribeEvent uploadEvents $ \x -> void . forkIO $ case x of
    UploadImg formfiles -> do
      mbUsrname <- pollBehavior usernameB
      case mbUsrname of
        Nothing      -> notifSink . Just . blError $ "can't upload an image: no user."
        Just usrname -> do
          res <- withBusy2 busySink uploadImages usrname formfiles
          case res of
            Left e      ->  notifSink . Just . NError $ e
            Right imgId -> (notifSink . Just . NSuccess $ "Image uploaded successfully! :-)")
                        >> ipcSink ImageLibraryUpdated

    _ -> return ()

  let view                     = layout <$> actionsS <*> toolbarView <*> uploadFromComputer

  return view
