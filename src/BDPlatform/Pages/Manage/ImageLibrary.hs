{-# LANGUAGE JavaScriptFFI       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module BDPlatform.Pages.Manage.ImageLibrary
  ( imageLibrary
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import qualified Data.List
import           Data.Maybe                     (fromMaybe)
import           Data.Monoid

import           Web.VirtualDom.Html            (Property)
import qualified Web.VirtualDom.Html            as E
import qualified Web.VirtualDom.Html.Attributes as A
import           Web.VirtualDom.Html.Events     (click, keyup, value)

import           Lubeck.App                     (Html, KbdEvents(..))
import           Lubeck.Forms
import           Lubeck.Forms.File
import           Lubeck.FRP
import qualified Lubeck.FRP                     as FRP

import qualified BD.Data.Account                as Account
import qualified BD.Data.Image                  as Im

import           BDPlatform.Types
import           BDPlatform.HTMLCombinators
import           BD.Api
import           BD.Types
import           BD.Utils
import           Components.Grid
import           Components.BusyIndicator       (BusyCmd (..), withBusy, withBusy2)
import           Lubeck.Util
import           Lubeck.Types


data ImgLibraryActions = ViewPrevImg Im.Image | ViewNextImg Im.Image | ViewGalleryIndex
                       | DeleteImg Im.Image | EnhanceImg Im.Image
                       | ViewImg Im.Image
                       | ReloadLibrary

instance Show ImgLibraryActions where
  show (ViewPrevImg i)  = "ViewPrevImg " <> show (Im.id i)
  show (ViewNextImg i)  = "ViewNextImg " <> show (Im.id i)
  show ViewGalleryIndex = "ViewGalleryIndex"
  show (DeleteImg i)    = "DeleteImg "   <> show (Im.id i)
  show (EnhanceImg i)   = "EnhanceImg "  <> show (Im.id i)
  show (ViewImg i)      = "ViewImg "     <> show (Im.id i)
  show ReloadLibrary    = "ReloadLibrary"


-- view

viewImageW :: Widget Im.Image ImgLibraryActions
viewImageW sink image = do
  panel' $
    E.div [ A.class_ "library-image-view" , keyup handleKeys ]
      [ toolbarLeft
          [ buttonGroupLeft
              [ buttonLinkIcon "Prev image"      "chevron-left"  False [click $ \_ -> sink $ ViewPrevImg image]
              , buttonLinkIcon "Next image"      "chevron-right" False [click $ \_ -> sink $ ViewNextImg image]
              , buttonLinkIcon "Back to library" "undo"          False [click $ \_ -> sink $ ViewGalleryIndex] ]

          , buttonGroupLeft
              [ E.div [A.class_ "btn"] [ E.text "Prediction score:" ]
              , E.div [A.class_ "btn image-prediction"] [showImagePred $ Im.prediction image] ]

          , buttonGroupLeft
              [ buttonPrimaryIcon "Enhance" "star"    False [click $ \_ -> sink $ EnhanceImg image]
              , buttonDangerIcon  "Delete"  "trash-o" False [click $ \_ -> sink $ DeleteImg image] ]
          ]
      , E.div [A.class_ "x-media"] [ E.img [A.src imgUrl, A.class_ "library-image-view-img"] [] ]
      ]
  where
    imgUrl = fromMaybe "no url" (Im.fb_image_url image)

    handleKeys e = case which e of
      37 -> sink $ ViewPrevImg image -- <-
      39 -> sink $ ViewNextImg image -- ->
      x  -> print $ "Unknown key: " <> showJS x

imageCell actionsSink image =
  E.div [A.class_ "thumbnail custom-thumbnail-1 fit-text"]
      [ E.div [A.class_ "thumbnail-wrapper"] [ imgWithAttrs actionsSink image [] ]
      , E.p [A.class_ "image-prediction"]    [ showImagePred $ Im.prediction image ] ]

showImagePred Nothing  = E.text "No prediction"
showImagePred (Just x) = renderScore x

renderScore :: Double -> Html
renderScore x =
  E.div [A.class_ "score-container badge", A.title $ "Score: " <> showJS x]
    [ E.div [A.class_ "neg-score"] [ (if x < 0 then negativeScore x else negativeScore 0) ]
    , E.div [A.class_ "pos-score"] [ (if x >= 0 then positiveScore x else positiveScore 0) ] ]

  where
    positiveScore x = E.div [ A.class_ "good-score-bar"
                            , A.style $ "width: " <> showJS (calcScoreBarWidthPx x 69 0.2) <> "px" ] []
    negativeScore x = E.div [ A.class_ "bad-score-bar"
                            , A.style $ "width: " <> showJS (calcScoreBarWidthPx x 69 0.2) <> "px" ] []

    -- current value, max width in px, max value
    calcScoreBarWidthPx :: Double -> Int -> Double -> Int
    calcScoreBarWidthPx x maxpx maxscale = abs . round $ (fromIntegral maxpx) * x / maxscale

imgWithAttrs :: Sink ImgLibraryActions -> Im.Image -> [Property] -> Html
imgWithAttrs actionsSink image attrs =
  let imgUrl   = case Im.fb_thumb_url image of
                    Nothing  -> Im.fb_image_url image
                    Just url -> Just url
  in E.img ([ A.class_ "img-thumbnail"
          , click (\_ -> actionsSink (ViewImg image))
          , A.src (imgOrDefault imgUrl)] ++ attrs) []
  where
    imgOrDefault Nothing = "No URL"
    imgOrDefault (Just x) = x


-- business logic

processActions :: Sink BusyCmd
               -> Sink (Maybe Notification)
               -> Sink ImgLibraryActions
               -> Behavior (Maybe [Im.Image])
               -> Behavior (Maybe Account.Account)
               -> ImgLibraryActions
               -> IO (Maybe Im.Image)
processActions busySink notifSink actionsSink2 imsB accB (ViewPrevImg image) = do
  mbIms <- pollBehavior imsB
  let prevImg = case mbIms of
                  Nothing -> image
                  Just ims -> case Data.List.findIndex ((Im.id image ==) . Im.id) ims of
                                  Nothing -> image
                                  Just x  -> ims !! (if x - 1 < 0 then (length ims) - 1 else x - 1)
  return (Just prevImg)

processActions busySink notifSink actionsSink2 imsB accB (ViewNextImg image) = do
  mbIms <- pollBehavior imsB
  let nextImg = case mbIms of
                  Nothing -> image
                  Just ims -> case Data.List.findIndex ((Im.id image ==) . Im.id) ims of
                                  Nothing -> image
                                  Just x  -> ims !! (if x + 1 >= length ims then 0 else x + 1)
  return (Just nextImg)

processActions busySink notifSink actionsSink2 imsB accB x@(EnhanceImg image) = do
  mbUsr <- pollBehavior accB
  case mbUsr of
    Nothing -> do
      notifSink . Just . blError $ "can't enhance an image: no user."
      return $ Just image

    Just acc -> do
      res <- (withBusy2 busySink enhanceImage) acc image
      case res of
        Left e        -> notifSink (Just . NError $ e) >> return (Just image)
        Right (Ok _)  -> notifSink (Just . NSuccess $ "Success! The enhanced image will be added to your Image Library automatically soon :-)")
                      >> return (Just image)
        Right (Nok s) -> notifSink (Just . apiError $ s) >> return (Just image)

processActions busySink notifSink actionsSink2 imsB accB (DeleteImg image) = do
  mbUsr <- pollBehavior accB
  case mbUsr of
    Nothing -> do
      notifSink . Just . blError $ "can't delete an image: no user."
      return $ Just image

    Just acc -> do
      rly <- jsConfirm "Are you sure?"
      case rly of
        1 -> do
          res <- (withBusy2 busySink deleteImage) acc image
          case res of
            Left e        -> notifSink (Just . NError $ e) >> return (Just image)
            Right (Ok _)  -> notifSink (Just . NInfo $ "Image deleted :-(")
                          >> actionsSink2 ReloadLibrary
                          >> return Nothing
            Right (Nok s) -> notifSink (Just . apiError $ s) >> return (Just image)

        0 -> return (Just image)

processActions busySink notifSink actionsSink2 imsB accB ViewGalleryIndex = return Nothing

processActions busySink notifSink actionsSink2 imsB accB ReloadLibrary =
  actionsSink2 ReloadLibrary >> return Nothing

processActions busySink notifSink actionsSink2 imsB accB (ViewImg i) = return $ Just i

-- backend

getImages :: Account.Account -> IO (Either AppError [Im.Image])
getImages acc = Im.getAllImagesOrError (Account.username acc)

deleteImage :: Account.Account -> Im.Image -> IO (Either AppError Ok)
deleteImage acc image = Im.deleteImageOrError (Account.username acc) (Im.id image)

enhanceImage :: Account.Account -> Im.Image -> IO (Either AppError Ok)
enhanceImage acc image = Im.enhanceImageOrError (Account.username acc) (Im.id image)

-- main entry point

imageLibrary :: Sink BusyCmd
             -> Sink (Maybe Notification)
             -> Sink IPCMessage
             -> Events IPCMessage
             -> Events Account.Account
             -> IO (Signal Html, Behavior (Maybe [Im.Image]), Sink KbdEvents)
imageLibrary busySink notifSink ipcSink ipcEvents userE = do
  (actionsSink,  actionsE)  <- newSyncEventOf (undefined                                           :: ImgLibraryActions)
  (actionsSink2, actionsE2) <- newSyncEventOf (undefined                                           :: ImgLibraryActions)

  userB                     <- stepper Nothing (fmap Just userE)                                   :: IO (Behavior (Maybe Account.Account))

  let ipcLoadImgE           = filterJust $ sample userB (FRP.filter (== ImageLibraryUpdated) ipcEvents)
  let localLIE              = filterJust $ sample userB actionsE2
  let loadImgE              = userE <> ipcLoadImgE <> localLIE

  galleryE                  <- withErrorIO notifSink $ fmap (withBusy busySink getImages) loadImgE :: IO (Events [Im.Image])
  galleryB                  <- stepper Nothing (fmap Just galleryE)                                :: IO (Behavior (Maybe [Im.Image]))

  imageE                    <- reactimateIOAsync $ fmap (processActions busySink notifSink actionsSink2 galleryB userB) actionsE :: IO (Events (Maybe Im.Image))

  imageViewS                <- stepperS Nothing imageE                                             :: IO (Signal (Maybe Im.Image))
  let imageView             = fmap (fmap (viewImageW actionsSink)) imageViewS                      :: Signal (Maybe Html)
  (galleryView, gridCmdsSink, gridActionE, gridItemsE, _) <- gridComponent gridOptions initialItems imageCell

  subscribeEvent galleryE      $ gridCmdsSink . Replace
  subscribeEvent gridItemsE    actionsSink
  subscribeEvent gridActionE   $ \x -> print $ "Got grid action in parent : " <> showJS x

  (kbdSink, kbdE)           <- newSyncEventOf (undefined                                           :: KbdEvents)

  subscribeEvent kbdE $ \e -> do
    curImage <- pollBehavior (current imageViewS)
    case curImage of
      Nothing    -> return ()
      Just image -> case e of
        Key 37 -> actionsSink $ ViewPrevImg image -- left arrow
        Key 39 -> actionsSink $ ViewNextImg image -- right arrow
        Key 46 -> actionsSink $ DeleteImg image   -- delete
        Key 13 -> actionsSink $ EnhanceImg image  -- enter
        Key 38 -> actionsSink $ ViewGalleryIndex  -- up arrow
        _      -> return ()

  return (layout <$> galleryView <*> imageView, galleryB, kbdSink)

  where
    sample                     = snapshotWith const
    layout indexView imageView = fromMaybe indexView imageView
    gridOptions                = Just defaultGridOptions
    initialItems               = []
