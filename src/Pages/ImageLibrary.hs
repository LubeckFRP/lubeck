{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Pages.ImageLibrary
  ( imageLibraryPage
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import JavaScript.Web.XMLHttpRequest (FormDataVal(..))

import           Control.Applicative
import qualified Data.List
import           Data.Maybe                     (fromMaybe)
import           Data.Monoid
import           Data.String                    (fromString)

import           GHCJS.Types                    (JSString)
import           Web.VirtualDom.Html            (Property, br, button, div,
                                                 form, h1, hr, img, p, table,
                                                 tbody, td, text, th, thead, tr)
import qualified Web.VirtualDom.Html            as E
import           Web.VirtualDom.Html.Attributes (class_, src, width)
import qualified Web.VirtualDom.Html.Attributes as A
import           Web.VirtualDom.Html.Events     (change, click, preventDefault,
                                                 stopPropagation, submit, value)

import           Lubeck.App                     (Html)
import           Lubeck.Forms
import           Lubeck.Forms.File
import           Lubeck.FRP

import qualified BD.Data.Account                as Account
import qualified BD.Data.Image                  as Im

import           BD.Api
import           BD.Types
import           BD.Utils
import           Lubeck.Util
import           Components.BusyIndicator (BusyCmd(..), withBusy, withBusy2)

type ImgIndex = Int
type ImgHash = Text

type UploadFiles = [(JSString, FormDataVal)]

data ImgLibraryActions = ViewPrevImg Im.Image | ViewNextImg Im.Image | ViewGalleryIndex
                       | DeleteImg Im.Image | EnhanceImg Im.Image
                       | UploadImg UploadFiles
                       | ViewImg Im.Image

instance Show ImgLibraryActions where
  show (ViewPrevImg i)  = "ViewPrevImg " <> show (Im.id i)
  show (ViewNextImg i)  = "ViewNextImg " <> show (Im.id i)
  show ViewGalleryIndex = "ViewGalleryIndex"
  show (DeleteImg i)    = "DeleteImg "   <> show (Im.id i)
  show (EnhanceImg i)   = "EnhanceImg "  <> show (Im.id i)
  show (UploadImg i)    = "UploadImg"    <> show (fmap fst i)
  show (ViewImg i)      = "ViewImg "     <> show (Im.id i)


-- view

viewImageW :: Widget Im.Image ImgLibraryActions
viewImageW sink image = do
  contentPanel $
    div [class_ "library-image-view"]
      [ div [class_ "btn-toolbar"]
          [ div [class_ "btn-group"]
              [ button [class_ "btn btn-default", click $ \_ -> sink $ ViewPrevImg image] [ text "← Prev image" ]
              , button [class_ "btn btn-default", click $ \_ -> sink $ ViewNextImg image] [ text "Next image →" ]
              , button [class_ "btn btn-default", click $ \_ -> sink $ ViewGalleryIndex]  [ text "Back to library"] ]

          , div [class_ "btn-group"]
              [ div [class_ "btn"] [ text "Prediction score:" ]
              , div [class_ "btn image-prediction"]
                  [showImagePred $ Im.prediction image] ]

          , div [class_ "btn-group"]
              [ button [class_ "btn btn-primary", click $ \_ -> sink $ EnhanceImg image] [ text "Enhance"]
              , button [class_ "btn btn-danger",  click $ \_ -> sink $ DeleteImg  image] [ text "Delete"] ]
          ]

      , div [class_ "x-media"] [ E.img [src imgUrl, class_ "library-image-view-img"] [] ]
      ]

  where
    imgUrl = fromMaybe "no url" (Im.fb_image_url image)

galleryW :: Widget [Im.Image] ImgLibraryActions
galleryW _ [] = contentPanel $ text "No images in library"

galleryW actionsSink ims =
  contentPanel $ div []
    ([ div [class_ "btn-toolbar"]
        [ button [ class_ "btn btn-default"
                 , click (\_ -> actionsSink (UploadImg [])) ]
                 [ text "Upload" ]
        , filesSelectWidget True (contramapSink (\x -> UploadImg x) actionsSink) []
        ] ]
    <> (map (imageCell actionsSink) ims))

imageCell actionsSink image =
  div [class_ "thumbnail custom-thumbnail-1 fit-text"]
      [ div [class_ "thumbnail-wrapper"] [ imgWithAttrs actionsSink image [] ]
      , p [class_ "image-prediction"]    [ showImagePred $ Im.prediction image ]
      --, p [class_ "image-hash"]          ( showImageHash $ Im.fb_image_hash image )
      ]

showImagePred Nothing  = text "No prediction"
showImagePred (Just x) = renderScore x

renderScore :: Double -> Html
renderScore x =
  div [class_ "score-container", A.title $ "Score: " <> showJS x]
    [ div [class_ "neg-score"] [ (if x < 0 then negativeScore x else mempty) ]
    , div [class_ "pos-score"] [ (if x >= 0 then positiveScore x else mempty) ] ]

  where
    positiveScore x = div [ class_ "good-score-bar"
                          , A.style $ "width: " <> showJS (calcScoreBarWidthPx x 69 0.2) <> "px" ] []
    negativeScore x = div [ class_ "bad-score-bar"
                          , A.style $ "width: " <> showJS (calcScoreBarWidthPx x 69 0.2) <> "px" ] []

    -- current value, max width in px, max value
    calcScoreBarWidthPx :: Double -> Int -> Double -> Int
    calcScoreBarWidthPx x maxpx maxscale = abs . round $ (fromIntegral maxpx) * x / maxscale

showImageHash Nothing  = [text "No hash"]
showImageHash (Just x) = [E.span [] [text "Hash: "], E.span [class_ "image-hash-value"] [text x]]

imgWithAttrs :: Sink ImgLibraryActions -> Im.Image -> [Property] -> Html
imgWithAttrs actionsSink image attrs =
  let imgUrl   = case Im.fb_thumb_url image of
                    Nothing  -> Im.fb_image_url image
                    Just url -> Just url
  in img ([ class_ "img-thumbnail"
          , click (\_ -> actionsSink (ViewImg image))
          , src (imgOrDefault imgUrl)] ++ attrs) []
  where
    imgOrDefault Nothing = "No URL"
    imgOrDefault (Just x) = x


-- business logic

processActions :: Sink BusyCmd
               -> Sink (Maybe AppError)
               -> Behavior (Maybe [Im.Image])
               -> Behavior (Maybe Account.Account)
               -> ImgLibraryActions
               -> IO (Maybe Im.Image)
processActions busySink errorSink imsB accB (ViewPrevImg image) = do
  mbIms <- pollBehavior imsB
  let prevImg = case mbIms of
                  Nothing -> image
                  Just ims -> case Data.List.findIndex ((Im.id image ==) . Im.id) ims of
                                  Nothing -> image
                                  Just x  -> ims !! (if x - 1 < 0 then (length ims) - 1 else x - 1)
  return (Just prevImg)

processActions busySink errorSink imsB accB (ViewNextImg image) = do
  mbIms <- pollBehavior imsB
  let nextImg = case mbIms of
                  Nothing -> image
                  Just ims -> case Data.List.findIndex ((Im.id image ==) . Im.id) ims of
                                  Nothing -> image
                                  Just x  -> ims !! (if x + 1 >= length ims then 0 else x + 1)
  return (Just nextImg)

processActions busySink errorSink imsB accB x@(EnhanceImg image)  = (notImp errorSink x) >> return (Just image)
processActions busySink errorSink imsB accB (DeleteImg image) = do
  mbUsr <- pollBehavior accB
  case mbUsr of
    Nothing -> do
      errorSink . Just . BLError $ "can't delete an image: no user."
      return $ Just image

    Just acc -> do
      -- XXX TODO ask for confirmation!
      res <- (withBusy2 busySink deleteImage) acc image
      case res of
        Left e -> (errorSink $ Just e) >> (return $ Just image)
        Right Ok -> do
          {- TODO reload gallery -}
          notImp errorSink "deleted ok, but TODO reload library"
          return Nothing

processActions busySink errorSink imsB accB ViewGalleryIndex = return Nothing
processActions busySink errorSink imsB accB (ViewImg i) = return $ Just i
-- processActions busySink errorSink imsB accB x@(UploadImg _) = notImp errorSink x
processActions busySink errorSink imsB accB (UploadImg formfiles) = do
  mbUsr <- pollBehavior accB
  case mbUsr of
    Nothing -> do
      errorSink . Just . BLError $ "can't upload an image: no user."
      return Nothing

    Just acc -> do
      res <- (withBusy2 busySink uploadImages) acc formfiles
      case res of
        Left e -> (errorSink $ Just e) >> (return Nothing)
        Right imgId -> do
          {- TODO reload gallery >>= return (imgFrom imgId) -}
          notImp errorSink "uploaded ok, but TODO reload library"
          return Nothing

notImp errorSink x = do
  errorSink . Just . NotImplementedError . showJS $ x
  return Nothing

-- backend

getImages :: Account.Account -> IO (Either AppError [Im.Image])
getImages acc = Im.getAllImagesOrError (Account.username acc)

deleteImage :: Account.Account -> Im.Image -> IO (Either AppError Ok)
deleteImage acc image = Im.deleteImageOrError (Account.username acc) (Im.id image)

uploadImages :: Account.Account -> [(JSString, FormDataVal)] -> IO (Either AppError Ok)
uploadImages acc files = Im.uploadImagesOrError (Account.username acc) files

-- main entry point

imageLibraryPage :: Sink BusyCmd
                 -> Sink (Maybe AppError)
                 -> Events Account.Account
                 -> IO (Signal Html)
imageLibraryPage busySink errorSink userE = do
  (actionsSink :: Sink ImgLibraryActions, actionsE :: Events ImgLibraryActions) <- newEvent

  userB           <- stepper Nothing (fmap Just userE)                                :: IO (Behavior (Maybe Account.Account))

  galleryE        <- withErrorIO errorSink $ fmap (withBusy busySink getImages) userE :: IO (Events [Im.Image])
  galleryS        <- stepperS Nothing (fmap Just galleryE)                            :: IO (Signal (Maybe [Im.Image]))

  imageE          <- reactimateIO $ fmap (processActions busySink errorSink (current galleryS) userB) actionsE :: IO (Events (Maybe Im.Image))

  imageViewS      <- stepperS Nothing imageE                                          :: IO (Signal (Maybe Im.Image))
  let imageView   = fmap (fmap (viewImageW actionsSink)) imageViewS                   :: Signal (Maybe Html)
  let galleryView = fmap ((altW mempty galleryW) actionsSink) galleryS                :: Signal Html

  return $ layout <$> galleryView <*> imageView

  where
    layout indexView imageView = case imageView of
      Nothing -> indexView
      Just v  -> v
