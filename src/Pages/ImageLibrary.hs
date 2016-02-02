{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Pages.ImageLibrary
  ( imageLibraryPage
  , getImages
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

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
import           Lubeck.FRP

import qualified BD.Data.Account                as Account
import qualified BD.Data.Image                  as Im


import           BD.Types
import           BD.Utils
import           Lubeck.Util
import           Components.BusyIndicator (BusyCmd(..), withBusy)

type ImgIndex = Int
type ImgHash = Text

data ImgLibraryActions = ViewPrevImg Im.Image | ViewNextImg Im.Image | ViewGalleryIndex
                       | DeleteImg Im.Image | EnhanceImg Im.Image
                       | UploadImg
                       | ViewImg Im.Image

instance Show ImgLibraryActions where
  show (ViewPrevImg i)  = "ViewPrevImg " <> show (Im.id i)
  show (ViewNextImg i)  = "ViewNextImg " <> show (Im.id i)
  show ViewGalleryIndex = "ViewGalleryIndex"
  show (DeleteImg i)    = "DeleteImg "   <> show (Im.id i)
  show (EnhanceImg i)   = "EnhanceImg "  <> show (Im.id i)
  show UploadImg        = "UploadImg"
  show (ViewImg i)      = "ViewImg "     <> show (Im.id i)

getImages :: Account.Account -> IO (Either AppError [Im.Image])
getImages acc = Im.getAllImagesOrError (Account.username acc)

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
              , button [class_ "btn btn-danger", click $ \_ -> sink $ DeleteImg image]   [ text "Delete"] ]
          ]

      , div [class_ "x-media"] [ E.img [src imgUrl, class_ "library-image-view-img"] [] ]
      ]

  where
    imgUrl = fromMaybe "no url" (Im.fb_image_url image)

galleryW :: Widget [Im.Image] ImgLibraryActions
galleryW _ [] =
  contentPanel $ text "No images in library"

galleryW actionsSink ims =
  contentPanel $ div []
    ([ div [class_ "btn-toolbar"] [ button [ class_ "btn btn-default"
                                           , click (\_ -> actionsSink UploadImg) ]
                                           [ text "Upload" ] ] ]
    <> (map (imageCell actionsSink) ims))

imageCell actionsSink image =
  div [class_ "thumbnail custom-thumbnail-1 fit-text"]
      [ div [class_ "thumbnail-wrapper"] [ imgWithAttrs actionsSink image [] ]
      , p [class_ "image-prediction"] [ showImagePred $ Im.prediction image ]
      , p [class_ "image-hash"]       (showImageHash $ Im.fb_image_hash image)
      ]

showImagePred Nothing  = text "No prediction"
showImagePred (Just x) = renderScore x

renderScore :: Double -> Html
renderScore x =
  div [class_ "score-container", A.title $ "Score: " <> showJS x]
    [ div [class_ "neg-score"] [ (if x < 0 then negativeScore x else mempty) ]
    , div [class_ "pos-score"] [ (if x >= 0 then positiveScore x else mempty) ]
    ]

  where
    positiveScore x = div [ class_ "good-score-bar"
                          , A.style $ "width: " <> showJS (calcScoreBarWidthPx x 69 0.2) <> "px"
                          ] []
    negativeScore x = div [ class_ "bad-score-bar"
                          , A.style $ "width: " <> showJS (calcScoreBarWidthPx x 69 0.2) <> "px"
                          ] []

    -- current value, max width in px, max value
    calcScoreBarWidthPx :: Double -> Int -> Double -> Int
    calcScoreBarWidthPx x maxpx maxscale = abs . round $ (fromIntegral maxpx) * x / maxscale

showImageHash Nothing  = [text "No hash"]
showImageHash (Just x) = [E.span [] [text "Hash: "], E.span [class_ "image-hash-value"] [text x]]

imgWithAttrs :: Sink ImgLibraryActions -> Im.Image -> [Property] -> Html
imgWithAttrs actionsSink image attrs =
  let imgUrl = case Im.fb_thumb_url image of
        Nothing  -> Im.fb_image_url image
        Just url -> Just url
  in img ([ class_ "img-thumbnail"
          , click (\_ -> actionsSink (ViewImg image))
          , src (imgOrDefault imgUrl)] ++ attrs) []
  where
    imgOrDefault Nothing = "No URL"
    imgOrDefault (Just x) = x

processActions :: Sink BusyCmd
               -> Sink (Maybe AppError)
              --  -> Behavior (Maybe [Im.Image])
               -> ImgLibraryActions
               -> IO (Maybe Im.Image)
processActions busySink errorSink x@(ViewPrevImg image) = (notImp errorSink x) >> return (Just image)
-- processActions busySink errorSink ims (ViewPrevImg img) = Just findPrev
  -- where
    -- findPrev = if idx - 1 < 0 then maxIdx else idx - 1
    -- [(_, idx)] = filter (\(img, idx) -> img == image ) (zip ims' [0..])
    -- maxIdx = length ims' - 1
    -- ims' = pollBehavior ims
processActions busySink errorSink x@(ViewNextImg image) = (notImp errorSink x) >> return (Just image)
processActions busySink errorSink x@(EnhanceImg image)  = (notImp errorSink x) >> return (Just image)
processActions busySink errorSink x@(DeleteImg image)   = (notImp errorSink x) >> return (Just image)
-- processActions busySink errorSink (DeleteImg hash) = reactimate $ do
--   busySink PushBusy
--   res <- Im.deleteByHash hash
--   errorSink res
--   busySink PopBusy
processActions busySink errorSink ViewGalleryIndex = return Nothing
processActions busySink errorSink (ViewImg i) = return $ Just i
processActions busySink errorSink x@UploadImg = notImp errorSink x
-- processActions busySink errorSink UploadImg = reactimate $ do
--   -- forkIO?
--   form <- showForm
--   busySink PushBusy
--   res <- Im.uploadImg form.img
--   busySink PopBusy
--   case res of
--     Left e -> errorSink $ "Upload failed: " <> showJS e
--     Right x -> reloadLibrary
--   return Nothing

notImp errorSink x = do
  errorSink . Just . NotImplementedError . showJS $ x
  return Nothing

imageLibraryPage :: Sink BusyCmd
                 -> Sink (Maybe AppError)
                 -> Events Account.Account
                 -> IO (Signal Html)
imageLibraryPage busySink errorSink userE = do
  (actionsSink :: Sink ImgLibraryActions, actionsE :: Events ImgLibraryActions) <- newEvent


  galleryE        <- withErrorIO errorSink $ fmap (withBusy busySink getImages) userE :: IO (Events [Im.Image])
  galleryS        <- stepperS Nothing (fmap Just galleryE)                            :: IO (Signal (Maybe [Im.Image]))

  imageE          <- reactimateIO $ fmap (processActions busySink errorSink) actionsE :: IO (Events (Maybe Im.Image))

  imageViewS      <- stepperS Nothing imageE                                      :: IO (Signal (Maybe Im.Image))
  let imageView   = fmap (fmap (viewImageW actionsSink)) imageViewS               :: Signal (Maybe Html)
  let galleryView = fmap ((altW mempty galleryW) actionsSink) galleryS            :: Signal Html

  return $ layout <$> galleryView <*> imageView

  where
    layout indexView imageView = case imageView of
      Nothing -> indexView
      Just v  -> v
