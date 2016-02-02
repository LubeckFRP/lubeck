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

data ImgLibraryActions = ViewPrevImg Im.Image | ViewNextImg Im.Image
                       | DeleteImg (Maybe ImgHash) | ViewGalleryIndex | EnhanceImg (Maybe ImgHash)
                       | UploadImg | ViewImg Im.Image

getImages :: Account.Account -> IO (Either AppError [Im.Image])
getImages acc = Im.getAllImagesOrError (Account.username acc)

viewImageW :: Widget Im.Image ImgLibraryActions
viewImageW sink image = do
  contentPanel $
    div [class_ "library-image-view"]
      [ E.img [src imgUrl, class_ "library-image-view-img"] []
      , button [click $ \_ -> sink $ ViewPrevImg image] [text "<-"]
      , button [click $ \_ -> sink $ ViewNextImg image] [text "->"]
      , button [click $ \_ -> sink $ ViewGalleryIndex] [text "x"]
      , button [click $ \_ -> sink $ EnhanceImg (Im.fb_image_hash image)] [text "Enhance"]
      , button [click $ \_ -> sink $ DeleteImg (Im.fb_image_hash image)] [text "Delete"]
      ]

  where
    imgUrl = fromMaybe "no url" (Im.fb_image_url image)

galleryW :: Widget [Im.Image] ImgLibraryActions
galleryW _ [] =
  contentPanel $ text "No images in library"

galleryW actionsSink ims =
  contentPanel $ div []
    ([ div [class_ "toolbar"] [ button [ click (\_ -> actionsSink UploadImg) ] [ text "Upload" ] ] ]
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
               -> Maybe Im.Image
-- processActions busySink errorSink ims (ViewPrevImg img) = Just findPrev
  -- where
    -- findPrev = if idx - 1 < 0 then maxIdx else idx - 1
    -- [(_, idx)] = filter (\(img, idx) -> img == image ) (zip ims' [0..])
    -- maxIdx = length ims' - 1
    -- ims' = pollBehavior ims
-- processActions busySink errorSink ims (ViewNextImg img) = Just $ getImg img
-- processActions busySink errorSink ims (DeleteImg hash) = reactimate $ do
--   busySink PushBusy
--   res <- Im.deleteByHash hash
--   errorSink res
--   busySink PopBusy
processActions busySink errorSink ViewGalleryIndex = Nothing
processActions busySink errorSink (ViewImg i) = Just i
-- processActions busySink errorSink ims UploadImg = reactimate $ do
--   -- forkIO?
--   form <- showForm
--   busySink PushBusy
--   res <- Im.uploadImg form.img
--   busySink PopBusy
--   case res of
--     Left e -> errorSink $ "Upload failed: " <> showJS e
--     Right x -> reloadLibrary
--   return Nothing

imageLibraryPage :: Sink BusyCmd
                 -> Sink (Maybe AppError)
                 -> Events Account.Account
                 -> IO (Signal Html)
imageLibraryPage busySink errorSink userE = do
  (actionsSink :: Sink ImgLibraryActions, actionsE :: Events ImgLibraryActions) <- newEvent


  galleryE        <- withErrorIO errorSink $ fmap (withBusy busySink getImages) userE :: IO (Events [Im.Image])
  galleryS        <- stepperS Nothing (fmap Just galleryE)                            :: IO (Signal (Maybe [Im.Image]))

  let imageE      = fmap (processActions busySink errorSink) actionsE             :: Events (Maybe Im.Image)

  imageViewS      <- stepperS Nothing imageE                                      :: IO (Signal (Maybe Im.Image))
  let imageView   = fmap (fmap (viewImageW actionsSink)) imageViewS               :: Signal (Maybe Html)
  let galleryView = fmap ((altW mempty galleryW) actionsSink) galleryS            :: Signal Html

  return $ layout <$> galleryView <*> imageView

  where
    layout indexView imageView = case imageView of
      Nothing -> indexView
      Just v  -> v
