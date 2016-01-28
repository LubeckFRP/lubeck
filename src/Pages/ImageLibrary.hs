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
import           Lib.Helpers


getImages :: Account.Account -> IO (Either AppError [Im.Image])
getImages acc = Im.getAllImagesOrError (Account.username acc)

imageLibraryPageW :: Widget [Im.Image] ()
imageLibraryPageW _ [] =
  contentPanel $ text "No images in library"

imageLibraryPageW _ ims =
  contentPanel $ div [] (map imageCell ims)

imageCell img =
  let imgUrl = case Im.fb_thumb_url img of
        Nothing  -> Im.fb_image_url img
        Just url -> Just url
  in div [class_ "thumbnail custom-thumbnail-1 fit-text"]
      [ div [class_ "thumbnail-wrapper"] [ imgWithAttrs imgUrl [] ]
      , p [class_ "image-prediction"] [ showImagePred $ Im.prediction img ]
      , p [class_ "image-hash"]       (showImageHash $ Im.fb_image_hash img)
      ]

showImagePred Nothing  = text "No prediction"
showImagePred (Just x) = text $ "Score: "<> showJS x

showImageHash Nothing  = [text "No hash"]
showImageHash (Just x) = [E.span [] [text "Hash: "], E.span [class_ "image-hash-value"] [text x]]

imgWithAttrs :: Maybe JSString -> [Property] -> Html
imgWithAttrs (Just url) attrs = img ([class_ "img-thumbnail", src url] ++ attrs) []
imgWithAttrs Nothing attrs    = text "No URL"

imageLibraryPage :: Signal (Maybe [Im.Image]) -> IO (Signal Html)
imageLibraryPage imagesS = do
  let imageLibView = fmap ((altW mempty imageLibraryPageW) emptySink) imagesS

  return imageLibView
