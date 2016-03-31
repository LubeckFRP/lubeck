{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import           Control.Monad
import           Data.Monoid                    (mconcat, (<>))

import           Data.JSString                  (JSString, pack, unpack)
import qualified GHC.Generics                   as GHC
import           GHCJS.Types                    (jsval)

import qualified Web.VirtualDom                 as V
import qualified Web.VirtualDom.Html            as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events     as EV

import           Lubeck.App                     (Html, runAppReactive,
                                                 runAppStatic)
import           Lubeck.Forms
import           Lubeck.Forms.Basic
import           Lubeck.Forms.Button
import           Lubeck.FRP
import           Lubeck.Util                    (reactimateIOAsync)

import           Data.Data                      (Data)
import           Data.Typeable                  (Typeable)

import           Data.Aeson
import qualified Data.Text as T

import           BD.Api
import           BD.Types

import           Data.Bifunctor                 (bimap, first, second)
import           Data.Int

import           BD.Data.ImageLR
import           BD.Data.ImageLabel

type ImageGrid = [(Image,Bool)]

render :: Html -> Html -> Html -> Html
render prompt imageGrid submitBtn = E.div
  [ A.class_ "container"
  , A.style "width: 1000px; margin-left: auto; margin-right: auto" ]
  [ prompt, imageGrid, submitBtn ]

chunksOf :: Int -> ImageGrid -> [ImageGrid]
chunksOf _ [] = []
chunksOf 0 xs = [xs]
chunksOf n xs = front : chunksOf n back
  where
    (front,back) = splitAt n xs

imgGridW :: Widget' ImageGrid
imgGridW actionSink imgs =
    E.div
      [ A.class_ "row" ] $
      map (imgRow imgs actionSink) $ chunksOf 3 imgs
  where
    imgRow :: ImageGrid -> Widget' ImageGrid
    imgRow allImgs actionSink imgs =
        E.div
          [ A.class_ "row" ] $
          map (imgCell allImgs actionSink) imgs

    imgCell :: ImageGrid ->  Widget (Image, Bool) ImageGrid
    imgCell imgs actionSink (image, state) =
        E.div
          [ A.class_ "col-md-4"]
          [ E.a
              (A.class_ "thumbnail" : clickProp : highlightProp)
              [ E.img [ A.src (img_url image) , A.class_ "img-responsive center-block" ] [] ]
          ]
      where
        toggleSink = contramapSink (`updateImage` imgs) actionSink
        clickProp = EV.click $ \_ -> toggleSink $ highlightImage (image,state)
        highlightProp = [ A.style "outline: 4px solid black;" | state ]

highlightImage :: (Image, Bool) -> (Image,Bool)
highlightImage = second not

squareClass :: E.Property
squareClass = A.style $
  "float:left; position: relative; width: 30%; padding-bottom : 30%;" <>
  "margin:1.66%; background-position:center center;" <>
  "background-repeat:no-repeat; background-size:cover;"

updateImage :: (Image,Bool) -> ImageGrid -> ImageGrid
updateImage img [] = []
updateImage img (curr:imgs)
  | fst img == fst curr = img : imgs
  | otherwise = curr : updateImage img imgs

promptW :: Widget' Label
promptW sink (Label id name)  =
  E.div
    [ A.class_ "text-center" ]
    [ E.h2
      []
      [ E.text $
          "Select the images that represent the label: " <>
          pack (T.unpack name)
      ]
    ]

submitBtnW :: Widget' ()
submitBtnW sink _ =
  E.div
    [ A.class_ "row" ]
    [ E.div
        [ A.class_ "col-md-2 col-md-offset-10" ]
        [ buttonWidget "Submit" sink () ]
    ]

-- Maybe make a function to return a label AND associated images
main = do
  randLabel <- getRandomLabel testAPI
  case randLabel of
    Left (ApiError err) -> print err
    Right label -> do
      images <- getNimagesWithLabel testAPI 9 label
      case images of
        Left (ApiError err) -> print err
        Right imgs -> do
          let imgStateList = zip imgs $ repeat False

          (submitS, submitE) <- componentR () submitBtnW

          promptE <- reactimateIOAsync $ fmap (const (getRandomLabel' testAPI)) submitE
          promptS <- componentListen promptW <$> stepperS label promptE

          getImagesE <- reactimateIOAsync $ fmap (getNimagesWithLabel' testAPI 9) promptE
          let imgsAndStateE = fmap (\x -> zip x $ repeat False) getImagesE
          (imgGridS,_) <- componentEvent imgStateList imgGridW imgsAndStateE

          runAppReactive $ render <$> promptS <*> imgGridS <*> submitS
