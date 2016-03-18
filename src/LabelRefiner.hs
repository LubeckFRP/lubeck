{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

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

import           BD.Api
import           BD.Types

import           Data.Bifunctor                 (bimap, first, second)
import           Data.Int

newtype ImageID = ImgID Int
  deriving (Eq, Read, Show, Data, Typeable, FromJSON)

newtype ImageSrc = ISrc Int
  deriving (Eq, Read, Show, Data, Typeable, FromJSON)

data LRImage = LRImage
  { img_id   :: ImageID
  , filename :: Text
  , img_src  :: ImageSrc
  } deriving (Eq, GHC.Generic, Show)
instance FromJSON LRImage

type ImageGrid = [(LRImage,Bool)]

getImages :: JSString -> IO (Either AppError [LRImage])
getImages path = getAPIEither testAPI "label-refiner" >>= return . first ApiError
  where testAPI = defaultAPI { baseURL = "http://localhost:3567/api/v1/" }

render :: Html -> Html
render imageGrid = E.div
  [ A.style "width: 1000px; margin-left: auto; margin-right: auto" ]
  [ imageGrid ]

imgGridW :: Widget' ImageGrid
imgGridW sink imgs = E.div [ A.class_ "container" ]
    [ E.div
      [ A.class_ "row" ] $
      map (imgCell imgs sink) imgs
    ]
  where
    imgCell :: ImageGrid ->  Widget (LRImage, Bool) ImageGrid
    imgCell imgs actionsSink imgAndState = E.div
      [ A.class_ "col-lg-3 col-md-4 col-xs-6 thumb"]
      [ E.a [ A.class_ "thumbnail"]
	    [ imgWithAttrs [] (contramapSink (`updateImage` imgs) actionsSink) imgAndState ]
      ]

imgWithAttrs :: [E.Property] -> Widget' (LRImage, Bool)
imgWithAttrs attrs actionSink (image, state) = E.img
   ([ EV.click $ \_ -> actionSink $ highlightImage (image,state)
    , A.src $ filename image
    ] ++ attrs ++ highlightStyle)
  []
  where
    imgOrDefault Nothing = "No URL"
    imgOrDefault (Just x) = x
    highlightStyle = [ A.style "outline: 4px solid black;" | state ]

highlightImage :: (LRImage, Bool) -> (LRImage,Bool)
highlightImage = second not

updateImage :: (LRImage,Bool) -> ImageGrid -> ImageGrid
updateImage img [] = []
updateImage img (curr:imgs)
  | fst img == fst curr = img : imgs
  | otherwise = curr : updateImage img imgs

main = do
  testImages <- getImages (pack "")
  case testImages of
    Left (ApiError err) -> print err
    Right imgs -> do
      let imgStateList = zip imgs $ repeat False
      (actionSink,_) <- newEvent :: IO (Sink ImageGrid, Events ImageGrid)
      (imgGridView, imgGridSink) <- componentW imgStateList imgGridW
      runAppReactive $ render <$> imgGridView
