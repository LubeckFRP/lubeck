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

getImages :: JSString -> IO (Either AppError [Image])
getImages path = first ApiError <$> getAPIEither testAPI "label-refiner/images/test"

render :: Html -> Html -> Html
render prompt imageGrid = E.div
  [ A.style "width: 1000px; margin-left: auto; margin-right: auto" ]
  [ prompt, imageGrid ]

imgGridW :: Widget' ImageGrid
imgGridW sink imgs =
    E.div [ A.class_ "container" ]
      [ E.div
        [ A.class_ "row" ] $
        map (imgCell imgs sink) imgs
      ]
  where
    imgCell :: ImageGrid ->  Widget (Image, Bool) ImageGrid
    imgCell imgs actionsSink imgAndState = E.div
      [ A.class_ "col-lg-3 col-md-4 col-xs-6 thumb"]
      [ E.a [ A.class_ "thumbnail"]
	    [ imgWithAttrs [] (contramapSink (`updateImage` imgs) actionsSink) imgAndState ]
      ]

imgWithAttrs :: [E.Property] -> Widget' (Image, Bool)
imgWithAttrs attrs actionSink (image, state) =
    E.img
      ([ EV.click $ \_ -> actionSink $ highlightImage (image,state)
       , A.src $ filename image] ++
         attrs ++ highlightStyle)
      []
  where
    imgOrDefault Nothing = "No URL"
    imgOrDefault (Just x) = x
    highlightStyle = [ A.style "outline: 4px solid black;" | state ]

highlightImage :: (Image, Bool) -> (Image,Bool)
highlightImage = second not

updateImage :: (Image,Bool) -> ImageGrid -> ImageGrid
updateImage img [] = []
updateImage img (curr:imgs)
  | fst img == fst curr = img : imgs
  | otherwise = curr : updateImage img imgs

promptW :: Widget' Label
promptW sink (Label id name)  =
  E.div [ A.class_ "text-center" ] [ E.text . pack $ T.unpack name ]

main = do
  -- call getRandomLabel
  randLabel <- getRandomLabel testAPI
  testImages <- getImages (pack "")
  case randLabel of
    Left (ApiError err) -> print err
    Right label -> case testImages of
      Left (ApiError err) -> print err
      Right imgs -> do
        let imgStateList = zip imgs $ repeat False
        (actionSink,_) <- newEvent :: IO (Sink ImageGrid, Events ImageGrid)
        (imgGridView, imgGridSink) <- componentW imgStateList imgGridW
        (promptView,_) <- componentR label promptW
        runAppReactive $ render <$> promptView <*> imgGridView
