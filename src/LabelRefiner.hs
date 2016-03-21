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

import           Data.JSString                  (JSString, pack)
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

import           Data.Aeson
import           Data.Text                      (unpack)

import           BD.Api
import           BD.Types

import           Data.Bifunctor                 (bimap, first, second)
import           Data.Int

import           BD.Data.ImageLR
import           BD.Data.ImageLabel

type ImageGrid = [(Image,Bool)]

getImages :: API -> JSString -> IO (Either AppError [Image])
getImages api path = getAPIEither api "label-refiner" >>= return . first ApiError

render :: Html -> Html -> Html
render promptV imageGridV = E.div
  [ A.style "width: 1000px; margin-left: auto; margin-right: auto" ]
  [ promptV, imageGridV ]

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
       , A.src . pack . unpack $ filename image] ++
         attrs ++ highlightStyle)
      []
  where
    highlightStyle = [ A.style "outline: 4px solid black;" | state ]

highlightImage :: (Image, Bool) -> (Image,Bool)
highlightImage = second not

updateImage :: (Image,Bool) -> ImageGrid -> ImageGrid
updateImage img [] = []
updateImage img (curr:imgs)
  | fst img == fst curr = img : imgs
  | otherwise = curr : updateImage img imgs

promptW :: Widget' Label
promptW sink label =
    E.div [ A.class_ "row" ]
      [ E.div [ A.class_ "col-xs-6" ]
          [ E.text prompt ]
      ]
  where
    jslabel = pack . unpack $ name label
    prompt = "This is label: " <> jslabel

main = do
  -- call getRandomLabel
  testImages <- getImages testAPI (pack "")
  randLabel <- getRandomLabel testAPI
  case testImages of
    Left (ApiError err) -> print err
    Right imgs -> case randLabel of
      Left (ApiError err) -> print err
      Right randlabel -> do
        let imgStateList = zip imgs $ repeat False
        -- (actionSink,_) <- newEvent :: IO (Sink ImageGrid, Events ImageGrid)
        (imgGridView, imgGridSink) <- componentW imgStateList imgGridW
        (promptView, _) <- component randlabel promptW
        runAppReactive $ render <$> promptView <*> imgGridView
