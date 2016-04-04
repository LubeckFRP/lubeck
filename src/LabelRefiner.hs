{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import           Control.Monad
import           Data.Monoid                    (mconcat, (<>))
import           Data.Time

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
import qualified Data.Map as Map

import           BD.Api
import           BD.Types

import           Data.Bifunctor                 (bimap, first, second)
import           Data.Int

import           BD.Data.ImageLR
import           BD.Data.ImageLabel
import           BD.Data.SessionLR

type ImageStates = Map.Map Image UTCTime

data SessionState = SessionState
  { images :: [Image]
  , imageStates :: ImageStates
  , server_time :: UTCTime
  , time_rec :: UTCTime
  }

render :: Html -> Html -> Html -> Html
render prompt imageGrid submitBtn = E.div
  [ A.class_ "container"
  , A.style "width: 1000px; margin-left: auto; margin-right: auto" ]
  [ prompt, imageGrid, submitBtn ]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf 0 xs = [xs]
chunksOf n xs = front : chunksOf n back
  where
    (front,back) = splitAt n xs

imgGridW :: Widget' SessionState
imgGridW actionSink s@(SessionState imgs imgStates _ _) =
    E.div [ A.class_ "row" ] $
      map (\irow -> imgRow irow actionSink s) $ chunksOf 3 imgs
  where
    imgRow :: [Image] -> Widget' SessionState
    imgRow rowImgs actionSink state =
        E.div [ A.class_ "row" ] $
          map (imgCell actionSink) rowImgs

    imgCell :: Widget Image SessionState
    imgCell actionSink img =
        E.div [ A.class_ "col-md-4"]
          [ E.a (A.class_ "thumbnail" : clickProp : highlightProp)
              [ E.img [ A.src (img_url img) , A.class_ "img-responsive center-block" ]
                  []
              ]
          ]
      where
        clickProp = EV.click $ \_ -> do
            t <- getCurrentTime
            actionSink (toggleImgState img t s)
        highlightProp = [A.style "outline: 4px solid black;" | img `Map.member` imgStates]

toggleImgState :: Image -> UTCTime -> SessionState -> SessionState
toggleImgState img t s@(SessionState imgs imgStates serverTime recTime)
    | img `Map.member` imgStates = s { imageStates = Map.delete img imgStates }
    | otherwise = s { imageStates = Map.insert img t imgStates }

promptW :: Widget' Label
promptW sink (Label id name)  =
  E.div [ A.class_ "text-center" ]
    [ E.h2 []
      [ E.text $
          "Select the images that represent the label: " <> pack (T.unpack name)
      ]
    ]

submitBtnW :: Widget' ()
submitBtnW sink _ =
  E.div [ A.class_ "row" ]
    [ E.div [ A.class_ "col-md-2 col-md-offset-10" ]
        [ buttonWidget "Submit" sink () ]
    ]

pageToSessState :: SessionPage -> IO SessionState
pageToSessState (SessionPage timeSent _ imgs) =
  SessionState imgs Map.empty timeSent <$> getCurrentTime

main = do
  let nImgsPerPage = 9
  initSession <- initializeSession' testAPI nImgsPerPage
  let Session sid sp@(SessionPage serverTime initLabel initImgs) = initSession
  initState <- pageToSessState sp

  (submitS, submitE) <- componentR () submitBtnW
  counterS <- accumS 0 (fmap (const (+1)) submitE)

  newPageE <- reactimateIOAsync $ fmap (const (getSessionPage' testAPI nImgsPerPage)) submitE
  promptS <- componentListen promptW <$> stepperS initLabel (fmap label newPageE)

  newSessionStateE <- reactimateIOAsync $ fmap pageToSessState newPageE
  (imgGridS,_) <- componentEvent initState imgGridW newSessionStateE

  runAppReactive $ render <$> promptS <*> imgGridS <*> submitS
