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

import           BD.Api
import           BD.Types

import           Data.Bifunctor                 (bimap, first, second)
import           Data.Int

import           BD.Data.ImageLR
import           BD.Data.ImageLabel
import           BD.Data.SessionLR 

data SImage = SImage
  { tselected :: Maybe UTCTime
  , img :: Image
  }

type Selected = [SImage]
type NotSelected = [SImage]
type Selections = (Selected,NotSelected)

data SessionState = SessionState 
  { images :: [Image]
  , selections :: Selections
  , pageRecTime :: UTCTime
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
imgGridW actionSink s@(SessionState imgs sels _) =
    E.div [ A.class_ "row" ] $
      map (\irow -> imgRow irow actionSink s) $ chunksOf 3 imgs
  where
    imgRow :: [Image] -> Widget' SessionState 
    imgRow imgs actionSink state =
        E.div [ A.class_ "row" ] $
          map (imgCell toggleSink) imgs 
      where
        toggleSink = contramapSink (`toggleImg` state) actionSink

    imgCell :: Widget SImage 
    imgCell toggleSink img =
        E.div [ A.class_ "col-md-4"]
          [ E.a (A.class_ "thumbnail" : clickProp : highlightProp)
              [ E.img [ A.src (img_url img) , A.class_ "img-responsive center-block" ] 
                  [] 
              ]
          ]
      where
        clickProp = EV.click $ \_ -> toggleSink img
        highlightProp = [A.style "outline: 4px solid black;" | img `elem` fst sels]

toggleImg :: SImage -> SessionState -> SessionState  
toggleImg img (SessionState imgs (sel,nsel) recTime) 
  | img `elem` sel = SessionState imgs (removeImg img sel, img:nsel)
  | otherwise = SessionState imgs (img:sel, removeImg img nsel) 
  where 
    removeImg :: Image -> [SImage] -> [SImage]
    removeImg _ [] = []
    removeImg img (i:imgs)
      | img == snd i = imgs
      | otherwise = i : removeImg img imgs

mapSinkWithTime :: (UTCTime -> a -> b) -> Sink b -> Sink a
mapSinkWithTime = undefined

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

main = do
  let nImgsPerPage = 9
  initSession <- initializeSession' testAPI nImgsPerPage
  let Session sid (SessionPage serverTime initLabel initImgs) = initSession 
  initState <- SessionState initImgs ([],initImgs) <$> getCurrentTime 

  (submitS, submitE) <- componentR () submitBtnW
  counterS <- accumS 0 (fmap (const (+1)) submitE)
  
  newPageE <- reactimateIOAsync $ fmap (const (getSessionPage' testAPI nImgsPerPage)) submitE
  promptS <- componentListen promptW <$> stepperS initLabel (fmap label newPageE) 

  let newSelectionsE = fmap (\page -> SessionState (imgs page) ([],imgs page)) newPageE 
  (imgGridS,_) <- componentEvent initState imgGridW newSelectionsE 
  
  runAppReactive $ render <$> promptS <*> imgGridS <*> submitS
