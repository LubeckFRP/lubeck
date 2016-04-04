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
import           Lubeck.Util                    (reactimateIOAsync, withErrorIO)

import           Components.BusyIndicator       (withBusy, busyIndicatorComponent)
import           Components.Notifications       (notificationsComponent)

import           Data.Data                      (Data)
import           Data.Typeable                  (Typeable)

import           Data.Aeson
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.List
import           Data.Ord                       (comparing)

import           BD.Api
import           BD.Types

import           Data.Bifunctor                 (bimap, first, second)
import           Data.Int

import qualified BD.Data.ImageLR as I
import           BD.Data.ImageLabel as IL
import           BD.Data.SessionLR

type ImageStates = Map.Map I.Image UTCTime

data SessionState = SessionState
  { images :: [I.Image]
  , imageStates :: ImageStates
  , sessionPage :: Int
  , sessionLabel :: Label
  , serverTime :: UTCTime
  , clientTime :: UTCTime
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
imgGridW actionSink s@(SessionState imgs imgStates _ _ _ _) =
    E.div [ A.class_ "row" ] $
      map (\irow -> imgRow irow actionSink s) $ chunksOf 3 imgs
  where
    imgRow :: [I.Image] -> Widget' SessionState
    imgRow rowImgs actionSink state =
        E.div [ A.class_ "row" ] $
          map (imgCell actionSink) rowImgs

    imgCell :: Widget I.Image SessionState
    imgCell actionSink img =
        E.div [ A.class_ "col-md-4"]
          [ E.a (A.class_ "thumbnail" : clickProp : highlightProp)
              [ E.img [ A.src (I.img_url img) , A.class_ "img-responsive center-block" ]
                  []
              ]
          ]
      where
        clickProp = EV.click $ \_ -> do
            t <- getCurrentTime
            actionSink (toggleImgState img t s)
        highlightProp = [A.style "outline: 4px solid black;" | img `Map.member` imgStates]

toggleImgState :: I.Image -> UTCTime -> SessionState -> SessionState
toggleImgState img t s@(SessionState imgs imgStates _ _ _ _)
    | img `Map.member` imgStates = s { imageStates = Map.delete img imgStates }
    | otherwise = s { imageStates = Map.insert img t imgStates }

promptW :: Widget' IL.Label
promptW sink (Label id name)  =
  E.div [ A.class_ "text-center" ]
    [ E.h2 []
        [ E.text "Select the images that are NOT described with the word/s: "]
    , E.h2 [A.style "font-weight:bold;"] 
        [ E.text $ pack $ T.unpack name ]
    ]

submitBtnW :: Widget' ()
submitBtnW sink _ =
  E.div [ A.class_ "row" ]
    [ E.div [ A.class_ "col-md-2 col-md-offset-10" ]
        [ buttonWidget "Submit" sink () ]
    ]

pageToSessState :: Signal Int -> SessionPage -> IO SessionState
pageToSessState counterS (SessionPage timeSent label imgs) = do
  pageNum <- pollBehavior (current counterS)
  SessionState imgs Map.empty pageNum label timeSent <$> getCurrentTime

makeSessionImages :: Int -> SessionState -> [SessionImage]
makeSessionImages sid (SessionState imgs imgStates sessPage label stime ctime) = 
    map makeSessionImage imgs
  where
    sessionImg = SessionImage sid sessPage (IL.id label)
    selectedAsList = sortBy (comparing snd) $ Map.toList imgStates  

    calcTimeDelta :: UTCTime -> UTCTime -> UTCTime -> UTCTime
    calcTimeDelta sTime cTime tClicked =
      addUTCTime (diffUTCTime tClicked cTime) sTime  

    makeSessionImage :: I.Image -> SessionImage
    makeSessionImage img =
      case lookup img selectedAsList of
        Nothing -> sessionImg (I.id img) Nothing Nothing
        Just t -> let Just idx = elemIndex (img,t) selectedAsList
                  in sessionImg (I.id img) (Just idx) (Just t)  

sendSessionImages :: Int -> Behavior SessionState -> IO (Either AppError Ok)
sendSessionImages sid sessionStateB = do 
  state <- pollBehavior sessionStateB
  postSessionData testAPI $ SessionData $ makeSessionImages sid state 

main = do
  let nImgsPerPage = 9

  (busyView, busySink) <- busyIndicatorComponent []
  (notifView, notifSink, _) <- notificationsComponent []  

  -- | Initialize app
  initSession <- initializeSession' testAPI nImgsPerPage
  let Session sid sp@(SessionPage serverTime initLabel initImgs) = initSession
  (submitS, submitE) <- componentR () submitBtnW
  counterS <- accumS 0 (fmap (const (+1)) submitE)
  initState <- pageToSessState counterS sp
  
  -- | Get data from server and display as SessionState
  newPageE <- withErrorIO notifSink $ 
    withBusy busySink (const $ getSessionPage testAPI nImgsPerPage) <$> submitE
  newSessionStateE <- reactimateIOAsync $ fmap (pageToSessState counterS) newPageE
  promptS <- componentListen promptW <$> stepperS initLabel (fmap label newPageE)
  
  (imgGridS,imgGridE) <- componentEvent initState imgGridW newSessionStateE
  sessionStateB <- stepper initState imgGridE 

  -- | on submit, send session data to server
  withErrorIO notifSink $
    withBusy busySink (const $ sendSessionImages sid sessionStateB) <$> submitE  

  runAppReactive $ render <$> promptS <*> imgGridS <*> submitS
