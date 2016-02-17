
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import Data.Monoid ((<>))

import GHCJS.Types(JSString, jsval)
import qualified Web.VirtualDom as VD
import qualified Web.VirtualDom.Html as H
import qualified Web.VirtualDom.Html.Attributes as H
import qualified Web.VirtualDom.Html.Events as H
import qualified Web.VirtualDom.Svg.Events as SvgEv
import qualified Data.JSString

import System.Random (mkStdGen, randoms, split)



-- TODO move
import Data.Time (UTCTime(..), DiffTime, Day(..))


-- TODO Debug
import Control.Concurrent(forkIO, threadDelay)
import Control.Monad(forever)

import Lubeck.FRP
import Lubeck.Plots.Drawing
import Lubeck.Plots.SimpleNormalized
import Lubeck.App (Html, runAppReactive)
import Lubeck.Forms
import Lubeck.Forms.Select
  -- (Widget, Widget', component, bothWidget)
import Lubeck.Forms.Basic
import Lubeck.Drawing
import Lubeck.Util(showJS, parseDateAndTimeToUTC)

import Data.VectorSpace
import Data.AffineSpace
import qualified Lubeck.Drawing

import Data.Colour (withOpacity)
import qualified Data.Colour.Names as Colors


-- MAIN

testSimple1, testSimple2, testSimple3 :: Drawing
testSimple1 = snd $ simpleLinePlot showJS showJS
  id id id id 10 10
  [(0.2, 0.3), (0.4, 1), (1,1)]

testSimple2 = snd $ simpleLinePlot showJS showJS
  id id id id 10 10
  (zip [-10,1,3,4,7,15] [10,20,0,300,30,50])

testSimple3 = snd $ simpleLinePlot showJS showJS
  utcTimeToApproxReal realToApproxUTCTime id id 10 10
  (zip
    (fmap ((\(Just x) -> x) . parseDateAndTimeToUTC)
      [ "1400-01-01T06:00:00"
      , "1900-12-24T06:53:00"
      , "2016-02-03T06:53:16"
      ])
    [10,15,-1]
    )



chooseDrawing :: [Drawing] -> IO (Signal Html)
chooseDrawing ds = do
  (view, intE) <- componentEvent 0 (selectEnumWidget 0 (length ds - 1)) mempty
  drawingS <- stepperS mempty (fmap (ds !!) intE)
  return $ mconcat [view, (fmap (toSvg defaultRenderingOptions . (xyAxis <>)) drawingS)]

main :: IO ()
main = do
  dS <- chooseDrawing [testSimple1, testSimple2, testSimple3]
  runAppReactive $ dS
  where
    ps = zipWith Point rand1 rand2




-- -- Work around 'blocked indefinitely' issue by embedding a dummy handler
-- runAppReactive2 :: Signal Html -> IO ()
-- runAppReactive2 x = do
--   (s,e) <- newEvent
--   s2 <- fmap (x <>) $ stepperS mempty e
--   runAppReactive $
--     fmap
--       (\html -> VD.node "div" [VD.on "unlikelythingtohappen" $ \_ -> s undefined] [html])
--       s2

-- length 100
rand1, rand2 :: [Double]
rand1 = randoms $ fst $ split randG
rand2 = randoms $ snd $ split randG

randG = (mkStdGen 8712261455)
