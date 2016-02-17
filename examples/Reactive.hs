
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import Data.Monoid ((<>))
import Control.Monad (when)

import GHCJS.Types(JSString, jsval)
import qualified Web.VirtualDom as VD
import qualified Web.VirtualDom.Html as H
import qualified Web.VirtualDom.Html.Attributes as H
import qualified Web.VirtualDom.Html.Events as H
import qualified Web.VirtualDom.Svg.Events as SvgEv
import qualified Data.JSString

-- TODO move
import Data.Time (UTCTime(..), DiffTime, Day(..))


-- TODO Debug
import Control.Concurrent(forkIO, threadDelay, myThreadId)
import Control.Monad(forever)
import GHCJS.Concurrent(synchronously)

import Control.Lens(_1, _2)

import BD.Data.Account (getUser)

import Lubeck.FRP
import Lubeck.App (Html, runAppReactive)
import Lubeck.Forms
import Lubeck.Forms.Basic
import Lubeck.Forms.Select
import Lubeck.Drawing
import Lubeck.Util(showJS, unselectable, parseDateAndTimeToUTC, newEventOf, reactimateIOAsync)
import qualified Lubeck.Drawing

import Data.VectorSpace
import Data.AffineSpace

import Data.Colour (withOpacity)
import qualified Data.Colour.Names as Colors


-- MAIN

render :: Widget' (Int, Int)
render = multiWidget mappend
  [ subWidget _1 integerWidget
  , subWidget _2 integerWidget
  , subWidget _1 rangeW
  , subWidget _2 (selectEnumWidget (-2) 10)
  , subWidget _1 svgW
  ]

rangeW :: Widget' Int
rangeW = rangeWidget 0 200 1

svgW :: Widget' Int
svgW output model = toSvg (RenderingOptions (Point (1400) (1200)) Center) $ drawing output model

drawing :: WidgetT' Drawing Int
drawing output n = mempty
  <> circles
  <> addProperty (SvgEv.onClick $ \_ -> output (succ n)) (scale 0.8 $ redCircle $ negate n)
  <> blueRect
  <> addProperty (SvgEv.onClick $ \_ -> output (pred n)) (redCircle n)
  <> shear (fromIntegral n/200) 0 (scale 40 (Lubeck.Drawing.text "Hans"))
  <> scale 10 xyAxis
  <> scale 10 smokeBackground
  where
    nCircles = 40
    circles = mconcat $ fmap (\i -> rotate (turn/fromIntegral nCircles*fromIntegral (negate i)) $ translateX (100+fromIntegral i+fromIntegral n) ci) [1..nCircles]
      where ci = fillColorA (Colors.green `withOpacity` 0.5) $ scale 10 $ scaleX 0.5 $ square
    blueRect = fillColorA (Colors.blue `withOpacity` 0.5) $ scale 50 $ scaleX 1.2 $ square
    redCircle n = fillColor Colors.red $
      translateY (negate $ 3 * fromIntegral n) $ translateX (4 * fromIntegral n) $
      scale (50 + 4 * fromIntegral n) $
      circle

main :: IO ()
main = do

   (view, ints) <- component (1,1) render
   let b = fmap snd ints
   subscribeEvent b print
   stringE <- reactimateIOAsync $ fmap (const $ fmap showJS (getUser "jacob")) $ Lubeck.FRP.filter (==5) b
   stringS <- stepperS "(nothing yet)" stringE

   --  $ \v -> when (v == 5) $ do
    --  print "Making request"
    --  forkIO $ (getUser "jacob" >>= \u -> threadDelay 1500000 >> synchronously (stringSi . showJS $ u))
    --  print "Done!"
    --  return ()
   runAppReactive (fmap (staticStringWidget emptySink) stringS <> view)
