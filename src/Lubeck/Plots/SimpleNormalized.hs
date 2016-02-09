
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Lubeck.Plots.SimpleNormalized
    ( simpleLinePlot
    , simpleTimeSeries
    , utcTimeToApproxReal
    , realToApproxUTCTime
    ) where

import Prelude hiding (div)
import qualified Prelude

import Data.Monoid ((<>))

import GHCJS.Types(JSString, jsval)
import qualified Data.JSString
import qualified Web.VirtualDom as VD
import qualified Web.VirtualDom.Html as H
import qualified Web.VirtualDom.Html.Attributes as H
import qualified Web.VirtualDom.Html.Events as H
import qualified Web.VirtualDom.Svg.Events as SvgEv
import qualified Data.JSString
import Data.VectorSpace
import Data.AffineSpace
import Data.Colour (withOpacity)
import qualified Data.Colour.Names as Colors

-- TODO move
import Data.Time (UTCTime(..), DiffTime, Day(..))

-- TODO Debug
import Control.Concurrent(forkIO, threadDelay)
import Control.Monad(forever)

import Lubeck.FRP
import Lubeck.App (Html, runAppReactive)
import Lubeck.Forms
  -- (Widget, Widget', component, bothWidget)
import Lubeck.Forms.Basic
import Lubeck.Drawing
import Lubeck.Util(showJS, formatDateAndTimeFromUTC)
import qualified Lubeck.Drawing
import Lubeck.Plots.Drawing(scatterData, lineData, ticks, labeledAxis)



-- -- TODO to many variants of these
-- -- componentRW as currently implemented arguably does the wrong thing, by forwarding incoming events
-- -- (why wrong: this can easily be accomplished by merging with the input, while the other way around is nothing
-- -- so straightforward)
--
-- -- The most general version:
-- -- (b -> a -> a) -> (c -> a -> a) -> a -> WT r a b -> E c -> IO (S r, S a)
--
-- compo :: WidgetT r a a -> a -> Events a -> IO (Signal r, Events a)
-- compo widget z input = do
--   (view, output, inputSink) <- componentRW z widget
--   subscribeEvent input inputSink
--   return (view, output)
--
-- compo_ :: WidgetT r a a -> a -> Events a -> IO (Signal r)
-- compo_ w z i = fmap fst $ compo w z i
--
-- compoS_ :: WidgetT r a a -> Signal a -> IO (Signal r)
-- compoS_ w i = do
--   z <- pollBehavior $ current i
--   let u = updates i
--   compo_ w z u
--
--
-- -- TODO nicer conventions for event listeners
-- -- * Standarize Names
-- -- * Add more of them
-- -- * Variants without the event (i.e. for "mouseover", "mouseout" etc)
--
-- -- Basic non-interactive plots
-- circleWithMouseOver :: WidgetT Drawing Bool Bool
-- circleWithMouseOver output state =
--   addProperty (SvgEv.onMouseOver $ const $ output True) $
--   addProperty (SvgEv.onMouseOut $ const $ output False) $
--   mconcat
--     [ fillColorA ((if state then Colors.lightblue else Colors.blue) `withOpacity` 0.5) $ scale 250 circle
--     , axisY
--     , axisX
--     ]


utcTimeToApproxReal :: UTCTime -> Double
utcTimeToApproxReal (UTCTime days seconds) = (fromIntegral (unDay days) * (3600*24)) + realToFrac seconds

realToApproxUTCTime :: Double -> UTCTime
realToApproxUTCTime x = UTCTime (day dayPart) (realToFrac x - fromIntegral dayPart) where dayPart = floor $ x/(3600*24)

unDay = toModifiedJulianDay
day = ModifiedJulianDay

simpleLinePlot
  :: (a -> JSString)                  -- ^ How to print ticks on X axis.
  -> (b -> JSString)                  -- ^ How to print ticks on Y axis.
  -> (a -> Double) -> (Double -> a)   -- ^ Mapping from domain(X) to R.
  -> (b -> Double) -> (Double -> b)   -- ^ Linear map from domain(Y) to R.
  -> Int                              -- ^ Number of ticks on X axis.
  -> Int                              -- ^ Number of ticks on Y axis.
  -> [(a,b)]                          -- ^ Data to plot.
  -> ((Double -> Double, Double -> Double), Drawing)
simpleLinePlot _     _     _   _   _   _   _         _         [] = ((id,id), mempty)
simpleLinePlot showA showB a2d d2a b2d d2b numTicksA numTicksB xs = ((normA, normB), drawing)
  where
    drawing = mconcat
      [ lineData points
      , ticks (zip tickOffsetsA tickLabelsA) (zip tickOffsetsB tickLabelsB)
      , labeledAxis "" ""
      , scale 100 smokeBackground
      ]

    tickLabelsA  = fmap (showA . d2a) ticksA
    tickLabelsB  = fmap (showB . d2b) ticksB
    tickOffsetsA = fmap normA ticksA
    tickOffsetsB = fmap normB ticksB
    nAs = fmap normA as
    nBs = fmap normB bs
    points = zipWith Point nAs nBs

    -- ticksA, ticksB :: [Double]
    ticksA = tickCalc numTicksA (lba,uba)
    ticksB = tickCalc numTicksB (lbb,ubb)

    -- normA... :: Double -> Double
    (normA, _) = normalizerFromBounds (lba, uba)
    (normB, _) = normalizerFromBounds (lbb, ubb)

    -- lba... :: Double
    (lba,uba) = (minimum as, maximum as)
    (lbb,ubb) = (minimum bs, maximum bs)

    -- as, bs :: [Double]
    as = fmap a2d as'
    bs = fmap b2d bs'
    -- as' :: [a], bs' :: [b]
    (as', bs') = unzip xs

    unzip xs = (fmap fst xs, fmap snd xs)

simpleTimeSeries :: (a -> JSString) -> (a -> Double) -> (Double -> a) -> [(UTCTime, a)] -> Drawing
simpleTimeSeries s f g = snd $ simpleLinePlot
  (Data.JSString.replace "T" "  " . Data.JSString.take 16 . formatDateAndTimeFromUTC) s
  utcTimeToApproxReal realToApproxUTCTime
  f g
  10 10

normalizerFromBounds :: Fractional a => (a, a) -> (a -> a, a -> a)
normalizerFromBounds (lb,ub) = (\x -> (x - lb)/d, \x -> x*d + lb) where d = ub - lb

--from here http://stackoverflow.com/questions/326679/choosing-an-attractive-linear-scale-for-a-graphs-y-axis
-- see also http://stackoverflow.com/questions/361681/algorithm-for-nice-grid-line-intervals-on-a-graph

-- number of ticks, interval, outpouts ticks
tickCalc :: Int -> (Double, Double) -> [Double]
tickCalc tickCount (lo, hi) =
  let range = hi - lo :: Double
      unroundedTickSize = range/(realToFrac $ tickCount-1) :: Double
      x = realToFrac (ceiling (logBase 10 (unroundedTickSize)-1)) :: Double
      pow10x = 10**x -- Math.pow(10, x);
      stepSize = realToFrac ((ceiling (unroundedTickSize / pow10x))::Int) * pow10x
      lb = stepSize * realToFrac (floor (lo / stepSize))
      ub = stepSize * realToFrac (ceiling (hi / stepSize))

  in [lb, lb+stepSize..ub]
  where
    exrng = (2.1, 11.5)

-- Find an approximate Iso between each dimension (i.e. UTCTime, Int, Double) and Double
-- Convert data to double
-- Find bounds (lower,upper)
-- Decide number of ticks
-- Get tick positions
  -- Convert ticks back to original type
-- Decide a normalization (trivially from bounds), i.e. a funtion to fit data into [0..1]
-- Run data and tick positions from normalization (retain original tick position for labels)
-- Generate plots and ticks
