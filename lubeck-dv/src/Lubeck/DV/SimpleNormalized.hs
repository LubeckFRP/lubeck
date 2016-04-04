
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections, CPP #-}

-- |
-- Basic normalized visualization.
module Lubeck.DV.SimpleNormalized
  ( simpleLinePlot
  , simpleTimeSeries
  , simpleTimeSeriesWithOverlay
  , utcTimeToApproxReal
  , realToApproxUTCTime
  ) where

import Prelude hiding (div)
import qualified Prelude

import Data.Monoid ((<>))

import Data.Colour (withOpacity)
import qualified Data.Colour.Names as Colors

-- TODO move
import Data.Time (UTCTime(..), DiffTime, Day(..), diffUTCTime, addUTCTime)
import qualified Data.Time.Format

-- TODO Debug
import Control.Concurrent(forkIO, threadDelay)
import Control.Monad(forever)

import Linear.Vector
import Linear.Affine
-- import Linear.Matrix hiding (translation)
-- import Linear.Metric -- Needed?
import Linear.V0
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4

-- import Lubeck.FRP
-- import Lubeck.App (Html, runAppReactive)
-- import Lubeck.Forms
  -- (Widget, Widget', component, bothWidget)
-- import Lubeck.Forms.Basic

import Lubeck.Drawing
import qualified Lubeck.Drawing
import Lubeck.DV.Drawing(scatterData, scatterDataX, lineData, ticks, labeledAxis)
import Lubeck.DV.Styling(withDefaultStyle, Styled)


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
utcTimeToApproxReal t = realToFrac $ (t `diffUTCTime` refTime) / (1000000000000)

realToApproxUTCTime :: Double -> UTCTime
realToApproxUTCTime x = ((realToFrac x) * 1000000000000) `addUTCTime` refTime

refTime :: UTCTime
refTime = case Data.Time.Format.parseTimeM True Data.Time.Format.defaultTimeLocale
  (Data.Time.Format.iso8601DateFormat Nothing) "2000-01-01" of
    Just t -> t
    _      -> error "Should not happen"

-- unDay = toModifiedJulianDay
-- day   = ModifiedJulianDay
--

{-|
Draw a simple line plot. Steps performed:

- Find an (approximate) Iso between axis and R

- Map data into R^2 (not normalized)

- Find bounds (lower,upper)

- Generate tick positions

- Map ticks from R^2 back into original type and render as text (for tick labels)

- Create a linear map (R^2 <-> R^2) such that all data points fall into the unit square

- Normalize tick positions and data

- Generate plot

-}
simpleLinePlot
  :: (a -> Str)                  -- ^ How to print ticks on X axis.
  -> (b -> Str)                  -- ^ How to print ticks on Y axis.
  -> (a -> Double) -> (Double -> a)   -- ^ Mapping from domain(X) to R.
  -> (b -> Double) -> (Double -> b)   -- ^ Linear map from domain(Y) to R.
  -> Int                              -- ^ Number of ticks on X axis.
  -> Int                              -- ^ Number of ticks on Y axis.
  -> [(a,b)]                          -- ^ Data to plot.
  -> ((Double -> Double, Double -> Double), Styled Drawing)
simpleLinePlot _     _     _   _   _   _   _         _         [] = ((id,id), mempty)
simpleLinePlot showA showB a2d d2a b2d d2b numTicksA numTicksB xs = ((normA, normB), drawing)
  where
    drawing = mconcat
      [ lineData points
      , ticks (zip tickOffsetsA (fmap Just tickLabelsA)) (zip tickOffsetsB (fmap Just tickLabelsB))
      , labeledAxis "" ""
      -- , scale 100 smokeBackground
      ]

    tickLabelsA  = fmap (showA . d2a) ticksA
    tickLabelsB  = fmap (showB . d2b) ticksB
    tickOffsetsA = fmap normA ticksA
    tickOffsetsB = fmap normB ticksB
    nAs = fmap normA as
    nBs = fmap normB bs
    points = zipWith (\x y -> P $ V2 x y) nAs nBs

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

    -- Utility
    unzip xs = (fmap fst xs, fmap snd xs)

    normalizerFromBounds :: Fractional a => (a, a) -> (a -> a, a -> a)
    normalizerFromBounds (lb,ub) = (\x -> (x - lb)/d, \x -> x*d + lb) where d = ub - lb

    --from here http://stackoverflow.com/questions/326679/choosing-an-attractive-linear-scale-for-a-graphs-y-axis
    -- see also http://stackoverflow.com/questions/361681/algorithm-for-nice-grid-line-intervals-on-a-graph

    -- number of ticks, interval, outpouts ticks
    tickCalc :: Int -> (Double, Double) -> [Double]
    tickCalc tickCount (lo, hi) =
      let range = hi - lo :: Double
          unroundedTickSize = range/(realToFrac $ tickCount-1)        --  :: Double
          x = realToFrac (ceiling (logBase 10 (unroundedTickSize)-1)) --  :: Double
          pow10x = 10**x -- Math.pow(10, x);
          stepSize = realToFrac ((ceiling (unroundedTickSize / pow10x))::Int) * pow10x
          lb = stepSize * realToFrac (floor (lo / stepSize))
          ub = stepSize * realToFrac (ceiling (hi / stepSize))

      in [lb, lb+stepSize..ub]
      where
        exrng = (2.1, 11.5)



simpleTimeSeries :: (a -> Str) -> (a -> Double) -> (Double -> a) -> [(UTCTime, a)] -> Styled Drawing
simpleTimeSeries s f g = snd . simpleLinePlot
  (replaceStr "T" "  " . takeStr 16 . formatDateAndTimeFromUTC) s
  utcTimeToApproxReal realToApproxUTCTime
  f g
  10 10

simpleTimeSeriesWithOverlay :: (a -> Str) -> (a -> Double) -> (Double -> a) -> [UTCTime] -> [(UTCTime, a)] -> Styled Drawing
simpleTimeSeriesWithOverlay s f g times dat = plot2 <> plot1
  where
    plot2 = scatterDataX $ fmap ((\t -> P $ V2 t 0.5) . normT . utcTimeToApproxReal) times
    ((normT, _), plot1) = simpleLinePlot
      (replaceStr "T" "  " . takeStr 16 . formatDateAndTimeFromUTC) s
      utcTimeToApproxReal realToApproxUTCTime
      f g
      10 10
      dat

-- TODO consolidate

-- | Format a date written in ISO 8601 i.e. @YYYY-MM-DDTHH:MM:SS@
formatDateAndTimeFromUTC :: UTCTime -> Str
formatDateAndTimeFromUTC = packStr . Data.Time.Format.formatTime l f
  where
    l = Data.Time.Format.defaultTimeLocale
    f = Data.Time.Format.iso8601DateFormat (Just "%H:%M:%S")
