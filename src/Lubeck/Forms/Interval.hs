
{-# LANGUAGE OverloadedStrings #-}

module Lubeck.Forms.Interval
  ( integerIntervalWidget
  , dateIntervalWidget
  , customIntervalWidget
  -- TODO move
  , hideableDateWidget
  , hideableIntegerWidget
  ) where

import qualified Data.List
-- import Numeric.Interval (Interval)
-- import qualified Numeric.Interval as I
import Data.Interval (Interval, interval, Extended(..), lowerBound, upperBound)
import Data.Time.Calendar (Day(..))
import Control.Lens (over, under, set, view, review, preview, lens, Lens, Lens', Prism, Prism', Iso, Iso')
import qualified Control.Lens

import Data.JSString (JSString, pack, unpack)

import qualified Web.VirtualDom.Html as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events as Ev

import Lubeck.Forms
import Lubeck.Forms.Select
import Lubeck.Util()
import BD.Query.PostQuery(formatDateUTC, parseDateUTC) -- TODO move these

data IntervalRange = Any | LessThan | GreaterThan | Between deriving (Show, Eq)

-- data EndPoint a =  NegInf | Fin a | PosInf
  -- deriving (Eq, Ord, Read, Show)

integerIntervalWidget :: JSString -> Widget' (Interval Int)
integerIntervalWidget = customIntervalWidget 0 hideableIntegerWidget

dateIntervalWidget :: Day -> JSString -> Widget' (Interval Day)
dateIntervalWidget dayNow = customIntervalWidget dayNow hideableDateWidget

hideableIntegerWidget :: Bool -> Widget' Int
hideableIntegerWidget False _ _ = mempty
hideableIntegerWidget True sink val = E.input
  [ A.class_ "form-control"
  , A.type_ "number"
  -- , A.style_ $ if enabled then "" else "visibility:hidden"
  , Ev.change $ \e -> do
      case Ev.value e of
        "" -> do
          print "Empty value for Int, ignoring."
          return ()
        s -> sink $ read $ unpack $ Ev.value e

  , A.value (pack $ show val)
  ]
  []

hideableDateWidget :: Bool -> Widget' Day
hideableDateWidget False _ _ = mempty
hideableDateWidget True sink val = E.input
  [ A.class_ "form-control"
  , A.type_ "date"
  , Ev.change $ \e -> maybeSink sink $ parseDateUTC $ unpack $ Ev.value e
  , A.value (pack $ showDate val)
  ]
  []
  where
    maybeSink sink val = case val of
      Just x -> sink x
      -- <input>'s value can be set to ""
      -- we probably should not ignore this, but pass upstream and cancel given date boundary?
      Nothing -> return ()

    showDate = formatDateUTC


customIntervalWidget :: Ord a => a -> (Bool -> Widget' a) -> JSString -> Widget' (Interval a)
customIntervalWidget z numW title = id
    $ mapHtmlWidget (\x -> E.div [A.class_ "form-group form-inline"] $ pure $ E.label [] [E.text title, x])
    $ lmapWidget fromInterval
    $ rmapWidget toInterval
    $ spanWidget2
  where
    -- fromInterval :: Ord a => Interval (Maybe a) -> (String, (a, a))
    -- toInterval   :: Ord a => (String, (a, a))   -> Interval (Maybe a)
    -- fromInterval i
    --   | I.null i = (Any, (z,z))
    --   | otherwise = case (I.inf i, I.sup i) of
    --     (Just x,  Nothing) -> (GreaterThan, (x,x))
    --     (Nothing, Just y)  -> (LessThan,    (y,y))
    --     (Just x,  Just y)  -> (Between,     (x,y))
    --     _                  -> (Any,         (z,z))
    fromInterval i = case (lowerBound i, upperBound i) of
      (NegInf,    PosInf)     -> (Any,          (z,z))
      (NegInf,    Finite b)   -> (LessThan,     (b,b))
      (Finite a,  PosInf)     -> (GreaterThan,  (a,a))
      (Finite a,  Finite b)   -> (Between,      (a, b))
      _                       -> (Any,          (z,z)) -- empty


    toInterval x = case x of
      (Any,         (_,_)) -> interval (NegInf,True) (PosInf,True)
      (GreaterThan, (x,_)) -> interval (Finite x,True) (PosInf,True)
      (LessThan,    (_,y)) -> interval (NegInf,True) (Finite y,True)
      -- If x > y, we don't want to generate empty, so increase upper bound to match lower bound
      (Between,     (x,y)) ->
        if x > y
            then interval (Finite x,True) (Finite x,True)
            else interval (Finite x,True) (Finite y,True)

    -- toInterval x = case x of
    --   (Any,         (_,_)) -> Nothing I.... Nothing
    --   (GreaterThan, (x,_)) -> Just x  I.... Nothing -- nothing sorts as smaller than Just, hence this becomes empty
    --   (LessThan,    (_,y)) -> Nothing I.... Just y
    --   (Between,     (x,y)) -> Just x  I.... Just y

    -- spanWidget2 :: Widget' (IntervalRange, (a, a))
    spanWidget2 s x = composeWidget spanTypeW (numsW $ fst x) s x

    -- spanTypeW :: Widget' IntervalRange
    spanTypeW = selectWidget
      [ (Any,         "Any")
      , (GreaterThan, "Greater than")
      , (LessThan,    "Less than")
      , (Between,     "Between")
      ]
    visible x = case x of
      Any         -> (False, False)
      GreaterThan -> (True, False)
      LessThan    -> (False, True)
      Between     -> (True, True)

    -- numsW :: IntervalRange -> Widget' (a, a)
    numsW infFin = composeWidget (numW (fst $ visible infFin)) (numW (snd $ visible infFin))

-- TODO is the (Monoid Html) instance what we need?
composeWidget :: Widget' a -> Widget' b -> Widget (a,b) (a,b)
composeWidget a b = bothWidget mappend (subWidget Control.Lens._1 a) (subWidget Control.Lens._2 b)
