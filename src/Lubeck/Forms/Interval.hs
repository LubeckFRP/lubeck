
{-# LANGUAGE OverloadedStrings #-}

module Lubeck.Forms.Interval
  ( integerIntervalWidget
  , dateIntervalWidget
  , customIntervalWidget
  ) where

import qualified Data.List
import Data.Interval (Interval, interval, Extended(..), lowerBound, upperBound)
import Data.Time.Calendar (Day(..))
import Control.Lens (over, under, set, view, review, preview, lens, Lens, Lens', Prism, Prism', Iso, Iso')
import qualified Control.Lens

import Data.JSString (JSString, pack, unpack)

import qualified Web.VirtualDom.Html as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events as Ev

import Lubeck.Forms
import Lubeck.Forms.Basic
import Lubeck.Forms.Select

data IntervalRange = Any | LessThan | GreaterThan | Between
  deriving (Show, Eq)

integerIntervalWidget :: JSString -> Widget' (Interval Int)
integerIntervalWidget = customIntervalWidget 0 hideableIntegerWidget

dateIntervalWidget :: Day -> JSString -> Widget' (Interval Day)
dateIntervalWidget dayNow = customIntervalWidget dayNow hideableDateWidget

-- |
-- Create a widget for intervals of arbitrary ordered type, based on an underlying widget.
--
-- Displayed as a menu with the alternatives "Any", "Less than", "Greater than" and "Between" followed
-- by two sub-widgets representing endpoints.
--
customIntervalWidget
  :: Ord a
  => a                      -- ^ Default value, used i.e. when switching from \"Any\" to an interval with endpoints.
  -> (Bool -> Widget' a)    -- ^ Underlying widget type. Argument is @False@ whenever the widget disabled,
                            --   i.e. because the endpoint is not in use.
  -> JSString               -- ^ Title
  -> Widget' (Interval a)
customIntervalWidget z numW title = id
    $ mapHtmlWidget (\x -> E.div [A.class_ "form-group form-inline"] $ pure $ E.label [] [E.text title, x])
    $ lmapWidget fromInterval
    $ rmapWidget toInterval
    $ spanWidget2
  where
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

    -- spanWidget2 :: Widget' (IntervalRange, (a, a))
    spanWidget2 s x = composeWidget spanTypeW (numsW $ fst x) s x

    -- spanTypeW :: Widget' IntervalRange
    spanTypeW = selectWidget
      [ (Any,         "Any")
      , (GreaterThan, "Greater than")
      , (LessThan,    "Less than")
      , (Between,     "Between")
      ]

    -- numsW :: IntervalRange -> Widget' (a, a)
    numsW infFin = composeWidget (numW (fst $ visible infFin)) (numW (snd $ visible infFin))

    visible x = case x of
      Any         -> (False, False)
      GreaterThan -> (True, False)
      LessThan    -> (False, True)
      Between     -> (True, True)

-- TODO is the (Monoid Html) instance what we need?
composeWidget :: Widget' a -> Widget' b -> Widget (a,b) (a,b)
composeWidget a b = bothWidget mappend (subWidget Control.Lens._1 a) (subWidget Control.Lens._2 b)
