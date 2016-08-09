
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lubeck.Forms.Interval
  ( integerIntervalWidget
  , dateIntervalWidget
  , customIntervalWidget
  , dateIntervalWidgetNoLabel
  , IntervalSpec(..)
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

data IntervalSpec = Varying | Bounded

-- | A widget for selecting an integer range.
integerIntervalWidget :: JSString -> IntervalSpec -> Widget' (Interval Int)
integerIntervalWidget = customIntervalWidget 0 hideableIntegerWidget

-- | A widget for selecting a date range.
dateIntervalWidget :: Day -> JSString -> IntervalSpec -> Widget' (Interval Day)
dateIntervalWidget dayNow = customIntervalWidget dayNow hideableDateWidget

-- | A widget for selecting a date range, with no label
dateIntervalWidgetNoLabel :: Day -> IntervalSpec -> Widget' (Interval Day)
dateIntervalWidgetNoLabel dayNow = customIntervalWidgetNoLabel dayNow hideableDateWidget

customIntervalWidgetNoLabel
  :: forall a. Ord a
  => a                      -- ^ Default value, used i.e. when switching from \"Any\" to an interval with endpoints.
  -> (Bool -> Widget' a)    -- ^ Underlying widget type. Argument is @False@ whenever the widget disabled,
                            --   i.e. because the endpoint is not in use.
  -> IntervalSpec           -- ^ Choose whether IntervalRange is static or malleable
  -> Widget' (Interval a)
customIntervalWidgetNoLabel z numW spec =
  case spec of
      Bounded -> dimapWidget (snd . fromInterval z) toIntervalBetween $ numsW numW Between
      Varying -> dimapWidget (fromInterval z) toInterval (spanWidget2 numW)


-- |
-- Create a widget for intervals of arbitrary ordered type, based on an underlying widget.
--
-- Displayed as a menu with the alternatives "Any", "Less than", "Greater than" and "Between" followed
-- by two sub-widgets representing endpoints.
--
customIntervalWidget
  :: forall a. Ord a
  => a                      -- ^ Default value, used i.e. when switching from \"Any\" to an interval with endpoints.
  -> (Bool -> Widget' a)    -- ^ Underlying widget type. Argument is @False@ whenever the widget disabled,
                            --   i.e. because the endpoint is not in use.
  -> JSString               -- ^ Title
  -> IntervalSpec           -- ^ Choose whether IntervalRange is static or malleable
  -> Widget' (Interval a)
customIntervalWidget z numW title spec =
  mapHtmlWidget (\x -> E.div [A.class_ "form-group"]
    [ E.label [A.class_ "control-label col-xs-2"] [E.text title]
    , E.div [A.class_ "col-xs-10 form-inline"] [x]
    ]) $ case spec of
      Bounded -> dimapWidget (snd . fromInterval z) toIntervalBetween $ numsW numW Between
      Varying -> dimapWidget (fromInterval z) toInterval (spanWidget2 numW)

fromInterval :: a -> Interval a -> (IntervalRange, (a,a))
fromInterval z i = case (lowerBound i, upperBound i) of
  (NegInf,    PosInf)     -> (Any,          (z,z))
  (NegInf,    Finite b)   -> (LessThan,     (b,b))
  (Finite a,  PosInf)     -> (GreaterThan,  (a,a))
  (Finite a,  Finite b)   -> (Between,      (a, b))
  _                       -> (Any,          (z,z)) -- empty

toInterval :: Ord a => (IntervalRange, (a,a)) -> Interval a
toInterval x = case x of
  (Any,         (_,_)) -> interval (NegInf,True) (PosInf,True)
  (GreaterThan, (x,_)) -> interval (Finite x,True) (PosInf,True)
  (LessThan,    (_,y)) -> interval (NegInf,True) (Finite y,True)
  -- If x > y, we don't want to generate empty, so increase upper bound to match lower bound
  (Between,     (x,y)) ->
    if x > y
        then interval (Finite x,True) (Finite x,True)
        else interval (Finite x,True) (Finite y,True)

toIntervalBetween :: Ord a => (a,a) -> Interval a
toIntervalBetween (x,y) = toInterval (Between, (x,y))

spanWidget2 :: (Bool -> Widget' a) -> Widget' (IntervalRange, (a,a))
spanWidget2 numW s x = composeWidget spanTypeW (numsW numW $ fst x) s x

spanTypeW :: Widget' IntervalRange
spanTypeW = selectWidget
  [ (Any,         "Any")
  , (GreaterThan, "Greater than")
  , (LessThan,    "Less than")
  , (Between,     "Between")
  ]

numsW :: (Bool -> Widget' a) -> IntervalRange -> Widget' (a, a)
numsW numW infFin = composeWidget (numW (fst $ visible infFin)) (numW (snd $ visible infFin))

visible :: IntervalRange -> (Bool,Bool)
visible x = case x of
  Any         -> (False, False)
  GreaterThan -> (True, False)
  LessThan    -> (False, True)
  Between     -> (True, True)

-- TODO is the (Monoid Html) instance what we need?
composeWidget :: Widget' b -> Widget' c -> Widget' (b,c)
composeWidget a b = bothWidget mappend (subWidget Control.Lens._1 a) (subWidget Control.Lens._2 b)
