
{-# LANGUAGE OverloadedStrings #-}

module Lubeck.Forms.Basic
    (
    -- * Basic types
    -- ** Numbers
      rangeWidget
    , integerWidget
    , dateWidget
    -- ** Strings
    , staticStringWidget
    -- * Hideable widgets
    , hideableDateWidget
    , hideableIntegerWidget
    , hideableRangeWidget
    -- * Display widgets
    , intDisplayWidget
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
import Lubeck.Forms.Select
import Lubeck.Util(formatDateFromUTC, parseDateToUTC, showJS)

-- TODO the text varieties (password, search, textarea)
-- TODO double rational version of integer/range

-- | Displays a static string.
staticStringWidget :: Widget JSString a
staticStringWidget _ val = E.div
  [ A.class_ "col-xs-2" ]
  [ E.p [ A.class_ "text-center" ]
        [ E.text (showJS val) ]
  ]

-- | A widget for selecting an integer from a range.
rangeWidget
  :: Int    -- ^ Min value (included)
  -> Int    -- ^ Max value (included)
  -> Int    -- ^ Step size
  -> Widget' Int
rangeWidget minBound maxBound step
  sink val = E.input
  [ A.class_ "form-control"
  , A.type_ "range"
  , A.min minBound
  , A.max maxBound
  , A.step step
  -- , A.style_ $ if enabled then "" else "visibility:hidden"
  , Ev.change $ \e ->
      case Ev.value e of
        "" -> do
          print "Empty value for Int, ignoring."
          return ()
        s -> sink $ read $ unpack $ Ev.value e
  , Ev.mousemove $ \e ->
      case Ev.value e of
        "" -> do
          print "Empty value for Int, ignoring."
          return ()
        s -> sink $ read $ unpack $ Ev.value e

  , A.value (pack $ show val)
  ]
  []

-- | A widget for selecting an integer.
integerWidget :: Widget' Int
integerWidget sink val = E.input
  [ A.class_ "form-control"
  , A.type_ "number"
  -- , A.style_ $ if enabled then "" else "visibility:hidden"
  , Ev.change $ \e ->
      case Ev.value e of
        "" -> do
          print "Empty value for Int, ignoring."
          return ()
        s -> sink $ read $ unpack $ Ev.value e

  , A.value (pack $ show val)
  ]
  []

-- | A widget for selecting a calendar date.
dateWidget :: Widget' Day
dateWidget sink val = E.input
  [ A.class_ "form-control"
  , A.type_ "date"
  , Ev.change $ \e -> maybeSink sink $ parseDateToUTC $ Ev.value e
  , A.value (showDate val)
  ]
  []
  where
    maybeSink sink val = case val of
      Just x -> sink x
      -- <input>'s value can be set to ""
      -- we probably should not ignore this, but pass upstream and cancel given date boundary?
      Nothing -> return ()
    showDate = formatDateFromUTC

hideableIntegerWidget :: Bool -> Widget' Int
hideableIntegerWidget False = const $ const mempty
hideableIntegerWidget True  = integerWidget

hideableRangeWidget :: Int -> Int -> Int -> Bool -> Widget' Int
hideableRangeWidget _  _  _ False = const $ const mempty
hideableRangeWidget lb ub s True  = rangeWidget lb ub s

hideableDateWidget :: Bool -> Widget' Day
hideableDateWidget False = const $ const mempty
hideableDateWidget True  = dateWidget

-- | A widget for displaying an integer
intDisplayWidget :: Widget Int ()
intDisplayWidget _ val = E.div
  [ A.class_ "col-xs-2" ]
  [ E.p [ A.class_ "text-center" ]
        [ E.text (pack $ show val) ]
  ]
