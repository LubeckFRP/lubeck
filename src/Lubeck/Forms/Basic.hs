
{-# LANGUAGE OverloadedStrings #-}

module Lubeck.Forms.Basic
    ( rangeWidget
    , integerWidget
    , dateWidget
    , hideableDateWidget
    , hideableIntegerWidget
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
import Lubeck.Util(formatDateUTC, parseDateUTC)

-- TODO the text varieties (password, search, textarea)
-- TODO double rational version of integer/range

rangeWidget :: Int -> Int -> Int -> Widget' Int
rangeWidget minBound maxBound step
  sink val = E.input
  [ A.class_ "form-control"
  , A.type_ "range"
  , A.min minBound
  , A.max maxBound
  , A.step step
  -- , A.style_ $ if enabled then "" else "visibility:hidden"
  , Ev.change $ \e -> do
      case Ev.value e of
        "" -> do
          print "Empty value for Int, ignoring."
          return ()
        s -> sink $ read $ unpack $ Ev.value e
  , Ev.mousemove $ \e -> do
      case Ev.value e of
        "" -> do
          print "Empty value for Int, ignoring."
          return ()
        s -> sink $ read $ unpack $ Ev.value e

  , A.value (pack $ show val)
  ]
  []

integerWidget :: Widget' Int
integerWidget sink val = E.input
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

dateWidget :: Widget' Day
dateWidget sink val = E.input
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

hideableIntegerWidget :: Bool -> Widget' Int
hideableIntegerWidget False = const $ const mempty
hideableIntegerWidget True  = integerWidget

hideableDateWidget :: Bool -> Widget' Day
hideableDateWidget False = const $ const mempty
hideableDateWidget True  = dateWidget
