
{-# LANGUAGE OverloadedStrings #-}

module Lubeck.Forms.Basic
    ( hideableDateWidget
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
import Lubeck.Util()
import BD.Query.PostQuery(formatDateUTC, parseDateUTC) -- TODO move these

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
