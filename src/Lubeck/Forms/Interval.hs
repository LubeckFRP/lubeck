
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
import Numeric.Interval (Interval)
import qualified Numeric.Interval as I
import Data.Time.Calendar (Day(..))

import Data.JSString (JSString, pack, unpack)

import qualified Web.VirtualDom.Html as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events as Ev

import Control.Lens (over, under, set, view, review, preview, lens, Lens, Lens', Prism, Prism', Iso, Iso')
import qualified Control.Lens

import Lubeck.Forms
import Lubeck.Forms.Select
import Lubeck.Util()
import BD.Query.PostQuery(formatDateUTC, parseDateUTC) -- TODO move these

data WRange = Any | LessThen | GreaterThen | Between deriving (Show, Eq)

integerIntervalWidget :: JSString -> Widget' (Interval (Maybe Int))
integerIntervalWidget = customIntervalWidget 0 hideableIntegerWidget
--
dateIntervalWidget :: Day -> JSString -> Widget' (Interval (Maybe Day))
dateIntervalWidget dayNow = customIntervalWidget dayNow hideableDateWidget

customIntervalWidget :: Ord a => a -> (Bool -> Widget' a) -> JSString -> Widget' (Interval (Maybe a))
customIntervalWidget z numW title = id
    $ mapHtmlWidget (\x -> E.div [A.class_ "form-group form-inline"] $ pure $ E.label [] [E.text title, x])
    $ lmapWidget fromInterval
    $ rmapWidget toInterval
    $ spanWidget2
  where
    -- fromInterval :: Ord a => Interval (Maybe a) -> (String, (a, a))
    -- toInterval   :: Ord a => (String, (a, a))   -> Interval (Maybe a)
    fromInterval i
      | I.null i = ((Any), (z,z))
      | otherwise = case (I.inf i, I.sup i) of
        (Just x,  Nothing) -> (GreaterThen, (x,x))
        (Nothing, Just y)  -> (LessThen,    (y,y))
        (Just x,  Just y)  -> (Between,     (x,y))
        _                  -> (Any,         (z,z))
    toInterval x = case x of
      (Any,         (_,_)) -> Nothing I.... Nothing
      (GreaterThen, (x,_)) -> Just x  I.... Nothing
      (LessThen,    (_,y)) -> Nothing I.... Just y
      (Between,     (x,y)) -> Just x  I.... Just y

    -- spanWidget2 :: Widget' (JSString, (a, a))
    -- spanTypeW :: Widget' JSString
    -- numsW :: JSString -> Widget' (a, a)
    spanWidget2 s x = composeWidget spanTypeW (numsW $ fst x) s x
    spanTypeW = selectWidget
      [ (Any,         "Any")
      , (LessThen,    "Less than")
      , (GreaterThen, "Greater than")
      , (Between,     "Between")
      ]
    visible x = case x of
      Any         -> (False, False)
      GreaterThen -> (True, False)
      LessThen    -> (False, True)
      Between     -> (True, True)
    numsW infFin = composeWidget (numW (fst $ visible infFin)) (numW (snd $ visible infFin))

-- TODO is the (Monoid Html) instance what we need?
composeWidget :: Widget' a -> Widget' b -> Widget (a,b) (a,b)
composeWidget a b = bothWidget mappend (subWidget Control.Lens._1 a) (subWidget Control.Lens._2 b)

hideableIntegerWidget :: Bool -> Widget' Int
hideableIntegerWidget False _ _ = mempty
hideableIntegerWidget True sink val = E.input
  [ A.class_ "form-control"
  , A.type_ "number"
  -- , A.style_ $ if enabled then "" else "visibility:hidden"
  , Ev.change $ \e -> sink $ read $ unpack $ Ev.value e
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
