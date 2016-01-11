
{-# LANGUAGE OverloadedStrings #-}

module Lubeck.Forms.Interval where

import qualified Data.List
import Numeric.Interval (Interval)
import qualified Numeric.Interval as I
import Data.Time.Calendar (Day(..))

import Data.JSString (JSString, pack, unpack)

import qualified GHCJS.VDOM.Event as Ev
import qualified GHCJS.VDOM.Element as E
import qualified GHCJS.VDOM.Attribute as A

import Control.Lens (over, under, set, view, review, preview, lens, Lens, Lens', Prism, Prism', Iso, Iso')
import qualified Control.Lens

import Lubeck.Forms
import Lubeck.Forms.Select
import Lubeck.Util(customAttrs)
import BD.Query.PostQuery(formatDateUTC, parseDateUTC) -- TODO move these


integerIntervalWidget :: JSString -> Widget' (Interval (Maybe Int))
integerIntervalWidget = customIntervalWidget 0 hideableIntegerWidget
--
dateIntervalWidget :: String -> Widget' (Interval Day)
dateIntervalWidget = customIntervalWidget (Day 0) hideableDateWidget

customIntervalWidget :: Ord a => a -> (Bool -> Widget' a) -> JSString -> Widget' (Interval (Maybe a))
customIntervalWidget z numW title = id
    $ mapHtmlWidget (\x -> E.div [A.class_ "form-group form-inline"] $ E.label () [E.text title, x])
    $ lmapWidget fromInterval
    $ rmapWidget toInterval
    $ spanWidget2
  where
    -- fromInterval :: Ord a => Interval (Maybe a) -> (String, (a, a))
    -- toInterval   :: Ord a => (String, (a, a))   -> Interval (Maybe a)
    -- spanWidget2 :: Widget' (JSString, (a, a))
    -- spanTypeW :: Widget' JSString
    -- numsW :: JSString -> Widget' (a, a)
    fromInterval i
      | I.null i = (("inf-inf"), (z,z))
      | otherwise = case (I.inf i, I.sup i) of
        (Just x,  Nothing) -> (("fin-inf"), (x,x))
        (Nothing, Just y)  -> (("inf-fin"), (y,y))
        (Just x,  Just y)  -> (("fin-fin"), (x,y))
        _                  -> (("inf-inf"), (z,z))
    toInterval x = case x of
      (("inf-inf"), (_,_)) -> I.empty
      (("fin-inf"), (x,_)) -> Just x  I.... Nothing
      (("inf-fin"), (_,y)) -> Nothing I.... Just y
      (("fin-fin"), (x,y)) -> Just x  I.... Just y
    spanWidget2 s x = composeWidget spanTypeW (numsW $ fst x) s x
    spanTypeW = selectWidget
      [ ("inf-inf", "Any")
      , ("inf-fin", "Less than")
      , ("fin-inf", "Greater than")
      , ("fin-fin", "Between")
      ]
    visible x = case x of
      "inf-inf" -> (False, False)
      "fin-inf" -> (True, False)
      "inf-fin" -> (False, True)
      _         -> (True, True)
    numsW infFin = composeWidget (numW (fst $ visible infFin)) (numW (snd $ visible infFin))

-- TODO is the (Monoid Html) instance what we need?
composeWidget :: Widget' a -> Widget' b -> Widget (a,b) (a,b)
composeWidget a b = bothWidget mappend (subWidget Control.Lens._1 a) (subWidget Control.Lens._2 b)

hideableIntegerWidget :: Bool -> Widget' Int
hideableIntegerWidget False _ _ = mempty
hideableIntegerWidget True s v = E.input
  [ A.class_ "form-control"
  , A.type_ "number"
  -- , A.style_ $ if enabled then "" else "visibility:hidden"
  , Ev.change $ \e -> s $ read $ unpack $ Ev.value e
  , A.value (pack $ show v)
  ]
  ()

hideableDateWidget :: Bool -> Widget' Day
hideableDateWidget False _ _ = mempty
hideableDateWidget True s v = E.input
  [ A.class_ "form-control"
  , A.type_ "date"
  , Ev.change $ \e -> s $ readDate $ unpack $ Ev.value e
  , A.value (pack $ showDate v)
  ]
  ()
  where
    readDate x = case parseDateUTC x of { Just x -> x }
    showDate = formatDateUTC
