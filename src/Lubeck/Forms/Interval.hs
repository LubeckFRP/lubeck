
{-# LANGUAGE OverloadedStrings #-}

module Lubeck.Forms.Interval where

import Lubeck.Forms
import Lubeck.Forms.Select
import qualified Data.List
import Numeric.Interval (Interval)
import qualified Numeric.Interval as I

import Data.JSString (JSString, pack, unpack)

import qualified GHCJS.VDOM.Event as Ev
import qualified GHCJS.VDOM.Element as E
import qualified GHCJS.VDOM.Attribute as A

import Control.Lens (over, under, set, view, review, preview, lens, Lens, Lens', Prism, Prism', Iso, Iso')
import qualified Control.Lens

-- data Interval a


-- integerIntervalWidget : String -> Widget' (Interval Int)
-- integerIntervalWidget = customIntervalWidget 0 hideableIntegerWidget
--
-- dateIntervalWidget : String -> Widget' (Interval Date)
-- dateIntervalWidget = customIntervalWidget (case Date.fromString "2015-11-06" of Ok x -> x) hideableDateWidget

customIntervalWidget :: a -> (Bool -> Widget' a) -> String -> Widget' (Interval (Maybe a))
customIntervalWidget z numW title = id
    -- $ mapHtmlWidget (\x -> E.div [A.class_ "form-group form-inline"] $ E.label () [E.text title, x])
    $ lmapWidget fromInterval
    $ rmapWidget toInterval
    $ spanWidget2
  where
    fromInterval :: Interval (Maybe a) -> (String, (a, a))
    toInterval   :: (String, (a, a))   -> Interval (Maybe a)
    fromInterval = undefined
    toInterval = undefined
    spanWidget2 :: Widget' (String, (a, a))
    spanWidget2 s x = composeWidget spanTypeW numsW s x
    -- fromInterval (Interval {min, max}) = case (min, max) of
    --   (NegInf, PosInf) -> (("inf-inf"), (z,z))
    --   (Fin x,  PosInf) -> (("fin-inf"), (x,x))
    --   (NegInf, Fin y)  -> (("inf-fin"), (y,y))
    --   (Fin x,  Fin y)  -> (("fin-fin"), (x,y))
    -- toInterval x = case x of
    --   (("inf-inf"), (_,_)) -> Interval {min = NegInf, max = PosInf}
    --   (("fin-inf"), (x,x)) -> Interval {min = Fin x,  max = PosInf}
    --   (("inf-fin"), (y,y)) -> Interval {min = NegInf, max = Fin y}
    --   (("fin-fin"), (x,y)) -> Interval {min = Fin x,  max = Fin y}

    spanTypeW :: Widget' String
    spanTypeW = undefined
    --     spanTypeW = selectWidget
    --       [ ("inf-inf", "Any")
    --       , ("inf-fin", "Less than")
    --       , ("fin-inf", "Greater than")
    --       , ("fin-fin", "Between")
    --       ]
    --     (showA, showB) = case fst x of
    --       "inf-inf" -> (False, False)
    --       "fin-inf" -> (True, False)
    --       "inf-fin" -> (True, False)
    --       _         -> (True, True)
    numsW :: Widget' (a, a)
    numsW = undefined
    --     numsW = composeWidget   (numW showA) (numW showB)

-- TODO is the (Monoid Html) instance what we need?
composeWidget :: Widget' a -> Widget' b -> Widget (a,b) (a,b)
composeWidget a b = bothWidget mappend (subWidget Control.Lens._1 a) (subWidget Control.Lens._2 b)
