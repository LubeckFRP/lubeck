
{-# LANGUAGE OverloadedStrings #-}

module Lubeck.Forms.Select
  ( selectWidget
  , selectEnumWidget
  , selectEnumBoundedWidget
  ) where

import Lubeck.Forms
import qualified Data.List
import Lubeck.Util()
import qualified Data.Map
import Data.JSString (JSString, pack, unpack)

import qualified Web.VirtualDom as VD
import qualified Web.VirtualDom.Html as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events as Ev

-- TODO Not Show
selectEnumBoundedWidget :: (Eq a, Enum a, Bounded a, Show a) => Widget' a
selectEnumBoundedWidget = selectEnumWidget minBound maxBound

selectEnumWidget :: (Eq a, Enum a, Show a) => a -> a -> Widget' a
selectEnumWidget lb ub = selectWidget $ fmap (\n -> (n, pack . show n)) $ enumFromTo lb ub

selectWidget :: Eq a => [(a, JSString)] -> Widget' a
selectWidget xs = dimapWidget (pack . show . toInt) (fromInt . read . unpack) $ selectWidget' (zip count names)
  where
    (vals, names) = unzip xs
    count         = fmap (pack . show) [0..]
    indexed xs = (fromJust . (`Data.List.elemIndex` xs), (xs !!))
      where fromJust (Just x) = x
    (toInt, fromInt) = indexed vals

selectWidget' :: [(JSString, JSString)] -> Widget' JSString
selectWidget' valuesLabels s x =
  E.select
    [ A.class_ "form-control"
    , A.value x
    , Ev.change (\e -> s $ Ev.value e)
    ]
    (fmap (\(v,l) -> E.option
      (optAttrs v x)
      [E.text l])
        valuesLabels)

-- optAttrs v selected = [A.value v]{-++ if v == x then [Attr.selected True]  else []-})

optAttrs v x = [A.value v] ++ if v == x then [selected "true"]  else []
  where
    selected = VD.attribute "selected"
-- optAttrs v x = customAttrs $ Data.Map.fromList $ [("value", unpack v)] ++ if v == x then [("selected", "true")]  else []
