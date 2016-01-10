
{-# LANGUAGE OverloadedStrings #-}

module Lubeck.Forms.Select where

import Lubeck.Forms
import qualified Data.List

import Data.JSString (JSString, pack, unpack)

import qualified GHCJS.VDOM.Event as Ev
import qualified GHCJS.VDOM.Element as E
import qualified GHCJS.VDOM.Attribute as A

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
      ([A.value v]{-++ if v == x then [Attr.selected True]  else []-})
      [E.text l])
        valuesLabels)
