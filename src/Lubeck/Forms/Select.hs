
{-# LANGUAGE OverloadedStrings #-}

{-|
  Select widgets, using for choosing one element of an enumeration type.

  Most of these accept a list or range that defines input alternatives. Beware
  that sending a value not in this list may result in undefined behavior.

  For complete safety, use a 'Bounded' type with 'selectEnumBoundedWidget'.
-}
module Lubeck.Forms.Select
  ( selectWidget
  , selectEnumWidget
  , selectEnumBoundedWidget
  , selectWithPromptWidget
  ) where

import Lubeck.Forms
import qualified Data.List
import Control.Lens
import Lubeck.Util()
import qualified Data.Map
import Data.Monoid
import Data.JSString (JSString, pack, unpack)

import qualified Web.VirtualDom as VD
import qualified Web.VirtualDom.Html as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events as Ev

-- TODO Do not rely on show for these

-- |
-- A widget that visualizes all possible values of a type that is 'Enum' and 'Bounded'.
--
-- @
-- selectEnumWidget :: Widget' Bool
-- selectEnumWidget :: Widget' Word32
--
-- data BaseColor = Red | Green | Blue
-- selectEnumWidget :: Widget' BaseColor
-- @
--
-- See https://developer.mozilla.org/en/docs/Web/HTML/Element/Input
--
selectEnumBoundedWidget :: (Eq a, Enum a, Bounded a, Show a) => Widget' a
selectEnumBoundedWidget = selectEnumWidget minBound maxBound

-- |
-- A widget that visualizes the given range of an 'Enum' type.
--
-- @
-- selectEnumWidget [1..10] :: Widget' Integer
-- @
--
-- See https://developer.mozilla.org/en/docs/Web/HTML/Element/Input
--
selectEnumWidget :: (Eq a, Enum a, Show a) => a -> a -> Widget' a
selectEnumWidget lb ub = selectWidget $ fmap (\n -> (n, pack $ show n)) $ enumFromTo lb ub

-- |
-- A widget that visualizes a given set of values..
--
-- @
-- data Undo = DoNothing | Undo Int
-- selectWidget [(DoNothing, "Do nothing"), (Undo 1, "Undo once")]
-- @
--
-- See https://developer.mozilla.org/en/docs/Web/HTML/Element/Input
--
selectWidget :: Eq a => [(a, JSString)] -> Widget' a
selectWidget xs = dimapWidget (pack . show . toInt) (fromInt . read . unpack) $ selectWidget' (zip count names)
  where
    (vals, names) = unzip xs
    count         = fmap (pack . show) [0..]
    indexed xs = (fromJust . (`Data.List.elemIndex` xs), (xs !!))
      where
        fromJust (Just x) = x
        fromJust Nothing  = 0    -- XXX it crashes here with failed pattern match
                                 -- when input (current) value not present in options.
                                 -- (eg, inital value, or random value from the network)
                                 -- defaulting to index 0 means we do not select the current
                                 -- value, but first item instead. Reasonably safe choice IMO.
    (toInt, fromInt) = indexed vals

selectWithPromptWidget :: Eq a => [(a, JSString)] -> Widget a (Maybe a)
selectWithPromptWidget xs = dimapWidget f g (selectWidget' (prompt <> rawOpts))
  where
    f                 = pack . show . toIdx
    g                 = fromIdx . read . unpack

    promptIdx         = 0
    prompt            = [((pack . show) promptIdx, "Choose one")]
    rawOpts           = zip count names

    (vals, names)     = unzip xs
    count             = fmap (pack . show) [1..] -- take prompt into account

    toIdx             = fromJust . (`Data.List.elemIndex` vals)
    fromIdx           = byIdx vals

    byIdx xs x        = xs ^? element (x - 1) -- compensate offset for prompt
    fromJust (Just x) = x + 1                 -- compensate offset for prompt
    fromJust Nothing  = promptIdx             -- XXX it crashes here with failed pattern match
                                              -- when input (current) value not present in options.
                                              -- (eg, inital value, or random value from the network)
                                              -- defaulting to index 0 means we do not select the current
                                              -- value, but first item instead. Reasonably safe choice IMO.


selectWidget' :: [(JSString, JSString)] -> Widget' JSString
selectWidget' valuesAndLabels sink curVal =
  E.select
    [ A.class_ "form-control"
    , A.value curVal -- ?
    , Ev.change (\e -> sink $ Ev.value e)
    ]
    (fmap (\(v,l) -> E.option
      (optAttrs v curVal)
      [E.text l])
        valuesAndLabels)
  where
    optAttrs v x = [A.value v] ++ if v == x then [selected "true"]  else []
    selected = VD.attribute "selected"
