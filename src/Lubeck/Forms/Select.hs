
module Lubeck.Forms.Select where

import Lubeck.Forms
import qualified Data.List

selectWidget :: Eq a => [(a, String)] -> Widget' a
selectWidget xs = dimapWidget (show . toInt) (fromInt . read) $ selectWidget' (zip count names)
  where
    (vals, names) = unzip xs
    count         = fmap show [0..]
    indexed xs = (fromJust . (`Data.List.elemIndex` xs), (xs !!))
      where fromJust (Just x) = x
    (toInt, fromInt) = indexed vals

selectWidget' :: [(String, String)] -> Widget' String
selectWidget' = undefined
-- selectWidget'' valuesLabels s x = [select [class_ "form-control", onChange s, Attr.value x] $ List.map (\(v,l) -> option ([Attr.value v]
--   ++ if v == x then [Attr.selected True] else []
--   )  [text l]) valuesLabels]
