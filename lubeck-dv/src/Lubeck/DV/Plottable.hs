
module Lubeck.DV.Plottable
  ( Plottable(..)
  )
where

import Lubeck.Drawing(Str)

type Iso' a b = ( (a -> b), (b -> a) )

class Plottable a where
  dimension   :: Iso' a Double
  stringRep   :: a -> Str
  linearity   :: (a, a) -> (a -> a, a -> a)
  -- ticks       :: Int -> (Double, Double) -> [Double]
  -- "dimension"
