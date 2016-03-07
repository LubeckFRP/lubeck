
module Lubeck.DV.Plottable
  (Plottable(..))
where


type Iso' a b = ( (a -> b), (b -> a) )

class Plottable a where
  dim  :: Iso' a Double
  -- "dimension"
