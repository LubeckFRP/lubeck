
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



-- What data (typically list of tuples/records) to visualize

-- What monadic input ("interactivity") will affect the visualization?
-- What style (if not default) to use
-- What post-transformations to apply to the drawing
-- What monadic output ("interactivity") is generated, if any?

-- Possibly divide the data set here (overlaps/facets), thereby "selecting" on tuplets
  -- Each subset can have local styles, post-drawing transformations and selections of data
    -- What "stat/aesthetic" (how are the tuples projected to the [Rn] or similar expected by EACH drawing functions)?
    -- Related:
      -- Where to put the ticks (if any)?
      -- How is the Rn data scaled?
