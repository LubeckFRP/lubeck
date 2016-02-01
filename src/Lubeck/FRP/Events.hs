
module Lubeck.FRP.Events
    ( overs
    , lookup
    , oversG
    ) where

import Lubeck.FRP
import qualified Data.Maybe
import qualified Data.Map
import Data.Map (Map)

-- |
-- Propagate events on the respective index.
--
-- I.e. in the resulting list, the first element has all events occuring when
-- the original event has an occurance with at least one element,
-- the second when the original has at least two elements, and so on.
--
overs :: Events [a] -> [Events (Maybe a)]
overs e = fmap (\n -> fmap (`safeIndex` n) e) [0..]
  where
    safeIndex :: [a] -> Int -> Maybe a
    safeIndex xs n = if n < 0
        then Nothing
        else Data.Maybe.listToMaybe (drop n xs)


lookups :: Ord k => Events (Map k a) -> k -> Events (Maybe a)
lookups = oversG Data.Map.lookup

-- oversG :: (k -> Map k b -> Maybe b) -> Events (Map k b) -> k -> Events (Maybe b)

oversG :: (a -> b -> c) -> Events b -> a -> Events c
oversG f e n = fmap (f n) e
