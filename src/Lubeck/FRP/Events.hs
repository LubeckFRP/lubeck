
module Lubeck.FRP.Events
    ( updatesAtIndex
    , updatesAtKey
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
updatesAtIndex :: Events [a] -> [Events (Maybe a)]
updatesAtIndex e = fmap (updatesAt (flip safeIndex) e) [0..]
  where
    safeIndex :: [a] -> Int -> Maybe a
    safeIndex xs n = if n < 0
        then Nothing
        else Data.Maybe.listToMaybe (drop n xs)

-- |
-- Propagate events on a particular key.
--
updatesAtKey :: Ord k => Events (Map k a) -> k -> Events (Maybe a)
updatesAtKey = updatesAt Data.Map.lookup

-- More general version
updatesAt :: (a -> b -> c) -> Events b -> a -> Events c
updatesAt f e n = fmap (f n) e
