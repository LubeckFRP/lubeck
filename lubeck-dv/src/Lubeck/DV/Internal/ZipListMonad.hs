
module Lubeck.DV.Internal.ZipListMonad where

import Control.Applicative

instance Monad ZipList where
   return = ZipList . repeat
   ZipList [] >>= _ = ZipList []
   ZipList xs >>= f = ZipList $ zipWith ((!!) . cycle . getZipList . f) xs
      [0..]
