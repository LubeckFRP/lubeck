
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, TypeFamilies, OverloadedStrings,
  NamedFieldPuns, CPP, NoMonomorphismRestriction, BangPatterns, StandaloneDeriving
  , ScopedTypeVariables, DeriveTraversable #-}

module Lubeck.Drawing.Internal.MMap
  ( MMap
  , singletonMMap
  , toListMMap
  )
where

import BasePrelude
import Data.Map.Strict(Map)
import qualified Data.Map as Map

{-|
Map with a better Monoid instance.
See https://mail.haskell.org/pipermail/libraries/2012-April/017747.html

-}
newtype MMap k a = MMap { getMMap :: Map k a }
  deriving (Functor, Foldable, Traversable)

instance (Ord k, Monoid v) =>  Monoid (MMap k v) where
  mempty  = MMap $ Map.empty
  mappend (MMap a) (MMap b) = MMap $ Map.unionWith mappend a b

singletonMMap k v = MMap $ Map.singleton k v

toListMMap (MMap m) = Map.toList m
