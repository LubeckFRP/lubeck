
{-# LANGUAGE
    RankNTypes
  , NoMonomorphismRestriction
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  , TupleSections
  , FlexibleContexts
  , NoImplicitPrelude
  , MultiParamTypeClasses
  , DeriveFunctor
  #-}

module Lubeck.DV.Internal.Normalized
where

import BasePrelude

{-| Tag a value to keep track of the fact that it is /normalized/, i.e. in the unit hypercube. -}
newtype Normalized a = Normalized { getNormalized :: a }
  deriving (Eq, Ord, Num, Fractional, Real, RealFrac, Floating)

instance Show a => Show (Normalized a) where
  -- OK because of overloaded literal
  show (Normalized x) = show x

normalize :: (Ord a, RealFloat a) => Maybe (a, a) -> a -> Normalized a
normalize Nothing        x = Normalized x
normalize (Just (lb,ub)) x
  | abs (ub - lb) < 0.00001      = Normalized x
  | isNaN ((x - lb) / (ub - lb)) = Normalized x
  | otherwise                    = Normalized $ (x - lb) / (ub - lb)
