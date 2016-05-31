
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, TypeFamilies, OverloadedStrings,
  NamedFieldPuns, CPP, NoMonomorphismRestriction, BangPatterns, StandaloneDeriving
  , ScopedTypeVariables #-}

module Lubeck.Drawing.Transformation
  -- TODO hide constructor without making Drawing less efficient
  ( Transformation(..)
  -- $matrixContructorLayout
  , matrix
  , transformationToMatrix
  , transformVector
  , transformPoint
  , lin
  , transp
  , transl
  )
where

import BasePrelude

import Linear.Vector
import Linear.Affine
import Linear.Matrix hiding (translation)
import Linear.Metric
import Linear.V0
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4
import qualified Linear.V1
import qualified Linear.V2
import qualified Linear.V3
import qualified Linear.V4

#if MIN_VERSION_linear(1,20,0)
#else
import Linear.Epsilon
#endif

import Lubeck.Drawing.Types

{-|
A 2D affine transformation, represented as a 3x3 matrix using homogeneous coordinates.

I.e rotation by T is represented as
    cos T   sin T   0
  ( -sin T  cos T   0 )
    0       0       1

-}
newtype Transformation a = TF { getTF :: M33 a }

instance Num a => Monoid (Transformation a) where
  mempty                = TF identity
  mappend (TF x) (TF y) = TF (x !*! y)

instance Num a => Num (Transformation a) where
  TF x + TF y = TF (x !+! y)
  TF x - TF y = TF (x !-! y)
  TF x * TF y = TF (x !*! y)
  abs    = error "Missing in Num (Transformation a)"
  signum = error "Missing in Num (Transformation a)"
  fromInteger n = TF $ identity !!* fromInteger n

-- linear 1.19 vs linear 1.20
instance
  ( Floating a
#if !MIN_VERSION_linear(1,20,0)
  , Epsilon a
#endif
  ) => Fractional (Transformation a) where
  recip (TF x) = TF (inv33_ x)
  fromRational = error "Missing in Fractional (Transformation a)"

#if MIN_VERSION_linear(1,20,0)
inv33_ = inv33
#else
inv33_ m = case inv33 m of
  Nothing -> m
  Just mi -> mi
#endif

-- $matrixContructorLayout
--
-- Both of these use same layout as SVG, see https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform
--
-- That is
--
-- @
-- a c e
-- b d f
-- 0 0 1
-- @
--
-- I.e. the identity is @(1,0,0,1,0,0)@ and the translation component is @(0,0,0,0,x,y)@.
--
-- This is column-major order with an implied extra row (0 0 1)

{-| Create a transformation from a matrix. -}
matrix :: Num a => (a, a, a, a, a, a) -> Transformation a
matrix (a,b,c,d,e,f) = TF $ V3 (V3 a c e) (V3 b d f) (V3 0 0 1)
{-# NOINLINE matrix #-}
{-# RULES
"matrix/matrix" forall a b c d e f.  matrix (a,b,c,d,e,f) = TF $ V3 (V3 a c e) (V3 b d f) (V3 0 0 1)
 #-}

{-| Convert a transformation to a matrix.

@
matrix . transformationToMatrix = id
transformationToMatrix . matrix = id
@
-}
transformationToMatrix :: Num a => Transformation a -> (a, a, a, a, a, a)
transformationToMatrix (TF (V3 (V3 a c e) (V3 b d f) (V3 _ _ _))) = (a,b,c,d,e,f)
{-# INLINABLE transformationToMatrix #-}

{-| Transform a vector, using the linear component of the transformation.
-}
transformVector :: Num a => Transformation a -> V2 a -> V2 a
transformVector t (V2 x y) =
  let (TF (V3 (V3 a c e) (V3 b d f) (V3 _ _ _))) = t
  in V2 (a*x + c*y) (b*x + d*y)
{-# INLINABLE transformVector #-}

{-| Transform a point, by applying the linear component of the transformation and translating the result.
-}
transformPoint :: Num a => Transformation a -> P2 a -> P2 a
transformPoint t (P (V2 x y)) =
  let (TF (V3 (V3 a c e) (V3 b d f) (V3 _ _ _))) = t
  in P $ V2 (a*x + c*y + e) (b*x + d*y + f)
{-# INLINABLE transformPoint #-}


{-| Return the linear component of a transformation.

@
    a c x        a c 0
  ( b d y ) -> ( b d 0 )
    0 0 1        0 0 1
@-}
lin :: Num a => Transformation a -> Transformation a
lin t =
  -- let (a,b,c,d,e,f) = transformationToMatrix t
  let (TF (V3 (V3 a c e) (V3 b d f) (V3 _ _ _))) = t
  in matrix (a,b,c,d,0,0)


{-| Return the transposition of the linear component of a transformation.

@
    a c x        a b 0
  ( b d y ) -> ( c d 0 )
    0 0 1        0 0 1
@-}
transp :: Num a => Transformation a -> Transformation a
transp t =
  -- let (a,b,c,d,e,f) = transformationToMatrix t
  let (TF (V3 (V3 a c e) (V3 b d f) (V3 _ _ _))) = t
  in matrix (a,c,b,d,0,0)

{-| Return the translation component of a transformation.

@
    a c x        x
  ( b d y ) -> (   )
    0 0 1        y
@-}
transl :: Num a => Transformation a -> V2 a
transl t =
  -- let (a,b,c,d,e,f) = transformationToMatrix t
  let (TF (V3 (V3 a c e) (V3 b d f) (V3 _ _ _))) = t
  in V2 e f
