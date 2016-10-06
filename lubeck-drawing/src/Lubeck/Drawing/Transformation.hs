
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, TypeFamilies, OverloadedStrings,
  NamedFieldPuns, CPP, NoMonomorphismRestriction, BangPatterns, StandaloneDeriving
  , ScopedTypeVariables #-}

module Lubeck.Drawing.Transformation
  -- TODO hide constructor without making Drawing less efficient
  (
  -- ** 2D transformations
    Transformation(..)
  -- $matrixContructorLayout
  , matrix
  , transformationToMatrix
  , transformVector
  , transformPoint
  , lin
  , transp
  , transl
  -- ** 1D transformations
  , Transformation1(..)
  , matrix1
  , scaling1
  , translation1
  , transformationToMatrix1
  , transformVector1
  , transformPoint1
  , T1
  , T2
  , transformRect
  , transformDirection

  , translation
  , translationX
  , translationY
  , scaling
  , scalingX
  , scalingY
  , scalingXY
  , rotation
  , shearingX
  , shearingY

  , rectToTransf
  , transfToRect
  , transformLineSeg
  , lineSegToTransf
  , transfToLineSeg
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


{-
For consistency:

points, vectors, affine transformations

  P1, V1, T1
  P2, V2, T2

Arguably Tn should be a type/data family.
-}
type T2 a = Transformation a
type T1 a = Transformation1 a


{-|
A 2D affine transformation, represented as a 3x3 matrix using homogeneous coordinates.

I.e rotation by T is represented as
    cos T   sin T   0
  ( -sin T  cos T   0 )
    0       0       1

-}
newtype Transformation a = TF { getTF :: M33 a }
  deriving (Eq, Ord)

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



transformRect :: Num a => T2 a -> Rect a -> Rect a
transformRect t (Rect_ p1 p2) = Rect_ (transformPoint t p1) (transformPoint t p2)


















{-|
A 2D affine transformation, represented as a 3x3 matrix using homogeneous coordinates.

I.e rotation by T is represented as
    cos T   sin T   0
  ( -sin T  cos T   0 )
    0       0       1

-}
newtype Transformation1 a = TF1 { getTF1 :: M22 a }
  deriving (Eq, Ord)

instance Num a => Monoid (Transformation1 a) where
  mempty                = TF1 identity
  mappend (TF1 x) (TF1 y) = TF1 (x !*! y)

instance Num a => Num (Transformation1 a) where
  TF1 x + TF1 y = TF1 (x !+! y)
  TF1 x - TF1 y = TF1 (x !-! y)
  TF1 x * TF1 y = TF1 (x !*! y)
  abs    = error "Missing in Num (Transformation1 a)"
  signum = error "Missing in Num (Transformation1 a)"
  fromInteger n = TF1 $ identity !!* fromInteger n

-- linear 1.19 vs linear 1.20
instance
  ( Floating a
#if !MIN_VERSION_linear(1,20,0)
  , Epsilon a
#endif
  ) => Fractional (Transformation1 a) where
  recip (TF1 x) = TF1 (inv22_ x)
  fromRational = error "Missing in Fractional (Transformation1 a)"

#if MIN_VERSION_linear(1,20,0)
inv22_ = inv22
#else
inv22_ m = case inv22 m of
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
matrix1 :: Num a => (a, a) -> Transformation1 a
matrix1 (a,x) = TF1 $ V2 (V2 a x) (V2 0 1)
{-# INLINE matrix1 #-}


{-| Convert a transformation to a matrix.

@
matrix . transformationToMatrix = id
transformationToMatrix . matrix = id
@
-}
transformationToMatrix1 :: Num a => Transformation1 a -> (a, a)
transformationToMatrix1 (TF1 (V2 (V2 a x) (V2 _ _))) = (a, x)
{-# INLINABLE transformationToMatrix1 #-}

{-| Transform a vector, using the linear component of the transformation.
-}
transformVector1 :: Num a => Transformation1 a -> V1 a -> V1 a
transformVector1 t (V1 x) =
  let (TF1 (V2 (V2 a e) (V2 _ _))) = t
  in V1 (a*x)
{-# INLINABLE transformVector1 #-}

{-| Transform a point, by applying the linear component of the transformation and translating the result.
-}
transformPoint1 :: Num a => Transformation1 a -> P1 a -> P1 a
transformPoint1 t (P (V1 x)) =
  let (TF1 (V2 (V2 a e) (V2 _ _))) = t
  in P $ V1 (a*x + e)
{-# INLINABLE transformPoint1 #-}

instance (Num a, Show a) => Show (Transformation1 a) where
  show t = "matrix1 " <> show (transformationToMatrix1 t)

instance (Num a, Show a) => Show (Transformation a) where
  show t = "matrix " <> show (transformationToMatrix t)

scaling1 :: Num a => a -> Transformation1 a
scaling1 x = matrix1 (x,0)

translation1 :: Num a => a -> Transformation1 a
translation1 x = matrix1 (1,x)


transformDirection :: Num n => Transformation n -> Direction V2 n -> Direction V2 n
transformDirection t (Direction v) = Direction (transformVector t v)


{-| Translates (move) an object. -}
translation :: Num a => V2 a -> Transformation a
translation (V2 a b) = matrix (1,0,0,1,a,b)

{-| Translates (move) an object along the horizonal axis.
A positive argument will move the object to the right. -}
translationX :: Num a => a -> Transformation a
translationX a = translation (V2 a 0)

{-| Translates (move) an object along the vertical axis.
A positive argument will move the object upwards (as opposed to standard SVG behavior). -}
translationY :: Num a => a -> Transformation a
translationY b = translation (V2 0 b)

{-| Scales (stretches) an object, preserving its horizontal/vertical proportion. -}
scaling :: Num a => a -> Transformation a
scaling a = matrix (a,0,0,a,0,0)

scalingXY :: Num a => V2 a -> Transformation a
scalingXY (V2 a b) = matrix (a,0,0,b,0,0)

{-| Scales (stretches) an object. -}
scalingX :: Num a => a -> Transformation a
scalingX a = matrix (a,0,0,1,0,0)

{-| Scales (stretches) an object. -}
scalingY :: Num a => a -> Transformation a
scalingY b = matrix (1,0,0,b,0,0)

{-| Rotates an object. A positive vale will result in a counterclockwise rotation
    and negative value in a clockwise rotation. -}
rotation :: Floating a => Angle a -> Transformation a
-- rotation (Radians a) = matrix (cos a, 0 - sin a, sin a, cos a, 0, 0)
rotation (Radians a) = matrix (cos (-a), 0 - sin (-a), sin (-a), cos (-a), 0, 0)

-- {-| Shears an object. -}
-- shearing :: Num a => a -> a -> Transformation a
-- shearing a b = matrix (1, b, a, 1, 0, 0)

{-| Shears an object. -}
shearingX :: Num a => a -> Transformation a
shearingX a = matrix (1, 1, a, 1, 0, 0)

{-| Shears an object. -}
shearingY :: Num a => a -> Transformation a
shearingY b = matrix (1, b, 1, 1, 0, 0)

{-# INLINABLE translation #-}
{-# INLINABLE translationX #-}
{-# INLINABLE translationY #-}
{-# INLINABLE scaling #-}
{-# INLINABLE scalingX #-}
{-# INLINABLE scalingY #-}
{-# INLINABLE rotation #-}
{-# INLINABLE shearingX #-}
{-# INLINABLE shearingY #-}

{-|
Turn a rectangle into a transformation that transforms the unit square into
the original rectangle.

Inverse of @rectToTransf@.
-}
rectToTransf :: Num a => Rect a -> T2 a
rectToTransf (Rect_ (p1@(P (v1@(V2 x1 y1)))) (p2@(P (v2@(V2 x2 y2))))) =
  translation (p1 .-. origin) <> scalingXY (p2 .-. p1)

{-|
The inverse of @rectToTransf@.
-}
transfToRect :: Num a => T2 a -> Rect a
transfToRect t = transformRect t (rect 0 0 1 1)



transformLineSeg :: Num a => T1 a -> LineSeg a -> LineSeg a
transformLineSeg t (LineSeg p1 p2) = LineSeg (transformPoint1 t p1) (transformPoint1 t p2)

lineSegToTransf :: Num a => LineSeg a -> T1 a
lineSegToTransf (LineSeg (p1@(P (v1@(V1 x1)))) (p2@(P (v2@(V1 x2))))) =
  translation1 (p1 .-. origin) <> scaling1 (p2 .-. p1)
  where
    translation1 (V1 x) = matrix1 (1,x)
    scaling1 (V1 x)     = matrix1 (x,0)
    -- origin = P (V1 0)

transfToLineSeg :: Num a => T1 a -> LineSeg a
transfToLineSeg t = transformLineSeg t (lineseg 0 1)
