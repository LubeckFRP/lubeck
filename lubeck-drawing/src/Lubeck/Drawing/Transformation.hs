
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, TypeFamilies, OverloadedStrings,
  NamedFieldPuns, CPP, NoMonomorphismRestriction, BangPatterns, StandaloneDeriving
  , ScopedTypeVariables #-}

-- {-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}

module Lubeck.Drawing.Transformation
  -- (
  -- -- * Creating drawings
  -- -- ** Geometry
  --   Point(..)
  -- , V1(..)
  -- , V2(..)
  -- , V3(..)
  -- , V4(..)
  -- , P1
  -- , P2
  -- , P3
  -- , P4
  -- , _x
  -- , _y
  --
  -- , Angle
  -- , acosA
  -- , turn
  -- , angleToRadians
  -- , angleToDegrees
  -- -- TODO move/rename these?
  -- , offsetVectors
  -- , betweenPoints
  --
  -- , Direction
  -- , dir
  -- , fromDirection
  -- , angleBetween
  -- , angleBetweenDirections
  --
  -- , Rect(..)
  -- , fitInsideRect
  --
  -- -- ** Transformations
  -- , Transformation
  -- , negTransformation
  -- , lin
  -- , transp
  -- , transl
  -- , transformVector
  -- , transformPoint
  -- , transformDirection
  -- , transformEnvelope
  -- , transformationToMatrix
  -- -- ** Raw transformations
  -- , rotation
  -- , scaling
  -- , scalingX
  -- , scalingY
  -- , translation
  -- , translationX
  -- , translationY
  -- , shearingX
  -- , shearingY
  -- -- $matrixContructorLayout
  -- , matrix
  -- -- ** Applying transformations
  -- , transform
  --
  -- , rotate
  -- , scale
  -- , scaleX
  -- , scaleY
  -- , scaleXY
  -- , translate
  -- , translateX
  -- , translateY
  -- -- , scaleXY
  -- , shearX
  -- , shearY
  --
  -- -- ** Styling
  -- , Style
  -- , styleNamed
  -- , fillColor
  -- , fillColorA
  -- , strokeColor
  -- , strokeColorA
  -- , strokeWidth
  -- -- *** Rendering styles
  -- , styleToAttrString
  -- -- *** Applying styles
  -- , style
  --
  -- -- *** Line style
  -- , dash
  -- , dashing
  --
  -- -- *** Text
  -- , text
  -- , textMiddle
  -- , textEnd
  -- , textLeftMiddle
  -- , textMiddleMiddle
  -- , textRightMiddle
  -- , TextAnchor(..)
  -- , AlignmentBaseline(..)
  -- , FontStyle(..)
  -- , FontSize(..)
  -- , FontWeight(..)
  -- , TextOptions(..)
  -- , textWithOptions
  --
  -- -- ** Events
  -- , addHandler
  -- -- , addProperty
  --
  -- -- ** Embedded SVG
  -- , Embed(..)
  -- , addEmbeddedSVG
  -- , addEmbeddedSVGFromStr
  --
  -- -- ** Envelopes, Alignment, Juxtaposition
  -- , Envelope
  -- , envelope
  -- -- transformEnvelope
  -- , unitX
  -- , unitY
  -- , posDiagonal
  -- , negDiagonal
  -- , (|||)
  -- , (===)
  -- , juxtapose
  --
  -- , boundaries
  -- , align'
  -- , align
  -- , OctagonSide(..)
  --
  -- -- ** Drawings
  -- , Drawing
  -- -- ** Basic drawings
  -- , transparent
  -- , circle
  -- , square
  -- , triangle
  -- , horizontalLine
  -- , verticalLine
  -- , segments
  -- , polygon
  --
  -- -- ** Utility
  -- , xyAxis
  -- , xyCoords
  -- , showUnitX
  -- , showDirection
  -- , showPoint
  -- , showBoundaries
  -- , showEnvelope
  -- , smokeBackground
  --
  -- -- * Rendering drawings
  -- , OriginPlacement(..)
  -- , RenderingOptions(..)
  -- -- mempty
  -- , toSvg
  -- , toSvgStr
  -- , toSvgAny
  --
  -- -- ** High-performance
  -- , RDrawing
  -- , renderDrawing
  -- , emitDrawing
  -- )
where

import Control.Applicative
import Data.Colour (Colour, AlphaColour, withOpacity)
import Data.Map.Strict(Map)
import Data.Monoid
import Data.Semigroup(Max(..))
import qualified Data.Colour
import qualified Data.Colour.Names as Colors
import qualified Data.Colour.SRGB
import qualified Data.List
import qualified Data.Ord
import qualified Data.Map.Strict as Map
import qualified Data.String
import qualified Data.Sequence as Seq
import Data.Sequence(Seq)
import Data.Foldable(toList)

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

import qualified Data.List.Split
import Data.String (IsString(..))

import qualified Text.XML.Light
import qualified Text.XML.Light as X

#if MIN_VERSION_linear(1,20,0)
#else
import Linear.Epsilon
#endif

import Lubeck.Str
import Lubeck.Drawing.Types

#ifdef __GHCJS__
import GHCJS.Types(JSVal, JSString)
import Web.VirtualDom.Svg (Svg)
import qualified Web.VirtualDom as VD
import qualified Web.VirtualDom.Svg as E
import qualified Web.VirtualDom.Svg.Attributes as A
#endif

import System.IO.Unsafe(unsafePerformIO)


{-| -}
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
instance (Floating a
#if MIN_VERSION_linear(1,20,0)
#else
  , Epsilon a
#endif
  )
  => Fractional (Transformation a) where
  recip (TF x) = TF (inv33_ x)
  fromRational = error "Missing in Fractional (Transformation a)"

#if MIN_VERSION_linear(1,20,0)
inv33_ = inv33
#else
inv33_ m = case inv33 m of
  Nothing -> m
  Just mi -> mi
#endif

-- | a.k.a. @1@
emptyTransformation :: Num a => Transformation a
emptyTransformation = TF identity

-- | a.k.a '*', 'mappend', '<>'
apTransformation :: Num a => Transformation a -> Transformation a -> Transformation a
apTransformation (TF x) (TF y) = TF (x !*! y)

-- | a.k.a 'recip'
negTransformation :: (Num a, Floating a
#if MIN_VERSION_linear(1,20,0)
#else
  , Epsilon a
#endif
  ) => Transformation a -> Transformation a
negTransformation (TF x) = TF (inv33_ x)

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

{-| -}
transformationToMatrix :: Num a => Transformation a -> (a, a, a, a, a, a)
transformationToMatrix (TF (V3 (V3 a c e) (V3 b d f) (V3 _ _ _))) = (a,b,c,d,e,f)
{-# INLINABLE transformationToMatrix #-}

transformVector :: Num a => Transformation a -> V2 a -> V2 a
transformVector t (V2 x y) =
  -- let (a,b,c,d,e,f) = transformationToMatrix t
  let (TF (V3 (V3 a c e) (V3 b d f) (V3 _ _ _))) = t
  in V2 (a*x+c*y) (b*x+d*y)
{-# INLINABLE transformVector #-}

transformPoint :: Num a => Transformation a -> P2 a -> P2 a
transformPoint t (P (V2 x y)) =
  -- let (a,b,c,d,e,f) = transformationToMatrix t
  let (TF (V3 (V3 a c e) (V3 b d f) (V3 _ _ _))) = t
  in P $ V2 (a*x+c*y+e) (b*x+d*y+f)
{-# INLINABLE transformPoint #-}


-- | Linear component of a transformation.
lin :: Num a => Transformation a -> Transformation a
lin t =
  -- let (a,b,c,d,e,f) = transformationToMatrix t
  let (TF (V3 (V3 a c e) (V3 b d f) (V3 _ _ _))) = t
  in matrix (a,b,c,d,0,0)


-- | Linear component of a transformation (transposed).
transp :: Num a => Transformation a -> Transformation a
transp t =
  -- let (a,b,c,d,e,f) = transformationToMatrix t
  let (TF (V3 (V3 a c e) (V3 b d f) (V3 _ _ _))) = t
  in matrix (a,c,b,d,0,0)

-- | Translation component of a transformation.
transl :: Num a => Transformation a -> V2 a
transl t =
  -- let (a,b,c,d,e,f) = transformationToMatrix t
  let (TF (V3 (V3 a c e) (V3 b d f) (V3 _ _ _))) = t
  in V2 e f

-- TODO cleanup definitions/names here
