
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, TypeFamilies, OverloadedStrings,
  NamedFieldPuns, NoMonomorphismRestriction, BangPatterns, StandaloneDeriving
  , ScopedTypeVariables, NoImplicitPrelude #-}

-- | Basic types and combinators.
-- More "geometry" than "types".
-- Lubeck.Drawing.Transformation depends on this.
module Lubeck.Drawing.Types
where

import BasePrelude hiding (Handler, rotate, (|||), mask)
import Data.Colour(Colour, AlphaColour, withOpacity)
import Control.Lens (Lens, Lens', (^.))
import Control.Monad.Writer
import Control.Monad.State
import Data.Functor.Identity
import qualified Data.Colour
import qualified Data.Colour.Names as Colors
import qualified Data.Colour.SRGB
import qualified Data.List
import qualified Data.Ord
import qualified Data.Map.Strict as Map
import qualified Data.String
import qualified Data.Sequence as Seq
import qualified Data.List.Split
import qualified Text.XML.Light
import qualified Text.XML.Light as X

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

type P1 a = Point V1 a
type P2 a = Point V2 a
type P3 a = Point V3 a
type P4 a = Point V4 a



{-| Where to place origo in the generated SVG. -}
data OriginPlacement
  = TopLeft
  | BottomLeft
  | Center
  deriving (Eq, Ord, Show)

{-| Specifies how to generate an SVG from a Drawing. -}
data RenderingOptions = RenderingOptions
  { dimensions          :: !(P2 Double)
  -- ^ Dimensions. Describes a rectangle from (0,0) to the given point (x,y).
  -- Default is @800 x 800@..
  , originPlacement     :: !OriginPlacement
  -- ^ Where to place origo in the generated image.
  -- Default is @Center@.
  , renderSpecialStyles :: !Bool
  -- ^ When true, render special styles (currently only gradients), which could make
  -- rendering slower. Default is @False@.
  }
  deriving (Eq, Ord, Show)

-- | Left-biased. Mainly here for the 'mempty'.
instance Monoid RenderingOptions where
  mempty  = RenderingOptions (P $ V2 800 800) Center False
  mappend = const


-- Ideomatically: (V2 Double), (P2 Double) and so on
-- |
-- @
-- offsetVectors :: Num a => P2 a -> [V2 a] -> [P2 a]
-- @
offsetVectors :: (Num a, Affine p) => p a -> [Diff p a] -> [p a]
offsetVectors p = tail . offsetVectors' p
  where
    offsetVectors' = Data.List.scanl (.+^)

-- |
-- @
-- betweenPoints :: Num a => [P2 a] -> [V2 a]
-- @
betweenPoints :: (Num a, Affine p) => [p a] -> [Diff p a]
betweenPoints xs = case xs of
  []     -> []
  (_:ys) -> zipWith (.-.) ys xs

-- | An angle in 2D space.
--
-- The value [turn](#turn) is used to represent a full rotation, so a half-turn
-- can be expressed as `turn/2`, three quarters of a turn by `turn*3/4` and so on.
--
-- To convert to radians or degrees, use
newtype Angle n = Radians n
  deriving (Functor, Enum, Eq, Ord, Num, Fractional, Floating)

instance Show a => Show (Angle a) where
  show (Radians x) = show x

instance Applicative Angle where
  pure = Radians
  {-# INLINABLE pure #-}
  Radians f <*> Radians x = Radians (f x)
  {-# INLINABLE (<*>) #-}

instance Additive Angle where
  zero = pure 0
  {-# INLINABLE zero #-}

instance Num n => Monoid (Angle n) where
  mappend = (^+^)
  mempty  = Radians 0

{-| The value representing a full turn.
This can be expressed in radians as τ (or 2π), or in degrees as 360°. -}
turn :: Floating a => a
turn = pi * 2

{-| Convert an angle to turns. -}
angleToTurns :: Floating a => Angle a -> a
angleToTurns (Radians x) = x / turn

{-| Convert an angle to radians. -}
angleToRadians :: Floating a => Angle a -> a
angleToRadians (Radians x) = x

{-| Convert an angle to degrees. -}
angleToDegrees :: Floating a => Angle a -> a
angleToDegrees (Radians x) = let tau = pi * 2 in (x / tau * 360)

acosA :: Floating n => n -> Angle n
acosA = pure . acos

-- Ideomatically: Direction V2 Double
newtype Direction v a = Direction (v a)
  deriving (Eq, Ord, Show)

dir :: v n -> Direction v n
dir = Direction

-- | @fromDirection d@ is the unit vector in the direction @d@.
fromDirection :: (Metric v, Floating n) => Direction v n -> v n
fromDirection (Direction v) = signorm v

angleBetween :: (Metric v, Floating n) => v n -> v n -> Angle n
angleBetween v1 v2 = acosA (signorm v1 `dot` signorm v2)

angleBetweenDirections :: (Metric v, Floating n) => Direction v n -> Direction v n -> Angle n
angleBetweenDirections x y = acosA $ (fromDirection x) `dot` (fromDirection y)

{-
Turn two radii to scale and offset in polar coordinates, clockwise direction.
-}
anglesToPolarScaleOffset
  :: Angle Double
  -> Angle Double
  -> (Double, Double)
anglesToPolarScaleOffset a1 a2 = (s, o )
  where
    o = mod' (1 - angleToTurns a2) 1
    s = mod' (angleToTurns a2 - angleToTurns a1) 1


{-|
A rectangle, represented as two points.

Access the corners as
@
let r = Rect_ (P (V2 1 2) (3 4))

r^.p1.x -- left
r^.p2.x -- right
r^.p1.y -- bottom
r^.p2.y -- top
@
-}
data Rect a = Rect_ { _p1 :: P2 a, _p2 :: P2 a }
  deriving (Eq, Ord, Show, Functor)

rect :: a -> a -> a -> a -> Rect a
rect x1 y1 x2 y2 = Rect_ (P (V2 x1 y1)) (P (V2 x2 y2))

-- makeLenses ''Rect
p1 :: Lens' (Rect a) (P2 a)
p1 f (Rect_ p1 p2) = fmap (\p1' -> Rect_ p1' p2) $ f p1

p2 :: Lens' (Rect a) (P2 a)
p2 f (Rect_ p1 p2) = fmap (\p2' -> Rect_ p1 p2') $ f p2

_left, _right, _bottom, _top :: Lens' (Rect a) a
_left    = p1._x
_right   = p2._x
_bottom  = p1._y
_top     = p2._y

width :: Num a => Rect a -> a
width r = r^._right - r^._left

height :: Num a => Rect a -> a
height r = r^._top - r^._bottom


-- | Vector of length one, pointing to the left.
unitX :: Num a => V2 a
unitX = V2 1 0

-- | Vector of length one, pointing upwards.
unitY :: Num a => V2 a
unitY = V2 0 1

-- | Vector of length one, pointing diagonally upwards and left.
posDiagonal :: Floating a => V2 a
posDiagonal = V2 (sqrt 2) (sqrt 2)

-- | Vector of length one, pointing diagonally downwards and left.
negDiagonal :: Floating a => V2 a
negDiagonal = V2 (sqrt 2) (-sqrt 2)





{-|
A line segment.

The 1-dimensional analogue of @Rect@.
-}
data LineSeg a = LineSeg { _lp1 :: P1 a, _lp2 :: P1 a }
  deriving (Eq, Ord, Show, Functor)

lineseg :: a -> a -> LineSeg a
lineseg x1 x2 = LineSeg (P (V1 x1)) (P (V1 x2))
