
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, TypeFamilies, OverloadedStrings,
  NamedFieldPuns, QuasiQuotes, CPP, NoMonomorphismRestriction #-}

{-|

High-level vector graphics library. Renders to as SVG using "Web.VirtualDom.Svg".

The API is a rather stripped-down version of Diagrams.

Similar to diagrams:

* Based on "linear", points, vectors and transformations

* Has monoidal overlay of transparent images, local origins and envelpoes

Main differences from Diagrams:

* Limited to 2D

* No Core.Measure

* No Semigroups (?)

* No Default (use a left-biased monoid, i.e. mappend = const)

* No names, no queries

* No transformable/special attributes, everything is showable/string

  ** Consequently (?)

  ** No local or normalized units (behaves as Diagrams' global units)

* traces?

* No HasOrigin class (just use tranformation)


* Supports (tentatively) text envelopes, see http://bl.ocks.org/nitaku/8745933

* Potential animation supports

* Event handler support (VDOM only)

[diagrams]: http://projects.haskell.org/diagrams

-}
module Lubeck.Drawing (
    -- * Creating drawings
    -- ** Geometry
    Point(..),
    V1(..), V2(..), V3(..), V4(..),
    P1, P2, P3, P4,

    Angle,
    acosA,
    angleBetween,
    turn,
    angleToRadians,
    angleToDegrees,
    -- TODO move/rename these?
    offsetVectors, betweenPoints,

    Direction,
    dir,
    fromDirection,
    angleBetween,
    transformDirection,
    angleBetweenDirections,

    -- ** Transformations
    Transformation,
    negTransformation,
    transformVector,
    transformPoint,
    transformationToMatrix,
    -- ** Raw transformations
    translation,
    translationX,
    translationY,
    scaling,
    scalingX,
    scalingY,
    rotation,
    shearing,
    matrix,
    -- ** Applying transformations
    transform,
    translate,
    translateX,
    translateY,
    scaleXY,
    scale,
    scaleX,
    scaleY,
    rotate,
    shear,

    -- ** Styling
    Style,
    styleNamed,
    fillColor,
    fillColorA,
    strokeColor,
    strokeColorA,
    strokeWidth,
    -- *** Rendering styles
    styleToAttrString,
    -- *** Applying styles
    style,


    -- ** Events
    addProperty,

    -- ** Envelopes/Alignment/Juxtaposition
    Envelope,
    envelope,
    -- transformEnvelope,
    unitX,
    unitY,
    (|||),
    (===),
    juxtapose,


    -- ** Drawings
    Drawing,
    -- ** Basic drawings
    transparent,
    circle,
    square,
    horizontalLine,
    verticalLine,
    segments,
    polygon,
    -- ** Text
    text,
    textMiddle,
    textEnd,
    TextAnchor(..),
    TextOptions(..),
    textWithOptions,

    -- ** Utility
    xyAxis,
    xyCoords,
    showUnitX,
    showDirection,
    showDirection2,
    showPoint,
    showEnvelope,
    smokeBackground,

    -- * Rendering drawings
    OriginPlacement(..),
    RenderingOptions(..),
    -- mempty,
    toSvg,
  ) where

import Control.Applicative
-- import Data.AffineSpace
-- import Data.AffineSpace.Point hiding (Point)
import Data.Colour (Colour, AlphaColour, withOpacity)
import Data.Map(Map)
import Data.Monoid
import Data.Semigroup(Max(..))
-- import Data.VectorSpace
import qualified Data.Colour
import qualified Data.Colour.Names as C
import qualified Data.Colour.SRGB
import qualified Data.JSString
import qualified Data.List
import qualified Data.Map
import qualified Data.String

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

#ifdef __GHCJS__
import GHCJS.Types(JSString)
import qualified Web.VirtualDom as VD
import Web.VirtualDom.Svg (Svg)
import qualified Web.VirtualDom.Svg as E
import qualified Web.VirtualDom.Svg.Attributes as A
#else
type JSString = String
#endif

import Lubeck.Util(showJS)


{- A point in 2D space. -}
-- type Point =
-- data Point = Point { x :: Double, y :: Double }
  -- deriving (Eq, Ord, Show)

{- A vector (distance between two points) in 2D space. -}
-- data Vector = Vector { dx :: Double, dy :: Double }
  -- deriving (Eq, Ord, Show)

-- type R = R1
-- type V = V1

-- Ideomatically: (V2 Double), (P2 Double) and so on
-- type V1 a = Linear.V1.V1 a
-- type V2 a = Linear.V2.V2 a
-- type V3 a = Linear.V3.V3 a
-- type V4 a = Linear.V4.V4 a

type P1 a = Point V1 a
type P2 a = Point V2 a
type P3 a = Point V3 a
type P4 a = Point V4 a

-- type R = Double
-- type V = Double
-- type R2 = Point
-- type V2 = Vector
-- type R3 = (R,R,R)
-- type V3 = (R,R,R)
-- type R4 = (R,R,R,R)
-- type V4 = (R,R,R,R)
-- type R5 = (R,R,R,R,R)
-- type V5 = (R,R,R,R,R)

-- instance AdditiveGroup Vector where
--   zeroV   = Vector 0 0
--   negateV (Vector x y) = Vector (negate x) (negate y)
--   Vector xa ya ^+^ Vector xb yb = Vector (xa + xb) (ya + yb)
--
-- instance VectorSpace Vector where
--   type Scalar Vector = Double
--   a *^ Vector x y = Vector (a*x) (a*y)
--
-- instance AffineSpace Point where
--   type Diff Point = Vector
--   Point xa ya .+^ Vector xb yb = Point  (xa + xb) (ya + yb)
--   Point xa ya .-. Point  xb yb = Vector (xa - xb) (ya - yb)

-- TODO generalize types for offsetVectors and betweenPoints

offsetVectors :: Num a => P2 a -> [V2 a] -> [P2 a]
offsetVectors p = tail . offsetVectors' p
  where
    offsetVectors' = Data.List.scanl (.+^)

betweenPoints :: Num a => [P2 a] -> [V2 a]
betweenPoints xs = case xs of
  []     -> []
  (_:ys) -> zipWith (.-.) ys xs

-- distanceVs : Point -> List Point -> List Vector
-- distanceVs p = tail . pointOffsets p



{-| Name of dimensions in 2 and 3D space. -}
-- data Dimension = X | Y | Z

{-| An angle in 2D space.

The value [turn](#turn) is used to represent a full rotation, so a half-turn
can be expressed as `turn/2`, three quarters of a turn by `turn*3/4` and so on.

To convert to radians or degrees, use

 -}
newtype Angle n = Radians n
  deriving (Functor, Enum, Eq, Ord, Num, Fractional)

instance Applicative Angle where
  pure = Radians
  {-# INLINE pure #-}
  Radians f <*> Radians x = Radians (f x)
  {-# INLINE (<*>) #-}

instance Additive Angle where
  zero = pure 0
  {-# INLINE zero #-}

instance Num n => Monoid (Angle n) where
  mappend = (^+^)
  mempty  = Radians 0

-- TODO wrap

{-| The value representing a full turn.
This can be expressed in radians as τ (or 2π), or in degrees as 360°. -}
turn :: Floating a => Angle a
turn = pure $ pi * 2

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

dir :: v n -> Direction v n
dir = Direction

-- | @fromDirection d@ is the unit vector in the direction @d@.
fromDirection :: (Metric v, Floating n) => Direction v n -> v n
fromDirection (Direction v) = signorm v

angleBetween :: (Metric v, Floating n) => v n -> v n -> Angle n
angleBetween v1 v2 = acosA (signorm v1 `dot` signorm v2)

transformDirection :: Num n => Transformation n -> Direction V2 n -> Direction V2 n
transformDirection t (Direction v) = Direction (transformVector t v)

-- TODO do we really need angleBetween?
angleBetweenDirections :: (Metric v, Floating n) => Direction v n -> Direction v n -> Angle n
angleBetweenDirections x y = acosA $ (fromDirection x) `dot` (fromDirection y)


--- Eventually use
--    identity :: (Num a, Traversable t, Applicative t) => t (t a)




{-| -}
newtype Transformation a = TF { getTF :: M33 a }
-- newtype Transformation a = TF { getTF ::
--     (a,a,
--      a,a,
--      a,a)
--      }
instance Num a => Monoid (Transformation a) where
  mempty  = emptyTransformation
  mappend = apTransformation
-- TODO change to use M33 (including the 0 0 1 row)

emptyTransformation :: Num a => Transformation a
emptyTransformation = TF identity
-- emptyTransformation = TF (1,0,0,1,0,0)

{-| -}
apTransformation :: Num a => Transformation a -> Transformation a -> Transformation a
apTransformation (TF x) (TF y) = TF (x !*! y)
-- apTransformation
--   (TF (a1,b1,c1,d1,e1,f1))
--   (TF (a2,b2,c2,d2,e2,f2)) =
--     TF
--       (a1*a2 + c1*b2,
--        b1*a2 + d1*b2,
--        a1*c2 + c1*d2,
--        b1*c2 + d1*d2,
--        a1*e2 + c1*f2 + e1,
--        b1*e2 + d1*f2 + f1)

negTransformation :: (Num a, Floating a) => Transformation a -> Transformation a
negTransformation (TF x) = TF (inv33 x)



-- Both of these use same layout as SVG, see https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform
--
-- That is
--   a c e
-- ( b d f )
--   0 0 1
--
-- I.e. the identity is (1,0,0,1,0,0) and the translation component is (0,0,0,0,x,y)
--
-- This is column-major order with an implied extra row (0 0 1)

{-| -}
matrix :: Num a => (a, a, a, a, a, a) -> Transformation a
-- matrix = TF
matrix (a,b,c,d,e,f) = TF $ V3 (V3 a c e) (V3 b d f) (V3 0 0 1)

{-| -}
transformationToMatrix :: Num a => Transformation a -> (a, a, a, a, a, a)
-- transformationToMatrix = getTF
transformationToMatrix (TF (V3 (V3 a c e) (V3 b d f) (V3 _ _ _))) = (a,b,c,d,e,f)
transformationToMatrix _ = error "transformationToMatrix: Bad transformation"



transformVector :: Num a => Transformation a -> V2 a -> V2 a
transformVector t (V2 x y) =
  let (a,b,c,d,e,f) = transformationToMatrix t
  in V2 (a*x+c*y) (b*x+d*y)

transformPoint :: Num a => Transformation a -> P2 a -> P2 a
transformPoint t (P (V2 x y)) =
  let (a,b,c,d,e,f) = transformationToMatrix t
  in P $ V2 (a*x+c*y+e) (b*x+d*y+f)

{-| -}
newtype Style = S { getS :: Map JSString JSString }
  deriving (Monoid)
-- TODO newtype wrapper
-- Monoid

{-| -}
emptyStyle :: Style
emptyStyle = S $ Data.Map.empty

{-| -}
styleNamed :: JSString -> JSString -> Style
styleNamed k v = S $ Data.Map.singleton k v

{-| -}
apStyle :: Style -> Style -> Style
apStyle (S a) (S b) = S $ Data.Map.union a b

{-| -}
styleToAttrString :: Style -> JSString
styleToAttrString = Data.Map.foldrWithKey (\n v rest -> n <> ":" <> v <> "; " <> rest) "" . getS

{-| Embed an SVG property on a drawing.
    Intended to be used with the event handlers in "Web.VirtualDom.Svg.Events".
    -}
addProperty :: E.Property -> Drawing -> Drawing
addProperty = Prop



-- newtype Envelope v n = Envelope (Maybe (v n -> Max n))
  -- deriving (Monoid)

newtype Envelope v n = Envelope (Maybe (Direction v n -> Point v n))


instance (Foldable v, Additive v, Floating n, Ord n) => Monoid (Envelope v n) where
  mempty      = Envelope Nothing
  mappend (Envelope x) (Envelope y) = case (x, y) of
    (Nothing, g)       -> Envelope y
    (f,       Nothing) -> Envelope x
    (Just f,  Just g)  -> Envelope $ Just $ maxEnv f g
    _                  -> Envelope Nothing
    -- Invoke max explicitly, as Data.Monoid.Max has a superflous Bounded constraint
    -- Alternatively, we could escape this by using the semigroup version
    where
      -- maxEnv :: Floating n => (Direction v n -> Point v n) -> (Direction v n -> Point v n) -> Direction v n -> Point v n
      maxEnv f g = \r -> let
        p1 = f r
        p2 = g r
        in if (distanceA p1 origin) > (distanceA p2 origin)
          then p1 else p2

-- transformEnvelope :: (Num a, Floating a) => Transformation a -> Envelope V2 a -> Envelope V2 a
-- transformEnvelope t (Envelope (Just f)) = Envelope $ Just (f . transformVector (negTransformation t))
-- transformEnvelope _  _                  = Envelope Nothing

juxtapose :: V2 Double -> Drawing -> Drawing -> Drawing
juxtapose v a b = b -- TODO


  -- case (envelope a, envelope b) of
  -- (Envelope (Just f), Envelope (Just g)) ->

-- juxtapose v a b = case (envelope a, envelope b) of
--   -- FIXME negate this translation
--   (Envelope (Just ae), Envelope (Just be))  ->
--     let t = translate
--                                           (negated $ (negated v ^* getMax (be (negated v)))
--                                             ^-^
--                                           (v ^* getMax (ae v)))
--                                           in t b
--   _                                         -> b

envelope :: Drawing -> Envelope V2 Double
envelope x = case x of
  -- FIXME use (0.5/norm v), not (1/norm v)
  Circle        -> Envelope $ Just $ \dir -> (P $ fromDirection dir ^* 0.5)
  Rect          -> envelope Circle -- TODO
  Line          -> envelope Circle -- TODO
  Lines _ _     -> envelope Circle -- TODO
  Text _        -> envelope Circle -- TODO
  -- Transf t x    -> transformEnvelope t (envelope x)
  Transf t x    ->
    case envelope x of
      Envelope Nothing  -> Envelope $ Nothing
      Envelope (Just f) -> Envelope $ Just $
        \r -> let
          r2 = transformDirection (negTransformation t) r
          in transformPoint t (f r2)

  Style _ x     -> envelope x
  Prop  _ x     -> envelope x
  Em            -> Envelope $ Nothing
  Ap x y        -> mappend (envelope x) (envelope y)






unitX = V2 1 0
unitY = V2 0 1

a ||| b = a <> juxtapose unitX a b
a === b = a <> juxtapose (negated unitY) a b


-- moveOriginTo (origin .^+ v) === translate (negated v)
-- moveOriginTo (origin .^+ v) === translate (negated v)

-- transformEnvelope t = moveOriginTo (P . negated . transl $ t) . onEnvelope g
--   where
--     -- XXX add lots of comments explaining this!
--     g f v = f v' / (v' `dot` vi)
--       where
--         v' = signorm $ lapp (transp t) v
--         vi = apply (inv t) v

-- Max monoid
-- Transform by inverse-transforming argument and transforming (scaling) result
-- Transformable

-- TODO path support (generalizes all others! including text?)
-- TODO masks
-- TODO better font support

{-|
  A drawing is an infinite two-dimensional image, which supports arbitrary scaling transparency.

  Because the image is infinite, basic images have simple proportions, for example 'circle', 'square',
  and 'horizontalLine' all have a width of one. To obtain other sizes, use the 'transform' or 'scale' functions.

  Every image has a notion of a local origin, or "midpoint". Transformations such as scaling, rotation and
  reflection are carried out with respect to the local origin. By default, most shapes are centered around the
  local origin (or to put it differently: the local origin is the midpoint of the image). To move an image, use
  the 'translate' functions.

  Images can be composed using the 'Monoid' instance, which overlays the two images so that their origins match exactly.
-}
data Drawing
  = Circle
  | Rect
  | Line -- conceptually a line from point a to point b
  | Lines Bool [V2 Double]  -- sequence of straight lines, closed or not. For closed lines, there is no need to return
                            -- the original point (i.e. the sum of the vectors does not have to be zeroV).

  | Text JSString

  | Transf (Transformation Double) Drawing
  | Style Style Drawing
  -- Embed arbitrary SVG property (typically used for event handlers)
  | Prop E.Property Drawing

  | Em
  | Ap Drawing Drawing

instance Monoid Drawing where
  mempty  = transparent
  mappend = over

{-| An empty and transparent drawing. Same as 'mempty'. -}
transparent :: Drawing
transparent      = Em

{-| A centered circle with radius one. -}
circle :: Drawing
circle    = Circle

{-| A centered square with a width and height of one. -}
square :: Drawing
square = Rect

{-| A centered horizontal line of length one. -}
horizontalLine :: Drawing
horizontalLine = translateX (-0.5) Line

{-| A centered vertical line of length one. -}
verticalLine :: Drawing
verticalLine = rotate (turn/4) horizontalLine

{-| Draw a sequence of line segments. -}
segments :: [V2 Double] -> Drawing
segments = Lines False

{-| Draw a polygon. -}
polygon :: [V2 Double] -> Drawing
polygon = Lines True

{-| Draw text. See also 'textWithOptions'. -}
text :: JSString -> Drawing
text = Text

textMiddle = textWithOptions (defaultTextOptions { textAnchor = TextMiddle })

textEnd = textWithOptions (defaultTextOptions { textAnchor = TextEnd })

data TextAnchor
  = TextStart
  | TextMiddle
  | TextEnd
  deriving (Eq, Ord, Read, Show)

data TextOptions = TextOptions
  { textAnchor :: TextAnchor
  }
defaultTextOptions = TextOptions
  TextStart

-- | Left-biased. Mainly here for the 'mempty'.
instance Monoid TextOptions where
  mempty  = defaultTextOptions
  mappend = const

{-| -}
textWithOptions :: TextOptions -> JSString -> Drawing
textWithOptions opts = ta . Text
  where
    ta = case textAnchor opts of
      TextStart   -> Prop (VD.attribute "text-anchor" "start")
      TextMiddle  -> Prop (VD.attribute "text-anchor" "middle")
      TextEnd     -> Prop (VD.attribute "text-anchor" "end")

{-| Layer the two images so that their origins match precisely. The origin of the given
    images become the origin of the new image as well.

The order of the arguments matter: the first image is placed closer to the viewer than
the second, so all areas in the first image that are not transparent will cover the
corresponding area in the second image.
    -}
over :: Drawing -> Drawing -> Drawing
over = flip Ap

{-| Like [over](#over), but with an arbitrary number of images. -}
stack :: [Drawing] -> Drawing
stack = Data.List.foldr over transparent

{-| Apply a [Transformation](#Transformation) to an image.

This is the most general way to transform an image. Most of the functions
below are more convenient to use, but can not be used to transform arbitrary
objects.

@
transform (rotation x)   image = rotate x image
transform (stretching x) image = stretch x image
@

Composing transformations has the same effect as carrying out the
transformation one at a time:

@
transform s (transform t image) = transform (s <> t) image
@
 -}
transform :: Transformation Double -> Drawing -> Drawing
transform = Transf

{-| Translates (move) an object. -}
translation a b = matrix (1,0,0,1,a,b)

{-| Translates (move) an object along the horizonal axis.
A positive argument will move the object to the right. -}
translationX a  = translation a 0

{-| Translates (move) an object along the vertical axis.
A positive argument will move the object upwards (as opposed to standard SVG behavior). -}
translationY b  = translation 0 b

{-| Scales (stretch) an object, preserving its horizontal/vertical proportion. -}
scaling a b     = matrix (a,0,0,b,0,0)

{-| Scales(stretch) an object. -}
scalingX a      = scaling     a 1

{-| Scales (stretch) an object. -}
scalingY b      = scaling     1 b

{-| Rotates an object. A positive vale will result in a counterclockwise rotation and negative value in a clockwise rotation. -}
rotation a      = matrix (cos a, 0 - sin a, sin a, cos a, 0, 0)

{-| Shears an object. -}
shearing a b    = matrix (1, b, a, 1, 0, 0)



{-| Translate (move) an image. -}
translate :: V2 Double -> Drawing -> Drawing
translate (V2 dx dy) = transform $ matrix (1,0,0,1,dx,dy)

{-| Translate (move) an image along the horizonal axis.
A positive argument will move the image to the right. -}
translateX :: Double -> Drawing -> Drawing
translateX x = translate (V2 x 0)

{-| Translate (move) an image along the vertical axis.
A positive argument will move the image upwards (as opposed to standard SVG behavior). -}
translateY :: Double -> Drawing -> Drawing
translateY y = translate (V2 0 y)

{-| Scale (stretch) an image. -}
scaleXY :: Double -> Double -> Drawing -> Drawing
scaleXY     x y = transform $ matrix (x,0,0,y,0,0)

{-| Scale (stretch) an image, preserving its horizontal/vertical proportion. -}
scale :: Double -> Drawing -> Drawing
scale    x   = scaleXY x x

{-| Scale (stretch) an image horizontally. -}
scaleX :: Double -> Drawing -> Drawing
scaleX    x   = scaleXY x 1

{-| Scale (stretch) an image vertically. -}
scaleY :: Double -> Drawing -> Drawing
scaleY      y = scaleXY 1 y

{-| Rotate an image. A positive vale will result in a counterclockwise rotation and negative value in a clockwise rotation. -}
rotate :: Angle Double -> Drawing -> Drawing
rotate (Radians a) = transform $ matrix (cos a, 0 - sin a, sin a, cos a, 0, 0)
-- The b,c, signs are inverted because of the reverse y polarity.

{-| Shear an image. -}
shear :: Double -> Double -> Drawing -> Drawing
shear   a b = transform $ matrix (1, b, a, 1, 0, 0)



{-| A "big" smoke-colored background.

Useful to see the boundary of the canvas, as in:

@
  (fillColor "red" square) `over` smokeBackground
@
-}
smokeBackground :: Drawing
smokeBackground = fillColor C.whitesmoke $ scale 5000 $ square
--
{-| Draw the X and Y axis inside the unit square (their intersection is the origin). -}
xyAxis :: Drawing
xyAxis = strokeColor C.darkgreen $ strokeWidth 0.5 $
  stack [horizontalLine, verticalLine]

{-| Draw the X and Y axis inside the unit square, unit circle and unit square. -}
xyCoords :: Drawing
xyCoords = fillColorA (C.black `withOpacity` 0) $ strokeColor C.darkgreen $
  strokeWidth 0.5 $ stack [horizontalLine, verticalLine, circle, square]

showUnitX :: Drawing
showUnitX = strokeColor C.red $ strokeWidth 3 $ translateX 0.5 horizontalLine

-- showDirection :: Direction V2 Double -> Drawing
-- showDirection dir = scale 1000 $ rot showUnitX
--   where
--     rot = rotate (angleBetweenDirections dir (Direction unitX))
showDirection = showDirection2

showDirection2 :: Direction V2 Double -> Drawing
showDirection2 dir = scale 100 $ strokeColor C.red $ strokeWidth 3 $ segments [fromDirection dir]

showPoint p = translate (p .-. origin) base
  where
    base = strokeColor C.red $ fillColorA (C.black `withOpacity` 0) $strokeWidth 2 $ scale 1 $ circle

showEnvelope dir x = case envelope x of
  Envelope Nothing  -> x
  Envelope (Just f) -> showDirection dir <> showPoint (f dir) <> x

{-| Apply a style to a drawing. -}
style :: Style -> Drawing -> Drawing
style = Style

{-| -}
fillColor :: Colour Double -> Drawing -> Drawing
fillColor x = style (styleNamed "fill" $ showColor x)

{-| -}
strokeColor :: Colour Double -> Drawing -> Drawing
strokeColor x = style (styleNamed "stroke" $ showColor x)

showColor = Data.JSString.pack . Data.Colour.SRGB.sRGB24show

{-| -}
fillColorA :: AlphaColour Double -> Drawing -> Drawing
fillColorA x = fillColor c . alpha a
  where
    alpha a = style (styleNamed "fill-opacity" $ showJS a)
    c = Data.Colour.over x C.black
    a = Data.Colour.alphaChannel x

-- {-| -}
strokeColorA :: AlphaColour Double -> Drawing -> Drawing
strokeColorA x = strokeColor c . alpha a
  where
    alpha a = style (styleNamed "stroke-opacity" $ showJS a)
    c = Data.Colour.over x C.black
    a = Data.Colour.alphaChannel x

{-| Set the stroke width. By default stroke is /not/ affected by scaling or other transformations.

    TODO this can be overriden by setting the non-scaling-stroke attribute. Wrap in nice API?
-}
strokeWidth :: Double -> Drawing -> Drawing
strokeWidth x = style (styleNamed "stroke-width" (showJS x <> "px"))


{-| Where to place origo in the generated SVG. -}
data OriginPlacement
  = TopLeft
  | BottomLeft
  | Center
  deriving (Eq, Ord, Show)

{-| Specifies how to generate an SVG from a Drawing. -}
data RenderingOptions = RenderingOptions
  -- TODO dimensions here should really be some kind of rectangle type
  { dimensions      :: P2 Double                -- ^ Dimensions. Describes a rectangle from (0,0) to the given point (x,y).
  , originPlacement :: OriginPlacement          -- ^ Where to place origo in the generated image.
  }
  deriving (Eq, Ord, Show)

-- | Left-biased. Mainly here for the 'mempty'.
instance Monoid RenderingOptions where
  mempty  = RenderingOptions (P $ V2 800 800) Center
  mappend = const

{-| Generate an SVG from a drawing. -}
toSvg :: RenderingOptions -> Drawing -> Svg
toSvg (RenderingOptions {dimensions, originPlacement}) drawing =
  svgTopNode
    (showJS $ floor x)
    (showJS $ floor y)
    ("0 0 " <> showJS (floor x) <> " " <> showJS (floor y))
    (toSvg1 [] $ placeOrigo $ drawing)
  where
    P (V2 x y) = dimensions

    svgTopNode :: JSString -> JSString -> JSString -> [Svg] -> Svg
    svgTopNode w h vb = E.svg
      [ A.width w
      , A.height h
      , A.viewBox vb ]

    placeOrigo :: Drawing -> Drawing
    placeOrigo = case originPlacement of
      TopLeft     -> id
      Center      -> translateX (x/2) . translateY (y/(-2))
      BottomLeft  -> translateY (y*(-1))

    pointsToSvgString :: [P2 Double] -> JSString
    pointsToSvgString ps = toJSString $ mconcat $ Data.List.intersperse " " $ Data.List.map pointToSvgString ps
      where
        toJSString = Data.JSString.pack
        pointToSvgString (P (V2 x y)) = show x ++ "," ++ show y

    toSvg1 :: [E.Property] -> Drawing -> [Svg]
    toSvg1 ps x = let
        single x = [x]
        noScale = VD.attribute "vector-effect" "non-scaling-stroke"
        negY (a,b,c,d,e,f) = (a,b,c,d,e,negate f)
        offsetVectorsWithOrigin p vs = p : offsetVectors p vs
        reflY (V2 adx ady) = V2 adx (negate ady)
      in case x of
          Circle     -> single $ E.circle
            ([A.r "0.5", noScale]++ps)
            []
          Rect       -> single $ E.rect
            ([A.x "-0.5", A.y "-0.5", A.width "1", A.height "1", noScale]++ps)
            []
          Line -> single $ E.line
            ([A.x1 "0", A.x1 "0", A.x2 "1", A.y2 "0", noScale]++ps)
            []
          (Lines closed vs) -> single $ (if closed then E.polygon else E.polyline)
            ([A.points (pointsToSvgString $ offsetVectorsWithOrigin (P $ V2 0 0) (fmap reflY vs)), noScale]++ps)
            []
          Text s -> single $ E.text'
            ([A.x "0", A.y "0"]++ps)
            [E.text s]

          -- Don't render properties applied to Transf/Style on the g node, propagate to lower level instead
          -- As long as it is just event handlers, it doesn't matter
          Transf t x -> single $ E.g
            [A.transform $ "matrix" <> showJS (negY $ transformationToMatrix t) <> ""]
            (toSvg1 ps x)
          Style s x  -> single $ E.g
            [A.style $ styleToAttrString s]
            (toSvg1 ps x)
          Prop p x   -> toSvg1 (p:ps) x
          -- No point in embedding handlers to empty groups, but render anyway
          Em         -> single $ E.g ps []
          -- Event handlers applied to a group go on the g node
          Ap x y     -> single $ E.g ps (toSvg1 [] x ++ toSvg1 [] y)
