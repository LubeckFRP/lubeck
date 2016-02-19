
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, OverloadedStrings, NamedFieldPuns, QuasiQuotes, CPP, NoMonomorphismRestriction #-}

{-|

High-level vector graphics library. Renders to as SVG using "Web.VirtualDom.Svg".

The API is a rather stripped-down version of Diagrams.

[diagrams]: http://projects.haskell.org/diagrams

-}
module Lubeck.Drawing (
    -- ** Basics
    R,
    V,
    R2,
    V2,
    R3,
    V3,
    R4,
    V4,
    Point(..),
    Vector(..),
    Dimension(..),
    Angle,
    offsetVectors,
    betweenPoints,
    turn,
    angleToRadians,
    angleToDegrees,

    -- ** Transformations
    Transformation(..), -- TODO hide internals
    -- emptyTransformation,
    -- apTransformation,
    transformVector,
    transformPoint,
    -- (!<>),
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
    -- emptyStyle,
    styleNamed,
    apStyle,
    style,
    fillColor,
    fillColorA,
    strokeColor,
    strokeColorA,
    strokeWidth,
    -- *** Rendering
    styleToAttrString,

    -- ** Events
    addProperty,

    -- ** Envelopes
    Envelope,
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
    -- ** Combination
    -- over,
    -- mconcat,
    -- ** Utility
    xyAxis,
    xyCoords,
    smokeBackground,

    -- * Render
    OrigoPlacement(..),
    RenderingOptions(..),
    defaultRenderingOptions,
    toSvg,
  ) where

import Control.Applicative
import Data.AffineSpace
import Data.AffineSpace.Point hiding (Point)
import Data.Colour (Colour, AlphaColour, withOpacity)
import Data.Map(Map)
import Data.Monoid
import Data.Semigroup(Max(..))
import Data.VectorSpace
import qualified Data.Colour
import qualified Data.Colour.Names as C
import qualified Data.Colour.SRGB
import qualified Data.JSString
import qualified Data.List
import qualified Data.Map
import qualified Data.String

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


{-| A point in 2D space. -}
data Point = Point { x :: Double, y :: Double }
  deriving (Eq, Ord, Show)

{-| A vector (distance between two points) in 2D space. -}
data Vector = Vector { dx :: Double, dy :: Double }
  deriving (Eq, Ord, Show)

type R = Double
type V = Double
type R2 = Point
type V2 = Vector
type R3 = (Double,Double,Double)
type V3 = (Double,Double,Double)
type R4 = (Double,Double,Double,Double)
type V4 = (Double,Double,Double,Double)

instance AdditiveGroup Vector where
  zeroV   = Vector 0 0
  negateV (Vector x y) = Vector (negate x) (negate y)
  Vector xa ya ^+^ Vector xb yb = Vector (xa + xb) (ya + yb)

instance VectorSpace Vector where
  type Scalar Vector = Double
  a *^ Vector x y = Vector (a*x) (a*y)

instance AffineSpace Point where
  type Diff Point = Vector
  Point xa ya .+^ Vector xb yb = Point  (xa + xb) (ya + yb)
  Point xa ya .-. Point  xb yb = Vector (xa - xb) (ya - yb)

offsetVectors :: Point -> [Vector] -> [Point]
offsetVectors p = tail . offsetVectors' p
  where
    offsetVectors' = Data.List.scanl (.+^)

betweenPoints :: [Point] -> [Vector]
betweenPoints xs = case xs of
  []     -> []
  (_:ys) -> zipWith (.-.) ys xs

-- distanceVs : Point -> List Point -> List Vector
-- distanceVs p = tail . pointOffsets p



{-| Name of dimensions in 2 and 3D space. -}
data Dimension = X | Y | Z

{-| An angle in 2D space.

The value [turn](#turn) is used to represent a full rotation, so a half-turn
can be expressed as `turn/2`, three quarters of a turn by `turn*3/4` and so on.

To convert to radians or degrees, use

 -}
type Angle = Double

{-| The value representing a full turn.
This can be expressed in radians as τ (or 2π), or in degrees as 360°. -}
turn :: Angle
turn = pi * 2

{-| Convert an angle to radians. -}
angleToRadians :: Angle -> Double
angleToRadians x = x

{-| Convert an angle to degrees. -}
angleToDegrees :: Angle -> Double
angleToDegrees x = let tau = pi * 2 in (x / tau * 360)

{-| -}
newtype Transformation = Transformation { getTransformation ::
    (Double,Double,
     Double,Double,
     Double,Double)
     }
instance Monoid Transformation where
  mempty  = emptyTransformation
  mappend = apTransformation

-- We use same layout as SVG, see https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform
--
-- That is
--   a c e
-- ( b d f )
--   0 0 1
--
-- I.e. the identity is (1,0,0,1,0,0) and the translation component is (0,0,0,0,x,y)


-- TODO
{-| -}
emptyTransformation :: Transformation
emptyTransformation = Transformation (1,0,0,1,0,0)

{-| -}
apTransformation :: Transformation -> Transformation -> Transformation
apTransformation
  (Transformation (a1,b1,c1,d1,e1,f1))
  (Transformation (a2,b2,c2,d2,e2,f2)) =
    Transformation
      (a1*a2 + c1*b2,
       b1*a2 + d1*b2,
       a1*c2 + c1*d2,
       b1*c2 + d1*d2,
       a1*e2 + c1*f2 + e1,
       b1*e2 + d1*f2 + f1)

-- infixr 6 !<>

transformVector :: Transformation -> Vector -> Vector
transformVector (Transformation (a,b,c,d,e,f)) (Vector x y) = Vector (a*x+c*y) (b*x+d*y)

transformPoint :: Transformation -> Point -> Point
transformPoint (Transformation (a,b,c,d,e,f)) (Point x y) = Point (a*x+c*y+e) (b*x+d*y+f)

-- {-| Compose two transformations. -}
-- (!<>) :: Transformation -> Transformation -> Transformation
-- (!<>) = apTransformation

{-| -}
transformationToMatrix :: Transformation -> (Double, Double, Double, Double, Double, Double)
transformationToMatrix = getTransformation

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

-- | Defines how far an object extends in any direction.
--   @Nothing@ means the object has no extent (i.e. the empty image).
newtype Extent = Extent { getExtent  :: (Maybe Double) }
  deriving (Eq, Ord)
instance Bounded Extent where
  minBound = Extent $ Nothing
  maxBound = Extent $ Just (1/0)

newtype Envelope = Envelope (Vector -> Max Extent)
  deriving (Monoid)
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
  | Lines Bool [Vector] -- sequence of straight lines, closed or not. For closed lines, there is no need to return the original point (i.e. the sum of the vector does not have to be zeroV).

  | Text JSString
  | Transf Transformation Drawing
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
segments :: [Vector] -> Drawing
segments = Lines False

{-| Draw a polygon. -}
polygon :: [Vector] -> Drawing
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
transform :: Transformation -> Drawing -> Drawing
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

matrix = Transformation


{-| Translate (move) an image. -}
translate :: Vector -> Drawing -> Drawing
translate (Vector { dx, dy }) = transform $ matrix (1,0,0,1,dx,dy)

{-| Translate (move) an image along the horizonal axis.
A positive argument will move the image to the right. -}
translateX :: Double -> Drawing -> Drawing
translateX x = translate (Vector x 0)

{-| Translate (move) an image along the vertical axis.
A positive argument will move the image upwards (as opposed to standard SVG behavior). -}
translateY :: Double -> Drawing -> Drawing
translateY y = translate (Vector 0 y)

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
rotate :: Angle -> Drawing -> Drawing
rotate    a   = transform $ matrix (cos a, 0 - sin a, sin a, cos a, 0, 0)
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
data OrigoPlacement
  = TopLeft
  | BottomLeft
  | Center
  deriving (Eq, Ord, Show)

{-| Specifies how to generate an SVG from a Drawing. -}
data RenderingOptions = RenderingOptions
  { dimensions     :: Point                   -- ^ Dimensions. Describes a rectangle from (0,0) to the given point (x,y).
  , origoPlacement :: OrigoPlacement          -- ^ Where to place origo in the generated image.
  }
  deriving (Eq, Ord, Show)

defaultRenderingOptions :: RenderingOptions
defaultRenderingOptions = RenderingOptions (Point 800 800) Center

{-| Generate an SVG from a drawing. -}
toSvg :: RenderingOptions -> Drawing -> Svg
toSvg (RenderingOptions {dimensions, origoPlacement}) drawing =
  svgTopNode
    (showJS $ floor x)
    (showJS $ floor y)
    ("0 0 " <> showJS (floor x) <> " " <> showJS (floor y))
    (toSvg1 [] $ placeOrigo $ drawing)
  where
    Point {x,y} = dimensions

    svgTopNode :: JSString -> JSString -> JSString -> [Svg] -> Svg
    svgTopNode w h vb = E.svg
      [ A.width w
      , A.height h
      , A.viewBox vb ]

    placeOrigo :: Drawing -> Drawing
    placeOrigo = case origoPlacement of
      TopLeft     -> id
      Center      -> translateX (x/2) . translateY (y/(-2))
      BottomLeft  -> translateY (y*(-1))

    pointsToSvgString :: [Point] -> JSString
    pointsToSvgString ps = toJSString $ mconcat $ Data.List.intersperse " " $ Data.List.map pointToSvgString ps
      where
        toJSString = Data.JSString.pack
        pointToSvgString (Point {x,y}) = show x ++ "," ++ show y

    toSvg1 :: [E.Property] -> Drawing -> [Svg]
    toSvg1 ps x = let
        single x = [x]
        noScale = VD.attribute "vector-effect" "non-scaling-stroke"
        negY (a,b,c,d,e,f) = (a,b,c,d,e,negate f)
        offsetVectorsWithOrigin p vs = p : offsetVectors p vs
        reflY (Vector adx ady) = Vector { dx = adx, dy = negate ady }
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
            ([A.points (pointsToSvgString $ offsetVectorsWithOrigin (Point 0 0) (fmap reflY vs)), noScale]++ps)
            []
          Text s -> single $ E.text'
            ([A.x "0", A.y "0"]++ps)
            [E.text s]

          -- Don't render properties applied to Transf/Style on the g node, propagate to lower level instead
          -- As long as it is just event handlers, it doesn't matter
          Transf (Transformation t) x -> single $ E.g
            [A.transform $ "matrix" <> showJS (negY t) <> ""]
            (toSvg1 ps x)
          Style s x  -> single $ E.g
            [A.style $ styleToAttrString s]
            (toSvg1 ps x)
          Prop p x   -> toSvg1 (p:ps) x
          -- No point in embedding handlers to empty groups, but render anyway
          Em         -> single $ E.g ps []
          -- Event handlers applied to a group go on the g node
          Ap x y     -> single $ E.g ps (toSvg1 [] x ++ toSvg1 [] y)
