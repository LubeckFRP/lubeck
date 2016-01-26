
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, OverloadedStrings, NamedFieldPuns, QuasiQuotes, CPP, NoMonomorphismRestriction #-}

{-|

High-level graphics library with an SVG backend.

Essentially a stripped-down version of Diagrams:

[diagrams]: http://projects.haskell.org/diagrams

/Experimental/

-}
module Lubeck.Drawing (
    -- ** Basics
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
    Transformation,
    emptyTransformation,
    apTransformation,
    (!<>),
    transformationToMatrix,

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
    shearXY,

    -- ** Styling
    Style,
    emptyStyle,
    styleNamed,
    apStyle,
    style,
    fillColor,
    strokeColor,
    strokeWidth,
    -- *** Rendering
    styleToAttrString,

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
    text,
    -- ** Combination
    over,
    stack,
    -- ** Utility
    xyAxis,
    smokeBackground,

    -- * Render
    OrigoPlacement(..),
    RenderingOptions(..),
    toSvg,

    -- * Debug
    drawTest,
  ) where

import Data.Monoid
import Control.Applicative
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point hiding (Point)
import Data.Colour (Colour)
import qualified Data.Colour
import qualified Data.Colour.SRGB
import qualified Data.String
import qualified Data.JSString
import Data.Map(Map)
import qualified Data.Map
import qualified Data.List
import qualified Data.Colour.Names as C

#ifdef __GHCJS__
import GHCJS.Types(JSString)
import Data.JSString.Text (textFromJSString)

import qualified Web.VirtualDom as VD
import Web.VirtualDom.Svg (Svg)
-- import Web.VirtualDom.Svg (p, h1, div, form, button, img, hr, custom, table, td, tr, th, tbody, thead)
-- import Web.VirtualDom.Svg.Events (click, change, submit, stopPropagation, preventDefault, value)
-- import Web.VirtualDom.Svg.Attributes (src, width, class_)
import qualified Web.VirtualDom.Svg as E
import qualified Web.VirtualDom.Svg.Attributes as A

-- TODO consolidate (see below)
-- import GHCJS.VDOM.Unsafe (unsafeToAttributes, Attributes')
import GHCJS.Foreign.QQ (js, jsu, jsu')

#else
type JSString = String
#endif

-- TODO remove
import Data.Time.Calendar (Day)
-- import Time

import Numeric.Interval (Interval)

-- TODO svg, html nodes



{-| A point in 2D space. -}
data Point = Point { x :: Float, y :: Float }
  deriving (Eq, Ord, Show)

{-| A vector (distance between two points) in 2D space. -}
data Vector = Vector { dx :: Float, dy :: Float }
  deriving (Eq, Ord, Show)

instance AdditiveGroup Vector where
  zeroV   = Vector 0 0
  negateV (Vector x y) = Vector (negate x) (negate y)
  Vector xa ya ^+^ Vector xb yb = Vector (xa + xb) (ya + yb)

instance VectorSpace Vector where
  type Scalar Vector = Float
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
  (_:ys) -> liftA2 (.-.) ys xs

-- distanceVs : Point -> List Point -> List Vector
-- distanceVs p = tail . pointOffsets p



{-| Name of dimensions in 2 and 3D space. -}
data Dimension = X | Y | Z

{-| An angle in 2D space.

The value [turn](#turn) is used to represent a full rotation, so a half-turn
can be expressed as `turn/2`, three quarters of a turn by `turn*3/4` and so on.

To convert to radians or degrees, use

 -}
type Angle = Float

{-| The value representing a full turn.
This can be expressed in radians as τ (or 2π), or in degrees as 360°. -}
turn :: Angle
turn = pi * 2

{-| Convert an angle to radians. -}
angleToRadians :: Angle -> Float
angleToRadians x = x

{-| Convert an angle to degrees. -}
angleToDegrees :: Angle -> Float
angleToDegrees x = let tau = pi * 2 in (x / tau * 360)

{-| -}
newtype Transformation = Transformation { getTransformation ::
    (Float,Float,
     Float,Float,
     Float,Float)
     }

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

infixr 6 !<>

{-| Compose two transformations. -}
(!<>) :: Transformation -> Transformation -> Transformation
(!<>) = apTransformation

{-| -}
transformationToMatrix :: Transformation -> (Float, Float, Float, Float, Float, Float)
transformationToMatrix = getTransformation

{-| -}
type Style = Map JSString JSString

{-| -}
emptyStyle :: Style
emptyStyle = Data.Map.empty

{-| -}
styleNamed :: JSString -> JSString -> Style
styleNamed = Data.Map.singleton

{-| -}
apStyle :: Style -> Style -> Style
apStyle = Data.Map.union

{-| -}
styleToAttrString :: Style -> JSString
styleToAttrString = Data.Map.foldrWithKey (\n v rest -> n <> ":" <> v <> "; " <> rest) ""

{-|
  A drawing is an infinite two-dimensional image, which supports arbitrary scaling transparency.

  Because the image is infinite, basic images have simple proportions, for example [circle](#circle), [square](#square)
  and [horizontalLine](#horizontalLine) all have a width of one. To obtain other sizes, use the [transform](#transform) or [scale](#scale) functions.

  Every image has a notion of a local origin, or "midpoint". Transformations such as scaling, rotation and
  reflection are carried out with respect to the local origin. By default, most shapes are centered around the
  local origin (or to put it differently: the local origin is the midpoint of the image). To move an image, use
  the [translate](#translate) functions.

  Images can be composed using [over](#over) and [stack](#stack), which overlays the two images so that their origins match exactly.
-}
type Drawing = DrawingBase

type Envelope = Maybe (Vector -> Float)
-- Max monoid
-- Transform by inverse-transforming argument and transforming (scaling) result
-- Transformable

-- TODO path support (generalizes all others! including text?)
-- TODO masks
-- TODO better font support
data DrawingBase
  = Circle
  | Rect
  | Line -- conceptually a line from point a to point b
  | Lines Bool [Vector] -- sequence of straight lines, closed or not. For closed lines, there is no need to return the original point (i.e. the sum of the vector does not have to be zeroV).

  | Text JSString
  | Transf Transformation Drawing
  | Style Style Drawing
  | Em
  | Ap Drawing Drawing
  -- deriving (Eq, Ord)

instance Monoid DrawingBase where
  mempty = transparent
  mappend = over

{-| An empty and transparent drawing.
    Identity for [over](#over) and [stack](#stack). -}
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

{-| -}
segments :: [Vector] -> Drawing
segments = Lines False

{-| -}
polygon :: [Vector] -> Drawing
polygon = Lines True

{-| -}
text :: JSString -> Drawing
text = Text

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


{-| Translate (move) an image. -}
translate :: Vector -> Drawing -> Drawing
translate (Vector { dx, dy }) = transform $ Transformation (1,0,0,1,dx,dy)

{-| Translate (move) an image along the horizonal axis.
A positive argument will move the image to the right. -}
translateX :: Float -> Drawing -> Drawing
translateX x = translate (Vector x 0)

{-| Translate (move) an image along the vertical axis.
A positive argument will move the image upwards (as opposed to standard SVG behavior). -}
translateY :: Float -> Drawing -> Drawing
translateY y = translate (Vector 0 y)

{-| Scale (stretch) an image. -}
scaleXY :: Float -> Float -> Drawing -> Drawing
scaleXY     x y = transform $ Transformation (x,0,0,y,0,0)

{-| Scale (stretch) an image, preserving its horizontal/vertical proportion. -}
scale :: Float -> Drawing -> Drawing
scale    x   = scaleXY x x

{-| Scale (stretch) an image horizontally. -}
scaleX :: Float -> Drawing -> Drawing
scaleX    x   = scaleXY x 1

{-| Scale (stretch) an image vertically. -}
scaleY :: Float -> Drawing -> Drawing
scaleY      y = scaleXY 1 y

{-| Rotate an image. A positive vale will result in a counterclockwise rotation and negative value in a clockwise rotation. -}
rotate :: Angle -> Drawing -> Drawing
rotate    a   = transform $ Transformation (cos a, 0 - sin a, sin a, cos a, 0, 0)
-- The b,c, signs are inverted because of the reverse y polarity.

{-| Shear an image. -}
shearXY :: Float -> Float -> Drawing -> Drawing
shearXY   a b = transform $ Transformation (1, b, a, 1, 0, 0)



-- {-| A smoke-colored background big enough to fill the whole screen.
--
-- Useful to see the boundary of the canvas, as in:
--
-- ```elm
--   (fillColor "red" square) `over` smokeBackground
-- ```
-- -}
smokeBackground :: Drawing
smokeBackground = fillColor C.whitesmoke $ scale 50 $ square
--
{-| Draw the X and Y axis (their intersection is the origin). -}
xyAxis :: Drawing
xyAxis = strokeColor C.darkgreen $ strokeWidth 0.5 $ scale 50 $ stack [horizontalLine, verticalLine]

{-| Apply a style to a drawing. -}
style :: Style -> Drawing -> Drawing
style = Style

{-| -}
fillColor :: Colour Double -> Drawing -> Drawing
fillColor x = style (Data.Map.singleton "fill" $ showColor x)

{-| -}
strokeColor :: Colour Double -> Drawing -> Drawing
strokeColor x = style (Data.Map.singleton "stroke" $ showColor x)

showColor = Data.JSString.pack . Data.Colour.SRGB.sRGB24show
{-| -}
strokeWidth :: Float -> Drawing -> Drawing
strokeWidth x = style (styleNamed "stroke-width" (showJS x <> "px"))
  where
-- TODO move
showJS = Data.JSString.pack . show

-- TODO internal
pointsToSvgString :: [Point] -> JSString
pointsToSvgString ps = toJSString $ mconcat $ Data.List.intersperse " " $ Data.List.map pointToSvgString ps
  where
    toJSString = Data.JSString.pack
    pointToSvgString (Point {x,y}) = show x ++ "," ++ show y


svgNamespace = Data.Map.fromList[("namespace","http://www.w3.org/2000/svg")]
                -- ("xmlns","http://www.w3.org/2000/svg"),

toSvg1 :: Drawing -> [Svg]
toSvg1 x = let
    single x = [x]
    noScale = VD.attribute "vector-effect" "non-scaling-stroke"
    negY (a,b,c,d,e,f) = (a,b,c,d,e,negate f)
    offsetVectorsWithOrigin p vs = p : offsetVectors p vs
    reflY (Vector adx ady) = Vector { dx = adx, dy = negate ady }
  in case x of
      Circle     -> single $ E.circle
        [A.r "0.5", noScale]
        []
      Rect       -> single $ E.rect
        [A.x "-0.5", A.y "-0.5", A.width "1", A.height "1", noScale]
        []
      Line -> single $ E.line
        [A.x1 "0", A.x1 "0", A.x2 "1", A.y2 "0", noScale]
        []
      (Lines closed vs) -> single $ (if closed then E.polygon else E.polyline)
        [A.points (pointsToSvgString $ offsetVectorsWithOrigin (Point 0 0) (fmap reflY vs)), noScale]
        []
      Text s -> single $ E.text' [A.x "0", A.y "0"] [E.text s]
      Transf (Transformation t) x -> single $ E.g
        [A.transform $ "matrix" <> showJS (negY t) <> ""]
        (toSvg1 x)
      Style s x  -> single $ E.g
        [A.style $ styleToAttrString s]
        (toSvg1 x)
      Em         -> single $ E.g [] []
      Ap x y     -> single $ E.g [] (toSvg1 x ++ toSvg1 y)


{-| -}
data OrigoPlacement = TopLeft | BottomLeft | Center
  deriving (Eq, Ord, Show)
{-| -}
data RenderingOptions = RenderingOptions { dimensions :: Point, origoPlacement :: OrigoPlacement }
  deriving (Eq, Ord, Show)

{-| -}
toSvg :: RenderingOptions -> Drawing -> Svg
toSvg (RenderingOptions {dimensions,origoPlacement}) drawing =
      svg'
        (showJS $ floor x)
        (showJS $ floor y)
        ("0 0 " <> showJS (floor x) <> " " <> showJS (floor y))
        (toSvg1 $ placeOrigo $ drawing)
  where
    Point {x,y} = dimensions

    placeOrigo  = case origoPlacement of
      TopLeft     -> id
      Center      -> translateX (x/2) . translateY (y/(-2))
      BottomLeft  -> translateY (y*(-1))

    svg' :: JSString -> JSString -> JSString -> [Svg] -> Svg
    svg' w h vb = E.svg
      [ A.width w
      , A.height h
      , A.viewBox vb ]

drawTest :: Int -> Svg
drawTest n = toSvg (RenderingOptions (Point 500 500) Center)
  $ rotate ((turn/13)*fromIntegral n)
  $ translateX ((100/13)*fromIntegral n)
  $ scale 100 $ (strokeColor C.blue . fillColor C.red) circle <> scaleX 2 (fillColor C.green circle) -- <> xyAxis <> smokeBackground
  --  $ scale 1.1 $ (scale 200 $ fillColor C.blue circle) <> (scale 250 $ fillColor C.red square) <> smokeBackground
