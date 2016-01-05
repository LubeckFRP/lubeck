
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, OverloadedStrings, NamedFieldPuns #-}

module Lubeck.Drawing where

import GHCJS.Types(JSString)
import Data.Monoid
import Control.Applicative
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point hiding (Point)
import Data.Colour (Colour)
import qualified Data.Colour
import qualified Data.JSString
import Data.JSString.Text (textFromJSString)


import Data.Map(Map)
import qualified Data.Map
import qualified Data.List

-- TODO remove
import Data.Time.Calendar (Day)
-- import Time

import Numeric.Interval (Interval)

-- TODO svg, html nodes

  {-

-- All Floats below are in (0 < x < 1), cartesian relative plot size
{-| -}
type alias GrowthPlot = {
    xName  : String,
    xScale : List (Float, String),
    yName  : String,
    yScale : List (Float, String),
    data   : List (String, Color, List (Float, Float)) -- (x,y)
  }


{-| -}
examplePlot : GrowthPlot
examplePlot =
  {
    xName = "A", yName = "B", xScale = [], yScale = [], data = []
  }
-}


{-| A point in 2D space. -}
data Point = Point { x :: Float, y :: Float }

{-| A vector (distance between two points) in 2D space. -}
data Vector = Vector { dx :: Float, dy :: Float }

instance AdditiveGroup Vector where
instance VectorSpace Vector where
instance AffineSpace Point where
  type Diff Point = Vector

{-

{-| Add a point and a vector. -}
(.+^) : Point -> Vector -> Point
(.+^) a b = { x  = a.x  + b.dx, y  = a.y  + b.dy }

{-| Calculate the distance between two points. -}
(.-.) : Point -> Point -> Vector
(.-.) a b = { dx = a.x  - b.x,  dy = a.y - b.y }

{-| -}
zeroV : Vector
zeroV = Vector 0 0

{-| -}
(^+^) : Vector -> Vector -> Vector
(^+^) a b = { dx = a.dx + b.dx, dy = a.dy + b.dy }

{-| -}
negateV : Vector -> Vector
negateV a = { dx = negate a.dx, dy = negate a.dy }

{-| Multiply a vector by a scalar. -}
(*^) : Float -> Vector -> Vector
(*^) a b = { dx = a*b.dx, dy = a*b.dy }
-}

offsetVectors :: Point -> [Vector] -> [Point]
offsetVectors = error "TODO"
-- offsetVectors p = let
--   unsafeTail xs = case xs of
--     []      -> error "offsetVectors"
--     (x:xs) -> xs
--   offsetPoints = Data.List.scanl (flip (.+^))
--   in unsafeTail . offsetPoints p

betweenPoints :: [Point] -> [Vector]
betweenPoints = error "TODO"
-- betweenPoints xs = case Data.List.tail xs of
--   Nothing -> []
--   Just ys -> liftA2 (.-.) ys xs

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
type Transformation =
    (Float,Float,
     Float,Float,
     Float,Float)

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
emptyTransformation = (1,0,0,1,0,0)

{-| -}
apTransformation :: Transformation -> Transformation -> Transformation
apTransformation (a1,b1,c1,d1,e1,f1) (a2,b2,c2,d2,e2,f2) =
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
transformationToMatrix x = x

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
styleToAttrString = Data.Map.foldlWithKey (\n v rest -> n <> ":" <> v <> "; " <> rest) ""

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

-- transformEnvelope =

-- TODO polygon/polyline support
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

```elm
transform (rotation x)   image = rotate x image
transform (stretching x) image = stretch x image
```

Composing transformations has the same effect as carrying out the
transformation one at a time:

```elm
transform s (transform t image) = transform (s <> t) image
```
 -}
transform :: Transformation -> Drawing -> Drawing
transform = Transf


{-| Translate (move) an image. -}
translate :: Vector -> Drawing -> Drawing
translate (Vector { dx, dy }) = transform (1,0,0,1,dx,dy)

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
scaleXY     x y = transform (x,0,0,y,0,0)

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
rotate    a   = transform (cos a, 0 - sin a, sin a, cos a, 0, 0)
-- The b,c, signs are inverted because of the reverse y polarity.

{-| Shear an image. -}
shearXY :: Float -> Float -> Drawing -> Drawing
shearXY   a b = transform (1, b, a, 1, 0, 0)


{-

{-| A smoke-colored background big enough to fill the whole screen.

Useful to see the boundary of the canvas, as in:

```elm
  (fillColor "red" square) `over` smokeBackground
```
-}
smokeBackground : Drawing
smokeBackground = fillColor "whitesmoke" $ scale 200000 $ square

{-| Draw the X and Y axis (their intersection is the origin). -}
xyAxis : Drawing
xyAxis = strokeColor "darkgreen" $ strokeWidth 0.5 $ scale 20000 $ stack [horizontalLine, verticalLine]

{-| Apply a style to a drawing. -}
style : Style -> Drawing -> Drawing
style = Style

{-| -}
fillColor : String -> Drawing -> Drawing
fillColor x = style (Dict.singleton "fill" x)

{-| -}
strokeColor : String -> Drawing -> Drawing
strokeColor x = style (Dict.singleton "stroke" x)

{-| -}
strokeWidth : Float -> Drawing -> Drawing
strokeWidth x = style (styleNamed "stroke-width" (toString x ++ "px"))


pointToSvgString : Point -> String
pointToSvgString {x,y} = toString x ++ "," ++ toString y

pointsToSvgString : List Point -> String
pointsToSvgString ps = String.fromList $ List.concat $ List.intersperse (String.toList " ") $ List.map (String.toList << pointToSvgString) ps

toSvg1 : Drawing -> List Svg
toSvg1 x = let
    single x = [x]
    noScale = Html.Attributes.attribute "vector-effect" "non-scaling-stroke"
    x_ = Svg.Attributes.x
    y_ = Svg.Attributes.y
    negY (a,b,c,d,e,f) = (a,b,c,d,e,negate f)

    offsetVectorsWithOrigin p vs = p :: offsetVectors p vs
    reflY a = { dx = a.dx, dy = negate a.dy }

  in case x of
  Circle     -> single $ Svg.circle [r "0.5", noScale] []
  Rect       -> single $ Svg.rect [x_ "-0.5", y_ "-0.5", width "1", height "1", noScale] []
  Line       -> single $ Svg.line [x1 "0", y1 "0", x2 "1", y2 "0", noScale] []
  Lines closed vs -> if closed
    then single $ Svg.polygon [Svg.Attributes.points (pointsToSvgString $ offsetVectorsWithOrigin {x=0,y=0} (List.map reflY vs)), noScale] []
    else single $ Svg.polyline [Svg.Attributes.points (pointsToSvgString $ offsetVectorsWithOrigin {x=0,y=0} (List.map reflY vs)), noScale] []

  Text s     -> single $ text' [x_ "0", y_ "0"] [Svg.text s]

  Transf t x -> single $ g [Svg.Attributes.transform $ "matrix" ++ toString (negY t) ++ ""] (toSvg1 x)
  Style s x  -> single $ g [Svg.Attributes.style $ styleToAttrString s] (toSvg1 x)

  Em         -> single $ g [] []
  Ap x y     -> single $ g [] (toSvg1 x ++ toSvg1 y)


{-| -}
type OrigoPlacement = TopLeft | BottomLeft | Center
{-| -}
type alias RenderingOptions =  { dimensions : Point, origoPlacement : OrigoPlacement }

{-| -}
toSvg : RenderingOptions -> Drawing -> Svg
toSvg opts drawing =
    let
      dimensions = opts.dimensions
      placeOrigo = case opts.origoPlacement of
        TopLeft     -> \x -> x
        Center      -> translateX (dimensions.x/2) >> translateY (dimensions.y/(-2))
        BottomLeft  -> translateY (dimensions.y*(-1))
    in
    svg [
      width  $ toString dimensions.x,
      height $ toString dimensions.y,
      viewBox $ "0 0 " ++ toString dimensions.x ++ " " ++ toString dimensions.y] $
        toSvg1 $ placeOrigo $ drawing

-- TODO move
flipV = scaleY (-1)
zeroP = { x = 0, y = 0 }
(^*) = flip (*^)

-- TODO move
for : number -> (number -> a) -> List a
for n f = List.map f [0..n-1]

(&) : Float -> Float -> Point
(&) x y = { x = x, y = y }

(^) : Float -> Float -> Vector
(^) x y = { dx = x, dy = y }

scalePoint : Vector -> Point -> Point
scalePoint {dx,dy} {x,y} = Point (dx*x) (dy*y)

scaleVector : Vector -> Vector -> Vector
scaleVector a b = Vector (a.dx * b.dx) (a.dy * b.dy)




-- PLOTTING (TODO Move)

{-|
  A way to draw normalized data.

  The data is assumed to be points in some affine space, normalized so that the data to be plotted is
  in the range (0<x<1). Data outside this range is not necessarily drawn.

  Currently, this always maps
    o     -> o
    (1,1) -> (600,300)

  I.e the boundaries of the actual plotted data is (0,0) (600,300).
-}
type alias Plot a = a -> Drawing

plotPoints : { color : Maybe String } -> Plot (List Point)
plotPoints opts xs = let
    circleColor = Maybe.withDefault "red" opts.color
  in stack $ List.map (\p -> translate (scaleVector (600^300) (p .-. zeroP)) $ (scale 5 $ fillColor circleColor circle)) $ xs

plotGrowth : { color : Maybe String } -> Plot (List Point)
plotGrowth opts xs = let
    lineColor = Maybe.withDefault "blue" opts.color
    init = case List.head xs of
      Nothing -> zeroV -- OK?
      Just p  -> p .-. zeroP
  in
  translate (scaleVector (600^300) init) $ style (styleNamed "fill-opacity" (toString 0)) $ strokeColor lineColor $ strokeWidth 1.5 $ segments $
  List.map (scaleVector (600^300)) $ betweenPoints xs


{-|
  A way to normalize data of type a into the expected range (0<x<1) of type b, see above, and to provide an axis of type c.
-}
type alias Fit a b c = a -> (b, c)

{-| -}
type alias DataPlot a b c = {
  name     : String,
  data     : a,
  fit      : Fit a b c,
  plot     : Plot b,
  axisPlot : Plot c
  }


-- Should not be given []
fitSq : List Point -> Point -> Point
fitSq ps p = let
  mx = Maybe.withDefault 0 $ List.minimum $ List.map .x ps
  my = Maybe.withDefault 0 $ List.minimum $ List.map .y ps
  nx = Maybe.withDefault 0 $ List.maximum $ List.map .x ps
  ny = Maybe.withDefault 0 $ List.maximum $ List.map .y ps
  sq = {dx=1/(nx-mx), dy=1/(ny-my)}
  in scalePoint sq $ p .+^ {dx=-mx, dy=-my}

-- basicAdPlot : DataPlot (List (Date.Date, Float)) (List Point) ()
-- basicAdPlot = uf

{-| -}
drawDataPlot : DataPlot a b c -> Drawing
drawDataPlot dp = let
  (data,axis) = dp.fit dp.data
  in (dp.plot data `over` dp.axisPlot axis)

{-| -}
basicDataGrowth : (a -> Point) -> List a -> DataPlot (List a) (List Point) ()
basicDataGrowth f x = let
    ps = List.map f x

  in {
  name = "",
  data = x,
  fit xs = (List.map (fitSq ps) ps, ()),
  plot = plotGrowth { color = Nothing },
  axisPlot _ = transparent
  }

{-| -}
plotDrawingToSvg : Drawing -> Svg
plotDrawingToSvg x = toSvg { origoPlacement = BottomLeft, dimensions = { x = 640, y = 340 } } $ translate (20^20) x


{-| -}
plotTest : GrowthPlot -> Svg
plotTest _ = toSvg { origoPlacement = BottomLeft, dimensions = { x = 640, y = 340 } } $ translate (20^20) $ scale 1 $

      -- (scale 50 $ fillColor "blue" $ style (styleNamed "fill-opacity" (toString 0.5)) $ polygon [Vector 1 0, Vector (-0.5) 0.8660254037844386, Vector (-0.5) (-0.8660254037844386)])
        -- `over`
      -- (strokeColor "darkred" $ strokeWidth 1.5 $ translateY 10 $ rotate (turn/12) $ scaleX 800 $ horizontalLine)
        -- `over`
      (plotPoints {color=Just "red"}
        (List.take 200 $ List.map (scalePoint $ (1/10)^(1/10)) $ List.map2 Point [0..10] [0..10])
        )
        `over`
      (plotPoints {color=Just "yellow"}
        (List.take 200 $ List.map ((\x-> x .+^ (0.1^0.05)) << scalePoint ((1/10)^(1/10))) $ List.map2 Point [0..10] [0..10])
        )

        `over`
      (plotGrowth {color=Just "blue"}
        (List.take 200 $ List.map (scalePoint $ (1/10)^(1/10)) $ List.map2 Point [0..12] [6,3,10,5,4,2,1,2.2,3,-2,100000,1000,0])
        )
        `over`
      (plotGrowth {color=Just "darkblue"}
        [1&1,0&0,0&0.5,0.85&0.7,0.65&0.35]
        )
        `over`
      (plotGrowth {color=Just "pink"}
        [0.0&1.0, 0.1&0.1, 0.22&0.2, 0.3&0.32]
        )
        -- [
        -- {dx=10  , dy=10},
        -- {dx=3   , dy=-5},
        -- {dx=13  , dy=5},
        -- {dx=3   , dy=-5},
        -- {dx=10  , dy=1},
        -- {dx=10  , dy=-10},
        -- {dx=50  , dy=0},
        -- {dx=20  , dy=21},
        -- {dx=3   , dy=-5},
        -- {dx=3   , dy=-5},
        -- {dx=3   , dy=-7}
        -- ]
        `over`
      -- let plot = (translateX (-200) $ scale 4 $ style (styleNamed "fill-opacity" (toString 0.5)) $ fillColor "lightblue" $ strokeColor "blue" $ strokeWidth 1.5 $ Lines False
      --       [
      --         {dx=10  , dy=10},
      --         {dx=10  , dy=1},
      --         {dx=3   , dy=-5},
      --         {dx=13  , dy=5},
      --         {dx=10  , dy=-10},
      --         {dx=3   , dy=-5},
      --         {dx=20  , dy=21},
      --         {dx=3   , dy=-5},
      --         {dx=50  , dy=0},
      --         {dx=3   , dy=-5},
      --         {dx=3   , dy=-7}
      --       ])
      --   in (stack [plot, scale 1.1 plot, scale 1.3 plot])
      --   `over`
      xyAxis
        `over`
      smokeBackground
        -- (translateX 1 $ Style "fill: red" Circle)


{- Plotting conventions:
  All data points/values are received unformatted. Each axis has a definition with sensible defaults that tells us
    - The bounds of the axis (min,max).
    - The type of scale (only linear for now)
    - Where the labels go.
  The axis and data give rise to a number of points inside the square (0,0) to (1,1).
    This is scaled to the requested dimensions of the plotting area (not necessarily a square).
  Axes defs are used to generate legend (if necessary).
  Axes are generated from their defs and the plotting area.
  Data plotting:
    Points:   List Point
    Lines:    LinearEquation
    Growth:   List Point
      (show as line segments, with or without area)



 -}

-}
