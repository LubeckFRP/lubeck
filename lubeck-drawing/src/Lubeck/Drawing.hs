
{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveFunctor
  , TypeFamilies
  , OverloadedStrings
  , NamedFieldPuns
  , CPP
  , BangPatterns
  , ScopedTypeVariables
  , NoImplicitPrelude
  , NoMonomorphismRestriction
  , TypeSynonymInstances
  , FlexibleContexts
  #-}

{-# OPTIONS_GHC
  -fwarn-incomplete-patterns
  -fno-warn-name-shadowing
  -fno-warn-unused-binds
  -fno-warn-unused-matches
  -fno-warn-unused-imports
  -fno-warn-type-defaults
  -fno-warn-missing-signatures
  -Werror
  #-}

{-|

High-level vector graphics library. Renders to as SVG using "Web.VirtualDom.Svg".

The API is a rather stripped-down version of Diagrams.

Similar to diagrams:

* Based on "linear", points, vectors and transformations

* Has monoidal overlay of transparent images, local origins and envelpoes



Main differences from Diagrams:

* No 3D

* No HasOrigin
  - Instead of @moveOriginBy v@, use @translate (negated v)@

* No measures, names, queries

* No Semigroups (?)

* No Default (use a left-biased monoid, i.e. mappend = const)

* No names, no queries

* No transformable

* No special attributes, everything is showable/string

* No local or normalized units (behaves as Diagrams' global units)

* traces?

* No HasOrigin class (just use tranformation)

* Supports (tentatively) text envelopes, see http://bl.ocks.org/nitaku/8745933

* Potential animation supports

* Event handler support (VDOM only)

[diagrams]: http://projects.haskell.org/diagrams

-}
module Lubeck.Drawing
  (
  -- * Creating drawings
  -- $fakeInjectiveTFs
  -- ** Geometry
    Point(..)
  , V1(..)
  , V2(..)
  , V3(..)
  , V4(..)
  , P1
  , P2
  , P3
  , P4
  , _x
  , _y

  , Angle
  , acosA
  , turn
  , angleToRadians
  , angleToDegrees
  , offsetVectors
  , betweenPoints

  , Direction
  , dir
  , fromDirection
  , angleBetween
  , angleBetweenDirections

  , Rect(..)
  , rect
  , _left
  , _right
  , _bottom
  , _top
  , p1
  , p2
  , rectToTransf
  , transfToRect
  , fitInsideRect

  , LineSeg(..)
  , lineseg
  , transformLineSeg
  , lineSegToTransf
  , transfToLineSeg

  -- ** Transformations
  , Transformation
  -- , negTransformation
  , lin
  , transp
  , transl
  , transformVector
  , transformPoint
  , transformRect
  , transformDirection
  , transformEnvelope
  , transformationToMatrix
  -- ** Raw transformations
  , rotation
  , scaling
  , scalingX
  , scalingY
  , scalingXY
  , translation
  , translationX
  , translationY
  , shearingX
  , shearingY
  -- $matrixContructorLayout
  , matrix
  -- ** Applying transformations
  , transform

  , rotate
  , scale
  , scaleX
  , scaleY
  , scaleXY
  , translate
  , translateX
  , translateY
  -- , scaleXY
  , shearX
  , shearY

  -- ** Styling
  , Style
  -- *** Color styles
  , fillColor
  , fillColorA
  , strokeColor
  , strokeColorA
  , fillGradient
  -- *** Colors and backgrounds
  , strokeWidth
  -- *** Applying styles
  , style
  -- *** Line styles
  , dash
  , dashing

  -- *** Text styles
  , text
  , textMiddle
  , textEnd
  , textLeftMiddle
  , textMiddleMiddle
  , textRightMiddle
  , TextAnchor(..)
  , AlignmentBaseline(..)
  , FontStyle(..)
  , FontSize
  , FontWeight(..)
  , TextOptions(..)
  , textWithOptions

  -- *** SVG styles
  , styleToAttrString
  , styleNamed

  -- ** Events
  , addHandler
  , tag

  -- ** Embedded SVG
  , Embed(..)
  , addEmbeddedSVG
  , addEmbeddedSVGFromStr

  -- ** HasEnvelopes and alignment
  , Envelope
  , envelope
  -- transformEnvelope
  , unitX
  , unitY
  , posDiagonal
  , negDiagonal
  , (|||)
  , (===)
  , (///)
  , (\\\)
  , juxtapose

  , boundaries
  , align'
  , align
  , OctagonSide(..)

  -- ** Drawings
  -- ** Drawing/Draft type
  , Draft(..)
  , Drawing
  -- ** Backends
  , SVG
  , Fast
  -- ** Backend support
  , Backend
  , HasColors
  , HasLines
  , HasText
  , HasRegions
  , HasEnvelopes

  -- ** Basic drawings
  , transparent
  , circle
  , circleSector
  , square
  , rectangleRounded
  , triangle
  , horizontalLine
  , verticalLine
  , segments
  , polygon

  -- ** Utility
  , xyAxis
  , xyCoords
  , showUnitX
  , showDirection
  , showPoint
  , showBoundaries
  , showEnvelope
  , smokeBackground

  -- * Rendering drawings
  , OriginPlacement(..)
  , RenderingOptions(..)
  -- mempty
  , toSvg
  , toSvgStr
  , toSvgAny
  ) where

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
#if MIN_VERSION_linear(1,20,0)
#else
import Linear.Epsilon
#endif

import Lubeck.Str
import Lubeck.Drawing.Types
import Lubeck.Drawing.Style
import Lubeck.Drawing.Handlers
import Lubeck.Drawing.Text
import Lubeck.Drawing.Transformation

import Lubeck.Drawing.Internal.Backend.FastRenderer(FastDrawing)
import Lubeck.Drawing.Internal.Backend.SVG
import qualified Lubeck.Drawing.Internal.Backend.FastRenderer as FastB
import qualified Lubeck.Drawing.Internal.Backend.SVG as SVG

#ifdef __GHCJS__
-- TODO this is only used by the legacy event API and should be removed
import GHCJS.Types(JSVal)
#endif

{-| An empty and transparent drawing. Same as 'mempty'. -}
transparent :: Backend a => Draft a
transparent = mempty
{-# SPECIALIZE transparent :: Draft SVG #-}

{-| A centered circle with radius one. -}
circle :: Backend a => Draft a
circle  = circleB
{-# INLINABLE circle #-}
{-# SPECIALIZE circle :: Draft SVG #-}
{-# SPECIALIZE circleB :: Draft SVG #-}

{-| A centered circle sector with radius one and the given radii.

A point is in the sector if its in the circle with the same radius and the angle from origin to
the point x is (r1 < x < r2), where r1 and r2 are the radii of the sector.

https://en.wikipedia.org/wiki/Circular_sector
 -}
circleSector :: Backend a => Angle Double -> Angle Double -> Draft a
circleSector = circleSectorB
{-# INLINABLE circleSector #-}
{-# SPECIALIZE circleSector :: Angle Double -> Angle Double -> Draft SVG #-}

{-| A centered square with a width and height of one. -}
square :: Backend a => Draft a
square = squareB
{-# INLINABLE square #-}

{-| A centered square the given x, y, rx, ry dimensions. -}
rectangleRounded :: Backend a => Double -> Double -> Double -> Double -> Draft a
rectangleRounded = rectRoundedB
{-# INLINABLE rectangleRounded #-}
-- NOTE This was called rectangle at some point, confusingly enough.

{-| An (centered?) equilateral with sides of length 1. -}
triangle :: Backend a => Draft a
triangle =
    translate (V2 (-1/2) (-(eqTriAlt/3))) $ polygon
      [V2 1 0, V2 (-1/2) eqTriAlt, V2 (-1/2) (-eqTriAlt)]
  where
    eqTriAlt = sqrt 3 / 2

{-| A centered horizontal line of length one. -}
horizontalLine :: Backend a => Draft a
horizontalLine = translateX (-0.5) lineB

{-| A centered vertical line of length one. -}
verticalLine :: Backend a => Draft a
verticalLine = rotate (turn/4) horizontalLine

{-| Draw a sequence of line segments.

Similar to 'polygon' except endpoints are not joined.
-}
segments :: Backend a => [V2 Double] -> Draft a
segments = linesB False

{-| Draw a polygon.

Similar to 'polygon' except endpoints are not joined.

Argument vectors need not have a zero sum (if they do not an implicit extra
endpoint is assumed.
-}
polygon :: Backend a => [V2 Double] -> Draft a
polygon = linesB True


{-|
Line dash style.

>>> dash [1]
>>> dash [5, 5]
>>> dash [15, 10, 5, 10]

https://developer.mozilla.org/en/docs/Web/SVG/Attribute/stroke-dasharray
-}
dashing :: [Double] -> Style
dashing [] = mempty
dashing ns = styleNamed "stroke-dasharray" (intercalateStr ", " $ map toStr ns)

{-
Line dash style.

>>> dash [1]
>>> dash [5, 5]
>>> dash [15, 10, 5, 10]

https://developer.mozilla.org/en/docs/Web/SVG/Attribute/stroke-dasharray
-}
dash :: HasLines a => [Double] -> Draft a -> Draft a
dash = dash_B

{-| Draw text. See also 'textWithOptions'. -}
text :: Backend a => Str -> Draft a
text = textB

textStart, textMiddle, textEnd :: HasText a => Str -> Draft a

-- | Text horizontally aligned to the start of the text.
--
-- See also 'TextAnchor'.
textStart = textWithOptions $ mempty
  { textAnchor = TextAnchorStart }

-- | Text horizontally aligned to the middle of the text.
--
-- See also 'TextAnchor'.
textMiddle = textWithOptions $ mempty
  { textAnchor = TextAnchorMiddle }

-- | Text horizontally aligned to the end of the text.
--
-- See also 'TextAnchor'.
textEnd = textWithOptions $ mempty
  { textAnchor = TextAnchorEnd }

textLeftMiddle, textMiddleMiddle, textRightMiddle :: HasText a => Str -> Draft a

-- | Text horizontally aligned to the start of the text, and vertically aligned to the middle baseline.
--
-- See also 'TextAnchor', 'AlignmentBaseline'.
textLeftMiddle = textWithOptions $ mempty
  { textAnchor        = TextAnchorStart
  , alignmentBaseline = AlignmentBaselineMiddle }

-- | Text horizontally aligned to the start of the text, and vertically aligned to the middle baseline
--
-- See also 'TextAnchor', 'AlignmentBaseline'.
textMiddleMiddle = textWithOptions $ mempty
  { textAnchor        = TextAnchorMiddle
  , alignmentBaseline = AlignmentBaselineMiddle }

-- | Text horizontally aligned to the start of the text, and vertically aligned to the middle baseline
--
-- See also 'TextAnchor', 'AlignmentBaseline'.
textRightMiddle = textWithOptions $ mempty
  { textAnchor        = TextAnchorEnd
  , alignmentBaseline = AlignmentBaselineMiddle }

-- | Text woth options. Idiomatically:
--
-- @
-- textWithOptions $ mempty
--    { textAnchor        = TextAnchorStart
--    , alignmentBaseline = AlignmentBaselineMiddle }
-- @
--
textWithOptions :: HasText a => TextOptions -> Str -> Draft a
textWithOptions opts t = textOptions_B opts (textB t)
{-# INLINABLE textWithOptions #-}

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
transform :: Backend a => Transformation Double -> Draft a -> Draft a
transform t dr = transfB t dr
{-# INLINABLE transform #-}

{-| Translate (move) an image. -}
translate :: Backend a => V2 Double -> Draft a -> Draft a
-- translate (V2 dx dy) = transform $ matrix (1,0,0,1,dx,dy)
translate v = transform $ translation v

{-| Translate (move) an image along the horizonal axis.
A positive argument will move the image to the right. -}
translateX :: Backend a => Double -> Draft a -> Draft a
-- translateX x = translate (V2 x 0)
translateX x = transform $ translationX x

{-| Translate (move) an image along the vertical axis.
A positive argument will move the image upwards (as opposed to standard SVG behavior). -}
translateY :: Backend a => Double -> Draft a -> Draft a
-- translateY y = translate (V2 0 y)
translateY y = transform $ translationY y

{-| Scale (stretch) an image. -}
scaleXY :: Backend a => V2 Double -> Draft a -> Draft a
scaleXY (V2 x y) = transform $ matrix (x,0,0,y,0,0)

{-| Scale (stretch) an image, preserving its horizontal/vertical proportion. -}
scale :: Backend a => Double -> Draft a -> Draft a
-- scale x = scaleXY x x
scale a = transform $ scaling a

{-| Scale (stretch) an image horizontally. -}
scaleX :: Backend a => Double -> Draft a -> Draft a
-- scaleX x = scaleXY x 1
scaleX a = transform $ scalingX a

{-| Scale (stretch) an image vertically. -}
scaleY :: Backend a => Double -> Draft a -> Draft a
-- scaleY y = scaleXY 1 y
scaleY a = transform $ scalingY a

{-| Rotate an image. A positive vale will result in a counterclockwise rotation and negative value in a clockwise rotation. -}
rotate :: Backend a => Angle Double -> Draft a -> Draft a
-- rotate (Radians a) = transform $ matrix (cos a, 0 - sin a, sin a, cos a, 0, 0)
-- NOTE The b,c, signs are inverted because of the reverse y polarity.
rotate a = transform $ rotation a

{-| Shear an image. -}
-- shear :: Double -> Double -> Draft SVG -> Draft SVG
-- shear a b = transform $ matrix (1, b, a, 1, 0, 0)
shearX :: Backend a => Double -> Draft a -> Draft a
shearX a = transform $ shearingX a

shearY :: Backend a => Double -> Draft a -> Draft a
shearY a = transform $ shearingY a

{-# INLINABLE translate #-}
{-# INLINABLE translateX #-}
{-# INLINABLE translateY #-}
{-# INLINABLE scale #-}
{-# INLINABLE scaleX #-}
{-# INLINABLE scaleY #-}
{-# INLINABLE scaleXY #-}
{-# INLINABLE rotate #-}
{-# INLINABLE shearX #-}
{-# INLINABLE shearY #-}




{-| A "big" smoke-colored background.

Useful to see the boundary of the canvas, as in:

@
  (fillColor "red" square) `over` smokeBackground
@
-}
smokeBackground :: (Backend a, HasColors a) => Draft a
smokeBackground = fillColor Colors.whitesmoke $ scale 5000 $ square

{-| Draw the X and Y axis inside the unit square (their intersection is the origin). -}
xyAxis :: (Backend a, HasColors a, HasLines a) => Draft a
xyAxis = scale 5000 $ strokeColor Colors.darkgreen $ strokeWidth 0.5 $
  mconcat [horizontalLine, verticalLine]

{-| Draw the X and Y axis inside the unit square, unit circle and unit square. -}
xyCoords :: (Backend a, HasColors a, HasLines a) => Draft a
xyCoords = fillColorA (Colors.black `withOpacity` 0) $ strokeColor Colors.darkgreen $
  strokeWidth 0.5 $ mconcat [horizontalLine, verticalLine, circle, square]

-- | Draw the unit vector.
showUnitX :: (Backend a, HasColors a, HasLines a) => Draft a
showUnitX = strokeColor Colors.red $ strokeWidth 3 $ translateX 0.5 horizontalLine

-- | Draw an image representing a direction.
showDirection :: (Backend a, HasColors a, HasLines a) => Direction V2 Double -> Draft a
showDirection dir = scale 100 $ strokeColor Colors.red $ strokeWidth 3 $ fillColorA tp $ segments [fromDirection dir]
  where
    tp = Colors.black `withOpacity` 0

-- | Draw an image representing two boundaries of an image along the given line.
--
-- See also 'boundaries'.
showBoundaries :: (Backend a, HasColors a, HasLines a, HasEnvelopes a) => V2 Double -> Draft a -> Draft a
showBoundaries v dr = case boundaries v (envelope dr) of
  Nothing -> dr
  Just (lo, hi) -> strokeColor Colors.blue (showPoint lo) <> scale 2 (showPoint hi) <> dr

-- | Draw an image representing a point.
showPoint :: (Backend a, HasColors a, HasLines a) => Point V2 Double -> Draft a
showPoint p = translate (p .-. origin) base
  where
    base = strokeColor Colors.red $ fillColorA (Colors.black `withOpacity` 0) $strokeWidth 2 $ scale 1 $ circle

-- | Draw an image representing an envelope.
showEnvelope :: (Backend a, HasColors a, HasLines a, HasEnvelopes a) => V2 Double -> Draft a -> Draft a
showEnvelope v drawing = case envelopeVMay v drawing of
  Nothing -> drawing
  Just v2 -> (rotate (angleBetween v2 unitX) $ scale (norm v2) $ unitX_T) <> drawing
  where
    -- A sideways T in the unit square, with the "top" pointing
    -- in the direction of unitX
    unitX_T = strokeWidth 2 $ strokeColor Colors.red $ fillColorA tp $ segments [V2 0 0, V2 1 0, V2 0 0.5, V2 0 (-1)]
    tp = Colors.black `withOpacity` 0

{-| Apply a style to a drawing. -}
style :: Style -> Draft SVG -> Draft SVG
style s (Draft dr) = Draft $ Style s dr

{-| -}
fillColor :: HasColors a => Colour Double -> Draft a -> Draft a
fillColor = fillColor_B

{-| -}
strokeColor :: HasColors a => Colour Double -> Draft a -> Draft a
strokeColor = strokeColor_B

{-| -}
fillColorA :: HasColors a => AlphaColour Double -> Draft a -> Draft a
fillColorA = fillColorA_B

-- {-| -}
strokeColorA :: HasColors a => AlphaColour Double -> Draft a -> Draft a
strokeColorA = strokeColorA_B

{-| Set the stroke width. By default stroke is /not/ affected by scaling or other transformations.
-}
strokeWidth :: HasLines a => Double -> Draft a -> Draft a
strokeWidth = strokeWidth_B

fillGradient :: HasColors a => Gradient -> Draft a -> Draft a
fillGradient = fillGradient_B

-- SLOW
-- TODO move
showColor :: Colour Double -> Str
showColor = packStr . Data.Colour.SRGB.sRGB24show




{-| Add a tag to this drawing.
    Used for low-level event detection.
-}
tag :: HasRegions b => Int -> Draft b -> Draft b
tag = tag_B

{-| Add an event handler to the given drawing.

   Handlers_ are embedde using 'Web.VirtualDom.on' so the same naming conventions apply.
-}
#ifdef __GHCJS__
addHandler :: Str -> (JSVal -> IO ()) -> Draft SVG -> Draft SVG
addHandler k v (Draft d) = Draft $ Handlers (singleTonHandlers k v) d
#else
addHandler :: Str -> a -> Draft SVG -> Draft SVG
addHandler k v (Draft d) = Draft $ Handlers () d
#endif



newtype Envelope v n = Envelope (Maybe (v n -> n))

instance (Foldable v, Additive v, Floating n, Ord n) => Monoid (Envelope v n) where
  mempty      = Envelope Nothing
  mappend (Envelope x) (Envelope y) = case (x, y) of
    (Nothing, _)       -> Envelope y
    (_,       Nothing) -> Envelope x
    (Just f,  Just g)  -> Envelope $ Just $ maxEnv f g

    -- Invoke max explicitly, as Data.Monoid.Max has a superflous Bounded constraint
    -- Alternatively, we could escape this by using the semigroup version
    where
      -- maxEnv :: Floating n => (v n -> n) -> (v n -> n) -> v n -> n
      maxEnv f g v = max (f v) (g v)


transformEnvelope :: (Floating n
#if MIN_VERSION_linear(1,20,0)
#else
  , Epsilon n
#endif
  ) => Transformation n -> Envelope V2 n -> Envelope V2 n
transformEnvelope t env = moveOrigin (negated (transl t)) $ onEnvelope g env
    where
      onEnvelope f (Envelope Nothing)  = Envelope Nothing
      onEnvelope f (Envelope (Just e)) = Envelope (Just $ f e)
      moveOrigin u = onEnvelope $ \f v -> f v - ((u ^/ (v `dot` v)) `dot` v)

      -- funtion -> vector -> scalar
      g f v = f v' / (v' `dot` vi)
        where
          v'     = signorm $ lapp (transp t) v
          vi     = apply (inv t) v

          -- correct?
          apply  = transformVector
          lapp   = transformVector
          inv    = recip
{-# INLINABLE transformEnvelope #-}

juxtapose :: (HasEnvelopes a) => V2 Double -> Draft a -> Draft a -> Draft a
juxtapose v a1 a2 =   case (mv1, mv2) of
    (Just v1, Just v2) -> moveOriginBy (v1 ^+^ v2) a2
    _                  -> a2
  where mv1 = negated <$> envelopeVMay v a1
        mv2 = envelopeVMay (negated v) a2
        moveOriginBy v = translate (negated v)

envelopeVMay :: HasEnvelopes a => V2 Double -> Draft a -> Maybe (V2 Double)
envelopeVMay v = envelopeVMay' v . envelope

envelopeVMay' :: (Functor v, Num n) => v n -> Envelope v n -> Maybe (v n)
envelopeVMay' v = fmap ((*^ v) . ($ v)) . appEnvelope
  where
    appEnvelope (Envelope e) = e

-- | Takes a line and a direction (represented as a vector, whose magnitude is ignored).
--
--  Returns the two lowest and highest point on the line that falls insiode the envelope,
--  ordering by closeness to the infinity the vector points towards. I.e. given unitX,
--  returns the leftMost and rightMost point inside the envelope.
boundaries :: (Functor v, Num n, Num (v n), Additive v) => v n -> Envelope v n -> Maybe (Point v n, Point v n)
boundaries v e = liftA2 (,) lb ub
  where
    lb = (origin .+^) <$> envelopeVMay' (-v) e
    ub = (origin .+^) <$> envelopeVMay' v e

alignE' :: (Functor v, Num n, Num (v n), Additive v) => v n -> n -> Envelope v n -> Maybe (Point v n)
alignE' v n e = g <$> boundaries v e
  where
    g (lb, ub) = lerp n lb ub

data OctagonSide = BL | TR | TL | BR | L | R | T | B
  deriving (Eq, Ord, Show)

alignE :: (Floating n, v ~ V2) => OctagonSide -> Envelope v n -> Maybe (Point v n)
alignE BL  = alignE' posDiagonal 0
alignE TR  = alignE' posDiagonal 1
alignE TL  = alignE' negDiagonal 0
alignE BR  = alignE' negDiagonal 1
alignE L   = alignE' unitX 0
alignE R   = alignE' unitX 1
alignE T   = alignE' unitY 1
alignE B   = alignE' unitY 0

align' :: HasEnvelopes a => V2 Double -> Double -> Draft a -> Draft a
align' v n dr = case alignE' v n (envelope dr) of
  Nothing -> mempty
  Just p  -> moveOriginTo p dr

align :: HasEnvelopes a => OctagonSide -> Draft a -> Draft a
align t dr = case alignE t (envelope dr) of
  Nothing -> mempty
  Just p  -> moveOriginTo p dr

{-|
Fit a drawing inside a rectangle. Accomplished by aligning the drawing
at the bottom left corner, scaling by (v1.-.v2) and translating by (v1 .-. origin).
-}
fitInsideRect :: HasEnvelopes a => Rect Double -> Draft a -> Draft a
fitInsideRect (Rect_ (p1@(P (v1@(V2 x1 y1)))) (p2@(P (v2@(V2 x2 y2))))) d = id
    $ translate (p1 .-. origin)
    $ scaleXY   (p2 .-. p1)
    -- Align at bottom left corner, so that the translation part can be derived from the (x1,y1)
    -- Alternatively, we could align at TR and derive translation from (x2,y2) and so on
    $ align BL
    $ d

moveOriginBy :: Backend a => V2 Double -> Draft a -> Draft a
moveOriginBy v = translate (-v)

moveOriginTo :: Backend a => P2 Double -> Draft a -> Draft a
moveOriginTo p = moveOriginBy (origin .-. p)
-- FIXME should be the other way around!

envelope :: HasEnvelopes a => Draft a -> Envelope V2 Double
envelope = envelope_B

svgEnvelope :: SVGDrawing -> Envelope V2 Double
svgEnvelope x = case x of
  Circle            -> Envelope $ Just $ \v -> (0.5/norm v)
  -- FIXME no envelope for CircleSector
  CircleSector _ _  -> svgEnvelope Circle
  -- Goes around like: TL, TR, BR, BL
  Rect                -> pointsEnvelope $ fmap P [V2 (0.5) (0.5), V2 (-0.5) (0.5), V2 (-0.5) (-0.5), V2 (0.5) (-0.5)]
  RectRounded x y _ _ -> pointsEnvelope $ fmap P [V2 (0.5*x) (0.5*y), V2 (-0.5*x) (0.5*y), V2 (-0.5*x) (-0.5*y), V2 (0.5*x) (-0.5*y)]
  Line          -> pointsEnvelope $ fmap P [V2 0 0, V2 1 0]
  Lines _ vs    -> pointsEnvelope $ offsetVectors origin vs
  -- No proper text envelopes, fake by using a single rectangles
  -- https://github.com/BeautifulDestinations/lubeck/issues/73
  Text _        -> svgEnvelope Rect

  Embed _       -> mempty

  {-
  TODO envelopes should take masking into account.
  -}
  Mask d1 d2    -> svgEnvelope d2

  Transf t x    -> transformEnvelope t (svgEnvelope x)
  Style _ x     -> svgEnvelope x
  SpecialStyle _ x     -> svgEnvelope x
  Handlers _ x  -> svgEnvelope x
  Em            -> mempty
  Ap xs         -> mconcat (fmap svgEnvelope xs)

pointsEnvelope :: [P2 Double] -> Envelope V2 Double
pointsEnvelope [] = Envelope $ Nothing
pointsEnvelope ps = Envelope $ Just $ \v ->
  (\(P (V2 x _)) -> x / norm v) $ rightMost $ rotatePoints (angleBetween v unitX) ps
  where
    -- Rotate all points so that the input vector aligns with the "conceptual" unitX
    -- Find rightmost point by comparing x coord
    -- Return x of leftmost point / magnitude of vector
    --
    -- This could probably be expressed more concisely with basis, but the result is
    -- the same.
    rightMost = Data.List.maximumBy (Data.Ord.comparing (\(P (V2 x _)) -> x))

    rotatePoints :: Angle Double -> [P2 Double] -> [P2 Double]
    rotatePoints a = fmap (rotatePoint a)
    rotatePoint a  = transformPoint (rotation a)


(===), (|||), (\\\), (///) :: HasEnvelopes a => Draft a -> Draft a -> Draft a
a ||| b = a <> juxtapose unitX a b
a === b = a <> juxtapose (negated unitY) a b
a \\\ b = a <> juxtapose posDiagonal a b
a /// b = a <> juxtapose negDiagonal a b

-- TODO general path/beziers (cubic spline?) support (generalizes all others! including text?)


{-|
Embed arbitrary SVG markup in a drawing.
-}
addEmbeddedSVG :: Embed -> Draft SVG
addEmbeddedSVG x = Draft $ Embed x

{-|
Embed arbitrary SVG markup in a drawing.

Argument must be a valid SVG string, or @Nothing@ is returned.
-}
addEmbeddedSVGFromStr :: Str -> Maybe (Draft SVG)
addEmbeddedSVGFromStr x = fmap addEmbeddedSVGFromXmlLight $ Text.XML.Light.parseXMLDoc $ unpackStr x

addEmbeddedSVGFromXmlLight :: Text.XML.Light.Element -> Draft SVG
addEmbeddedSVGFromXmlLight = addEmbeddedSVG . textXmlLightElementToEmbed

textXmlLightElementToEmbed :: Text.XML.Light.Element -> Embed
textXmlLightElementToEmbed = unE
  where
    unE :: Text.XML.Light.Element -> Embed
    unE (X.Element (X.QName k _ _) as ns _) = EmbedNode (packStr k) (fmap unA as) (fmap unC ns)
    -- Ignore namespace/prefix

    unC :: Text.XML.Light.Content -> Embed
    unC (X.Elem x) = unE x
    unC (X.Text (X.CData _ x _)) = EmbedContent (packStr x)
    unC (X.CRef _) = error ""
    -- Ignore cdata/raw, assume CDataText

    unA :: Text.XML.Light.Attr -> (Str, Str)
    unA (X.Attr (X.QName k _ _) v) = (packStr k, packStr v)
    -- Ignore namespace/prefix



{-| $fakeInjectiveTFs

The 'SVG :: *' and 'Fast :: *' tyes represent backends
'Draft :: * -> *' is a type parameterized on backend type.

This can be encoded properly with injective TFs, until we
have those please pretend that 'SVG' and 'Fast' are opaque types.

It could *also* be encoded with GADTs, but we avoid these
because of the run-time cost.
-}
type SVG  = SVGDrawing
type Fast = FastDrawing

newtype Draft a = Draft { getDraft :: a }
  deriving (Monoid)

type Drawing = Draft SVG


-- | Drawing backends.
-- The double monoid constraint is because we fake injective TFs, see $fakeInjectiveTFs.
class (Monoid b, Monoid (Draft b)) => Backend b where
  circleB         :: Draft b
  squareB         :: Draft b
  circleSectorB   :: (Angle Double) -> (Angle Double) -> Draft b
  rectRoundedB    :: Double -> Double -> Double -> Double -> Draft b
  lineB           :: Draft b
  linesB          :: Bool -> [V2 Double] -> Draft b
  textB           :: Str -> Draft b
  transfB         :: Transformation Double -> Draft b -> Draft b

-- | Drawing backends with text style support.
class Backend b => HasText b where
  textOptions_B :: TextOptions -> Draft b -> Draft b

-- | Drawing backends with color support.
class Backend b => HasColors b where
  fillColor_B :: Colour Double -> Draft b -> Draft b
  strokeColor_B :: Colour Double -> Draft b -> Draft b
  fillColorA_B :: AlphaColour Double -> Draft b -> Draft b
  strokeColorA_B :: AlphaColour Double -> Draft b -> Draft b
  fillGradient_B :: Gradient -> Draft b -> Draft b

-- | Drawing backends with line style support.
class Backend b => HasLines b where
  strokeWidth_B :: Double -> Draft b -> Draft b
  dash_B :: [Double] -> Draft b -> Draft b
  -- TODO cap/join

-- | Drawing backends with region support (used for event detection etc).
class Backend b => HasRegions b where
  tag_B :: Int -> Draft b -> Draft b

-- | Drawing backends with envelope support (used for positioning/alignment)
class Backend b => HasEnvelopes b where
  envelope_B :: Draft b -> Envelope V2 Double


instance Backend SVG where
  circleB = Draft $ SVG.Circle
  squareB = Draft $ SVG.Rect
  circleSectorB a1 a2 = Draft $ SVG.CircleSector a1 a2
  rectRoundedB x y rx ry = Draft $ SVG.RectRounded x y rx ry
  lineB = Draft $ SVG.Line
  linesB c ps = Draft $ SVG.Lines c ps
  textB t = Draft $ SVG.Text t
  transfB t (Draft x) = Draft $ SVG.Transf t x

instance HasText SVG where
  textOptions_B t (Draft x) = Draft $ Style (textOptionsToStyle t) x

instance HasColors SVG where
  fillColor_B x (Draft d) = Draft $ SVG.Style (styleNamed "fill" $ showColor x) d
  strokeColor_B x (Draft d) = Draft $ SVG.Style (styleNamed "stroke" $ showColor x) d
  fillColorA_B x (Draft d) = Draft $ Style (styleNamed "fill" $ showColor c) . alpha a $ d
    where
      alpha a = SVG.Style (styleNamed "fill-opacity" $ toStr a)
      c = Data.Colour.over x Colors.black
      a = Data.Colour.alphaChannel x
  strokeColorA_B x (Draft d) = Draft $ Style (styleNamed "stroke" $ showColor c) . alpha a $ d
    where
      alpha a = SVG.Style (styleNamed "stroke-opacity" $ toStr a)
      c = Data.Colour.over x Colors.black
      a = Data.Colour.alphaChannel x
  fillGradient_B g (Draft d) = Draft $ SpecialStyle (FillGradient g) d

instance HasLines SVG where
  strokeWidth_B x (Draft d) = Draft $ SVG.Style (styleNamed "stroke-width" (toStr x <> "px")) d
  dash_B x (Draft d) = Draft $ SVG.Style (dashing x) d

instance HasEnvelopes SVG where
  envelope_B (Draft d) = svgEnvelope d -- TODO move


#ifdef __GHCJS__
instance Backend Fast where
  circleB = Draft $ FastB.circle 0 0 0.5
  squareB = Draft $ FastB.rect (-0.5) (-0.5) 1 1
  circleSectorB a1 a2 = circleB -- TODO
  rectRoundedB x y rx ry = scaleXY (V2 x y) $ squareB -- TODO
  lineB = linesB False [(V2 0 0), (V2 0 1)]
  linesB closed [] = Draft $ mempty
  {-
  What we get is a list of vectors between the points, first point always at origin.
  Transform into absolute points, starting at (P $ V2 0 0).
  -}
  linesB closed vs = Draft $ FastB.path 0 0 (FastB.linePathP2 closed ps) -- TODO
    where
      ps = offsetVectors (P $ V2 0 0) vs
  textB s = Draft $ FastB.text 0 0 s
  transfB (TF (V3 (V3 a c e) (V3 b d f) (V3 _ _ _))) (Draft dr) = Draft $ FastB.transf a b c d e f dr

instance HasColors Fast where
  fillColor_B c (Draft d) = Draft $ FastB.fillColor r g b 1 d
    where
      (Data.Colour.SRGB.RGB !r !g !b) = Data.Colour.SRGB.toSRGB c
  strokeColor_B c (Draft d) = Draft $ FastB.strokeColor r g b 1 d
    where
      (Data.Colour.SRGB.RGB !r !g !b) = Data.Colour.SRGB.toSRGB c
  fillColorA_B ac (Draft d) = Draft $ FastB.fillColor r g b a d
    where
      (Data.Colour.SRGB.RGB !r !g !b) = Data.Colour.SRGB.toSRGB c
      c = Data.Colour.over ac Colors.black
      a = Data.Colour.alphaChannel ac
  strokeColorA_B ac (Draft d) = Draft $ FastB.strokeColor r g b a d
    where
      (Data.Colour.SRGB.RGB !r !g !b) = Data.Colour.SRGB.toSRGB c
      c = Data.Colour.over ac Colors.black
      a = Data.Colour.alphaChannel ac
  -- TODO
  fillGradient_B _ x = x

instance HasText Fast where
  textOptions_B opts x = id
      -- style weight size family
      $ mapD (FastB.textFont $ toJSString $ st (fontStyle opts) <> " " <> fw (fontWeight opts) <> " " <> fs (fontSize opts) <> " " <> ff (fontFamily opts))
      $ tb (alignmentBaseline opts)
      $ ta (textAnchor opts) x
    where
      mapD f (Draft x) = Draft (f x)

      fs (First (Just v)) = v
      fs (First Nothing)  = ""

      ff (First (Just v)) = v
      ff (First Nothing)  = ""

      st FontStyleNormal = "normal"
      st FontStyleItalic = "italic"
      st FontStyleOblique = "oblique"
      st FontStyleInherit = ""

      fw FontWeightNormal  = "normal"
      fw (FontWeightN n)   = toStr n
      fw FontWeightBold    = "bold"
      fw FontWeightBolder  = "bolder"
      fw FontWeightLighter = "lighter"
      fw FontWeightInherit = ""

      ta TextAnchorStart    = mapD $ FastB.textAlign FastB.TextAlignStart
      ta TextAnchorEnd      = mapD $ FastB.textAlign FastB.TextAlignEnd
      ta TextAnchorMiddle   = mapD $ FastB.textAlign FastB.TextAlignCenter
      ta TextAnchorInherit  = id

      -- TODO top/bottom?
      tb AlignmentBaselineAuto = id
      tb AlignmentBaselineMiddle = mapD $ FastB.textBaseline FastB.TextBaselineMiddle
      tb AlignmentBaselineHanging = mapD $ FastB.textBaseline FastB.TextBaselineHanging
      tb AlignmentBaselineCentral = id
      tb AlignmentBaselineAlphabetic = mapD $ FastB.textBaseline FastB.TextBaselineAlphabetic
      tb AlignmentBaselineBaseline = id
      tb AlignmentBaselineAfterEdge = id
      tb AlignmentBaselineBeforeEdge = id
      tb AlignmentBaselineTextAfterEdge = id
      tb AlignmentBaselineTextBeforeEdge = id
      tb AlignmentBaselineMathematical = id
      tb AlignmentBaselineIdeographic = mapD $ FastB.textBaseline FastB.TextBaselineIdeographic


instance HasLines Fast where
  strokeWidth_B x (Draft d) = Draft $ FastB.lineWidth x d
  dash_B _ x = x -- TODO

instance HasRegions Fast where
  tag_B n (Draft x) = Draft $ FastB.tag n x

{-# SPECIALIZE rectangleRounded :: Double -> Double -> Double -> Double -> Draft Fast #-}
{-# SPECIALIZE square :: Draft Fast #-}
{-# SPECIALIZE transparent :: Draft Fast #-}
{-# SPECIALIZE circle :: Draft Fast #-}
{-# SPECIALIZE circleB :: Draft Fast #-}
{-# SPECIALIZE circleSector :: Angle Double -> Angle Double -> Draft Fast #-}

#endif
