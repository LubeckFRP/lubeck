
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, TypeFamilies, OverloadedStrings,
  NamedFieldPuns, CPP, NoMonomorphismRestriction, BangPatterns, StandaloneDeriving
  , ScopedTypeVariables, NoImplicitPrelude #-}

-- {-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}

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
  -- TODO move/rename these?
  , offsetVectors
  , betweenPoints

  , Direction
  , dir
  , fromDirection
  , angleBetween
  , angleBetweenDirections

  , Rect(..)
  , fitInsideRect

  -- ** Transformations
  , Transformation
  , negTransformation
  , lin
  , transp
  , transl
  , transformVector
  , transformPoint
  , transformDirection
  , transformEnvelope
  , transformationToMatrix
  -- ** Raw transformations
  , rotation
  , scaling
  , scalingX
  , scalingY
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
  , styleNamed
  , fillColor
  , fillColorA
  , strokeColor
  , strokeColorA
  , strokeWidth
  -- *** Rendering styles
  , styleToAttrString
  -- *** Applying styles
  , style

  -- *** Line style
  , dash
  , dashing

  -- *** Text
  , text
  , textMiddle
  , textEnd
  , textLeftMiddle
  , textMiddleMiddle
  , textRightMiddle
  , TextAnchor(..)
  , AlignmentBaseline(..)
  , FontStyle(..)
  , FontSize(..)
  , FontWeight(..)
  , TextOptions(..)
  , textWithOptions

  -- ** Events
  , addHandler
  -- , addProperty

  -- ** Embedded SVG
  , Embed(..)
  , addEmbeddedSVG
  , addEmbeddedSVGFromStr

  -- ** Envelopes, Alignment, Juxtaposition
  , Envelope
  , envelope
  -- transformEnvelope
  , unitX
  , unitY
  , posDiagonal
  , negDiagonal
  , (|||)
  , (===)
  , juxtapose

  , boundaries
  , align'
  , align
  , OctagonSide(..)

  -- ** Drawings
  , Drawing
  -- ** Basic drawings
  , transparent
  , circle
  , square
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

  -- ** High-performance
  , RDrawing
  , renderDrawing
  , emitDrawing
  ) where

import BasePrelude hiding (Handler, Handlers, rotate, (|||))
-- import Data.Semigroup(Max(..))
import Data.Colour(Colour, AlphaColour, withOpacity)
import qualified Data.Colour
import qualified Data.Colour.Names as Colors
import qualified Data.Colour.SRGB
import qualified Data.List
import qualified Data.Ord
import qualified Data.Map.Strict as Map
import qualified Data.String
import qualified Data.Sequence as Seq
-- import Data.Sequence(Seq)
-- import Data.Foldable(toList)

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
import Lubeck.Drawing.Style
import Lubeck.Drawing.Handlers
import Lubeck.Drawing.Text
import Lubeck.Drawing.Transformation

#ifdef __GHCJS__
import GHCJS.Types(JSVal, JSString)
import Web.VirtualDom.Svg (Svg)
import qualified Web.VirtualDom as VD
import qualified Web.VirtualDom.Svg as E
import qualified Web.VirtualDom.Svg.Attributes as A
#endif

import System.IO.Unsafe(unsafePerformIO)

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

angleBetweenDirections :: (Metric v, Floating n) => Direction v n -> Direction v n -> Angle n
angleBetweenDirections x y = acosA $ (fromDirection x) `dot` (fromDirection y)



{-|
A rectangle, represented as two points.
-}
data Rect = Rect_ (P2 Double) (P2 Double)
  deriving (Eq, Ord, Show)

{-
Fit a drawing inside a rectangle. Accomplished by aligning the drawing
at the bottom left corner, scaling by (v1.-.v2) and translating by (v1 .-. origin).
-}
fitInsideRect :: Rect -> Drawing -> Drawing
fitInsideRect (Rect_ (p1@(P (v1@(V2 x1 y1)))) (p2@(P (v2@(V2 x2 y2))))) d = id
    $ translate (p1 .-. origin)
    $ scaleXY   (p2 .-. p1)
    -- Align at bottom left corner, so that the translation part can be derived from the (x1,y1)
    -- Alternatively, we could align at TR and derive translation from (x2,y2) and so on
    $ align BL
    $ d



{-| Add an event handler to the given drawing.

   Handlers_ are embedde using 'Web.VirtualDom.on' so the same naming conventions apply.
-}
#ifdef __GHCJS__
addHandler :: Str -> (JSVal -> IO ()) -> Drawing -> Drawing
addHandler k v = Handlers (singleTonHandlers k v)
#else
addHandler :: Str -> a -> Drawing -> Drawing
addHandler k v = Handlers ()
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
          inv    = negTransformation
{-# INLINABLE transformEnvelope #-}

juxtapose :: V2 Double -> Drawing -> Drawing -> Drawing
juxtapose v a1 a2 =   case (mv1, mv2) of
    (Just v1, Just v2) -> moveOriginBy (v1 ^+^ v2) a2
    _                  -> a2
  where mv1 = negated <$> envelopeVMay v a1
        mv2 = envelopeVMay (negated v) a2
        moveOriginBy v = translate (negated v)

envelopeVMay :: V2 Double -> Drawing -> Maybe (V2 Double)
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

align' :: V2 Double -> Double -> Drawing -> Drawing
align' v n dr = case alignE' v n (envelope dr) of
  Nothing -> mempty
  Just p  -> moveOriginTo p dr

align :: OctagonSide -> Drawing -> Drawing
align t dr = case alignE t (envelope dr) of
  Nothing -> mempty
  Just p  -> moveOriginTo p dr

moveOriginBy :: V2 Double -> Drawing -> Drawing
moveOriginBy v = translate (-v)

moveOriginTo :: P2 Double -> Drawing -> Drawing
moveOriginTo p = moveOriginBy (origin .-. p)
-- FIXME should be the other way around!

envelope :: Drawing -> Envelope V2 Double
envelope x = case x of
  Circle        -> Envelope $ Just $ \v -> (0.5/norm v)
  Rect          -> pointsEnvelope $ fmap P [V2 (0.5) (0.5), V2 (-0.5) (0.5), V2 (-0.5) (-0.5), V2 (0.5) (-0.5)]
  Line          -> pointsEnvelope $ fmap P [V2 0 0, V2 1 0]
  Lines _ vs    -> pointsEnvelope $ offsetVectors origin vs
  -- No proper text envelopes, fake by using a single rectangles
  -- https://github.com/BeautifulDestinations/lubeck/issues/73
  Text _        -> envelope Rect

  Embed _       -> mempty

  Transf t x    -> transformEnvelope t (envelope x)
  Style _ x     -> envelope x
  Handlers _ x  -> envelope x
  Em            -> mempty
  Ap xs         -> mconcat (fmap envelope xs)

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

(===), (|||), (\\\), (///) :: Drawing -> Drawing -> Drawing
a ||| b = a <> juxtapose unitX a b
a === b = a <> juxtapose (negated unitY) a b
a \\\ b = a <> juxtapose posDiagonal a b
a /// b = a <> juxtapose negDiagonal a b

-- TODO general path/beziers (cubic spline?) support (generalizes all others! including text?)
-- TODO masks support
-- TODO better text API

{-|
Embedded SVG node.
-}
data Embed
  = EmbedNode Str [(Str, Str)] [Embed]
  | EmbedContent Str
  deriving (Eq, Show)

{-|
Embed arbitrary SVG markup in a drawing.
-}
addEmbeddedSVG :: Embed -> Drawing
addEmbeddedSVG = Embed

{-|
Embed arbitrary SVG markup in a drawing.

Argument must be a valid SVG string, or @Nothing@ is returned.
-}
addEmbeddedSVGFromStr :: Str -> Maybe Drawing
addEmbeddedSVGFromStr x = fmap addEmbeddedSVGFromXmlLight $ Text.XML.Light.parseXMLDoc $ unpackStr x

addEmbeddedSVGFromXmlLight :: Text.XML.Light.Element -> Drawing
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
  -- A line along the unit vector
  | Line
  -- A sequence of straight lines, closed or not. For closed lines, there is no need
  -- to return the original point (i.e. the sum of the vectors does not have to be zeroV).
  | Lines !Bool ![V2 Double]
  | Text !Str

  | Embed Embed

  | Transf !(Transformation Double) !Drawing
  | Style !Style !Drawing
  | Handlers !Handlers !Drawing

  | Em
  -- Compose drawings
  -- Left-most is a top,
  | Ap ![Drawing]


printTreeInfo :: Drawing -> IO ()
printTreeInfo x = do
  print $ "Tree N nodes " ++ show (drawingTreeNNodes x)
  print $ "Tree depth   " ++ show (drawingTreeDepth x)
drawingTreeNNodes = drawingTreeF (+)
drawingTreeDepth = drawingTreeF max

drawingTreeF f = go
  where
    go Circle = 1
    go Rect = 1
    go Line = 1
    go (Lines _ _) = 1
    go (Text _) = 1
    go (Embed _) = 1
    go (Transf _ x) = go x + 1
    go (Style _ x) = go x + 1
    go (Handlers _ x) = go x + 1
    go Em = 1
    go (Ap xs) = foldr f 0 (fmap go xs)

instance Monoid Drawing where
  mempty  = Em

  mappend Em x = x
  mappend x Em = x
  mappend (Ap xs) (Ap ys) = Ap (xs <> ys)
  mappend x (Ap ys) = Ap (x : ys)
  mappend (Ap xs) y = Ap (xs <> pure y)
  mappend x y = Ap [x, y]

  -- mconcat = Ap

drawingToRDrawing :: Drawing -> RDrawing
drawingToRDrawing = drawingToRDrawing' mempty
{-# INLINE drawingToRDrawing #-}

drawingToRDrawing' :: RNodeInfo -> Drawing -> RDrawing
drawingToRDrawing' nodeInfo x = case x of
    Circle                 -> RPrim nodeInfo RCircle
    Rect                   -> RPrim nodeInfo RRect
    Line                   -> RPrim nodeInfo RLine
    (Lines closed vs)      -> RPrim nodeInfo (RLines closed vs)
    (Text t)               -> RPrim nodeInfo (RText t)
    (Embed e)              -> RPrim nodeInfo (REmbed e)
    (Transf t x)           -> drawingToRDrawing' (nodeInfo <> toNodeInfoT t) x
    (Style s x)            -> drawingToRDrawing' (nodeInfo <> toNodeInfoS s) x
    (Handlers h x) -> drawingToRDrawing' (nodeInfo <> toNodeInfoH h) x
    Em        -> mempty
    -- TODO could probably be optimized by some clever redifinition of the Drawing monoid
    -- current RNodeInfo data render on this node alone, so further invocations uses (recur mempty)
    -- TODO return empty if concatMap returns empty list
    (Ap xs)   -> RMany nodeInfo ((\x -> seqListE x x) $ mapReverse recur xs)
      where
        recur = drawingToRDrawing' mempty

mapReverse :: (a -> b) -> [a] -> [b]
mapReverse f l =  rev l mempty
  where
    rev []     a = a
    rev (x:xs) a = rev xs (f x : a)

{-# INLINABLE drawingToRDrawing' #-}

data RPrim
   = RCircle
   | RRect
   | RLine
   -- TODO use Seq not []
   | RLines !Bool ![V2 Double]
   | RText !Str
   | REmbed !Embed
   deriving (Show)

data RNodeInfo
  = RNodeInfo
    { rStyle   :: !Style
    , rTransf  :: !(Transformation Double)
    , rHandler :: !Handlers
    }
   deriving (Show)

-- TODO remove

#ifdef __GHCJS__
instance Show Handlers where
  show x = "handlers"
#endif
instance Show Style where
  show x = "style"
instance (Show a, Num a) => Show (Transformation a) where
  show x = "transformation" ++ show (transformationToMatrix x)

data RDrawing
  = RPrim !RNodeInfo !RPrim
  | RMany !RNodeInfo ![RDrawing]
   deriving (Show)

instance Monoid RDrawing where
  mempty = RMany mempty mempty

  -- TODO this could emit extra nodes
  mappend x y = RMany mempty (y : pure x)

  mconcat     = RMany mempty . mapReverse id


toNodeInfoT :: Transformation Double -> RNodeInfo
toNodeInfoT t = mempty { rTransf = t }
{-# INLINABLE toNodeInfoT #-}

toNodeInfoS :: Style -> RNodeInfo
toNodeInfoS t = mempty { rStyle = t }
{-# INLINABLE toNodeInfoS #-}

toNodeInfoH :: Handlers -> RNodeInfo
toNodeInfoH t = mempty { rHandler = t }
{-# INLINABLE toNodeInfoH #-}

instance Monoid RNodeInfo where
  mempty = RNodeInfo mempty mempty mempty
  mappend (RNodeInfo a1 a2 a3) (RNodeInfo b1 b2 b3) =
    RNodeInfo (a1 <> b1) (a2 <> b2) (a3 <> b3)

printRTreeInfo :: RDrawing -> IO ()
printRTreeInfo x = do
  print $ "RTree N nodes " ++ show (drawingTreeRNNodes x)
  print $ "RTree depth   " ++ show (drawingTreeRDepth x)
drawingTreeRNNodes = drawingRTreeFold (+)
drawingTreeRDepth = drawingRTreeFold max
drawingRTreeFold :: (Int -> Int -> Int) -> RDrawing -> Int
drawingRTreeFold f = go
  where
    go (RPrim _ _)  = 1
    go (RMany _ xs) = foldr f 0 (fmap go xs)


{-| An empty and transparent drawing. Same as 'mempty'. -}
transparent :: Drawing
transparent      = Em

{-| A centered circle with radius one. -}
circle :: Drawing
circle    = Circle
{-# INLINABLE circle #-}

{-| A centered square with a width and height of one. -}
square :: Drawing
square = Rect
{-# INLINABLE square #-}


{-| An equilateral triangle. -}
triangle :: Drawing
triangle =
    Lubeck.Drawing.translate (V2 (-1/2) (-(eqTriAlt/3))) $ Lubeck.Drawing.polygon
      [V2 1 0, V2 (-1/2) eqTriAlt, V2 (-1/2) (-eqTriAlt)]
  where
    eqTriAlt = sqrt 3 / 2

{-| A centered horizontal line of length one. -}
horizontalLine :: Drawing
horizontalLine = translateX (-0.5) Line

{-| A centered vertical line of length one. -}
verticalLine :: Drawing
verticalLine = rotate (turn/4) horizontalLine

{-| Draw a sequence of line segments.

Similar to 'polygon' except endpoints are not joined.
-}
segments :: [V2 Double] -> Drawing
segments = Lines False

{-| Draw a polygon.

Similar to 'polygon' except endpoints are not joined.

Argument vectors need not have a zero sum (if they do not an implicit extra
endpoint is assumed.
-}
polygon :: [V2 Double] -> Drawing
polygon = Lines True


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
dash :: [Double] -> Drawing -> Drawing
dash = style . dashing

{-| Draw text. See also 'textWithOptions'. -}
text :: Str -> Drawing
text = Text

textStart, textMiddle, textEnd :: Str -> Drawing

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

textLeftMiddle, textMiddleMiddle, textRightMiddle :: Str -> Drawing

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

-- | Text anchor (horizontal alignment).
--
-- https://www.w3.org/TR/SVG/text.html#AlignmentProperties
data TextAnchor
  = TextAnchorStart
  | TextAnchorMiddle
  | TextAnchorEnd
  | TextAnchorInherit
  deriving (Eq, Ord, Read, Show)

instance Monoid TextAnchor where
  mappend x y
    | y == mempty = x
    | otherwise   = y
  mempty = TextAnchorInherit

-- | Text baseline (vertical) alignment.
--
-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/alignment-baseline
-- https://www.w3.org/TR/SVG/text.html#AlignmentProperties
--
-- TODO rename this type DominantBaseline (that's what it actually generates!)
data AlignmentBaseline
  = AlignmentBaselineAuto
  | AlignmentBaselineBaseline
  | AlignmentBaselineBeforeEdge
  | AlignmentBaselineTextBeforeEdge
  | AlignmentBaselineMiddle
  | AlignmentBaselineCentral
  | AlignmentBaselineAfterEdge
  | AlignmentBaselineTextAfterEdge
  | AlignmentBaselineIdeographic
  | AlignmentBaselineAlphabetic
  | AlignmentBaselineHanging
  | AlignmentBaselineMathematical
  deriving (Eq, Ord, Read, Show)

instance Monoid AlignmentBaseline where
  mappend x y
    | y == mempty = x
    | otherwise   = y
  mempty = AlignmentBaselineAuto

-- | Font style.
--
-- https://www.w3.org/TR/SVG/text.html#FontPropertiesUsedBySVG
data FontStyle
  = FontStyleNormal | FontStyleItalic | FontStyleOblique | FontStyleInherit
  deriving (Eq, Ord, Read, Show)

instance Monoid FontStyle where
  mappend x y
    | y == mempty = x
    | otherwise   = y
  mempty = FontStyleInherit

-- | Font weight.
--
-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/font-weight
data FontWeight
  = FontWeightNormal
  | FontWeightBold
  | FontWeightBolder
  | FontWeightLighter
  | FontWeightN Int
  | FontWeightInherit
  deriving (Eq, Ord, Read, Show)

instance Monoid FontWeight where
  mappend x y
    | y == mempty = x
    | otherwise   = y
  mempty = FontWeightInherit

-- Font size
--
-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/font-size
type FontSize = Str

-- | Text options. See 'textWithOptions'.
data TextOptions = TextOptions
  { textAnchor        :: !TextAnchor
  , alignmentBaseline :: !AlignmentBaseline
  , fontStyle         :: !FontStyle
  , fontFamily        :: !(First Str)
  , fontSize          :: !(First FontSize)
  , fontWeight        :: !FontWeight
  , textSelectable    :: !All
  }

-- | Left-biased. Mainly here for the 'mempty'.
instance Monoid TextOptions where
  mempty
    = TextOptions mempty mempty mempty mempty mempty mempty mempty
  mappend
    (TextOptions x1 x2 x3 x4 x5 x6 x7)
    (TextOptions y1 y2 y3 y4 y5 y6 y7)
      = TextOptions (x1 <> y1) (x2 <> y2) (x3 <> y3) (x4 <> y4) (x5 <> y5) (x6 <> y6) (x7 <> y7)
  -- TODO can we derive this?

-- | Text woth options. Idiomatically:
--
-- @
-- textWithOptions $ mempty
--    { textAnchor        = TextAnchorStart
--    , alignmentBaseline = AlignmentBaselineMiddle }
-- @
--
textWithOptions :: TextOptions -> Str -> Drawing
textWithOptions opts = style allOfThem . Text
  where
    allOfThem = mconcat
      [ _fontWeight, _fontSize, _fontFamily, _fontStyle, _textAnchor
      , _alignmentBaseline, _textSelectable
      ]

    _fontWeight = case fontWeight opts of
      FontWeightNormal          -> styleNamed "font-weight" "normal"
      FontWeightBold            -> styleNamed "font-weight" "bold"
      FontWeightBolder          -> styleNamed "font-weight" "bolder"
      FontWeightLighter         -> styleNamed "font-weight" "lighter"
      FontWeightInherit         -> styleNamed "font-weight" "inherit"
      FontWeightN n             -> styleNamed "font-weight" (toStr n)


    _fontSize = case fontSize opts of
      (First (Just v))          -> styleNamed "font-size" v
      _                         -> styleNamed "font-family" "10px"

    _fontFamily  = case fontFamily opts of
      (First (Just v))          -> styleNamed "font-family" v
      _                         -> styleNamed "font-family" "sans-serif"

    _fontStyle  = case fontStyle opts of
      FontStyleNormal           -> styleNamed "font-style" "normal"
      FontStyleItalic           -> styleNamed "font-style" "italic"
      FontStyleOblique          -> styleNamed "font-style" "oblique"
      FontStyleInherit          -> mempty

    _textAnchor = case textAnchor opts of
      TextAnchorStart           -> styleNamed "text-anchor"  "start"
      TextAnchorMiddle          -> styleNamed "text-anchor"  "middle"
      TextAnchorEnd             -> styleNamed "text-anchor"  "end"
      TextAnchorInherit         -> mempty

    _alignmentBaseline = case alignmentBaseline opts of
      AlignmentBaselineAuto           -> mempty
      AlignmentBaselineBaseline       -> styleNamed "dominant-baseline" "baseline"
      AlignmentBaselineBeforeEdge     -> styleNamed "dominant-baseline" "before-edge"
      AlignmentBaselineTextBeforeEdge -> styleNamed "dominant-baseline" "text-before-edge"
      AlignmentBaselineMiddle         -> styleNamed "dominant-baseline" "middle"
      AlignmentBaselineCentral        -> styleNamed "dominant-baseline" "central"
      AlignmentBaselineAfterEdge      -> styleNamed "dominant-baseline" "after-edge"
      AlignmentBaselineTextAfterEdge  -> styleNamed "dominant-baseline" "text-after-edge"
      AlignmentBaselineIdeographic    -> styleNamed "dominant-baseline" "ideographic"
      AlignmentBaselineAlphabetic     -> styleNamed "dominant-baseline" "alphabetic"
      AlignmentBaselineHanging        -> styleNamed "dominant-baseline" "hanging"
      AlignmentBaselineMathematical   -> styleNamed "dominant-baseline" "mathematical"

    {-
    *.unselectable {
       -moz-user-select: -moz-none;
       -khtml-user-select: none;
       -webkit-user-select: none;
       -ms-user-select: none;
       user-select: none;
    }
    -}
    _textSelectable = case textSelectable opts of
      All True  -> mempty
      All False -> mconcat
                    [ styleNamed "user-select"          "none"
                    , styleNamed "-moz-user-select"     "-moz-none"
                    , styleNamed "-khtml-user-select"   "none"
                    , styleNamed "-webkit-user-select"  "none"
                    , styleNamed "-ms-user-select"      "none"
                    -- Mouse pointer
                    , styleNamed "cursor"               "default"
                    ]

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
transform _ Em = Em
transform t dr = Transf t dr

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

{-| Scales (stretches) an object. -}
scalingX :: Num a => a -> Transformation a
scalingX a = matrix (a,0,0,1,0,0)

{-| Scales (stretches) an object. -}
scalingY :: Num a => a -> Transformation a
scalingY b = matrix (1,0,0,b,0,0)

{-| Rotates an object. A positive vale will result in a counterclockwise rotation
    and negative value in a clockwise rotation. -}
rotation :: Floating a => Angle a -> Transformation a
rotation (Radians a) = matrix (cos a, 0 - sin a, sin a, cos a, 0, 0)

-- {-| Shears an object. -}
-- shearing :: Num a => a -> a -> Transformation a
-- shearing a b = matrix (1, b, a, 1, 0, 0)

{-| Shears an object. -}
shearingX :: Num a => a -> Transformation a
shearingX a = matrix (1, 1, a, 1, 0, 0)

{-| Shears an object. -}
shearingY :: Num a => a -> Transformation a
shearingY b = matrix (1, b, 1, 1, 0, 0)

{-# INLINABLE transform #-}
{-# INLINABLE translation #-}
{-# INLINABLE translationX #-}
{-# INLINABLE translationY #-}
{-# INLINABLE scaling #-}
{-# INLINABLE scalingX #-}
{-# INLINABLE scalingY #-}
{-# INLINABLE rotation #-}
{-# INLINABLE shearingX #-}
{-# INLINABLE shearingY #-}


{-| Translate (move) an image. -}
translate :: V2 Double -> Drawing -> Drawing
-- translate (V2 dx dy) = transform $ matrix (1,0,0,1,dx,dy)
translate v = transform $ translation v

{-| Translate (move) an image along the horizonal axis.
A positive argument will move the image to the right. -}
translateX :: Double -> Drawing -> Drawing
-- translateX x = translate (V2 x 0)
translateX x = transform $ translationX x

{-| Translate (move) an image along the vertical axis.
A positive argument will move the image upwards (as opposed to standard SVG behavior). -}
translateY :: Double -> Drawing -> Drawing
-- translateY y = translate (V2 0 y)
translateY y = transform $ translationY y

{-| Scale (stretch) an image. -}
scaleXY :: V2 Double -> Drawing -> Drawing
scaleXY (V2 x y) = transform $ matrix (x,0,0,y,0,0)

{-| Scale (stretch) an image, preserving its horizontal/vertical proportion. -}
scale :: Double -> Drawing -> Drawing
-- scale x = scaleXY x x
scale a = transform $ scaling a

{-| Scale (stretch) an image horizontally. -}
scaleX :: Double -> Drawing -> Drawing
-- scaleX x = scaleXY x 1
scaleX a = transform $ scalingX a

{-| Scale (stretch) an image vertically. -}
scaleY :: Double -> Drawing -> Drawing
-- scaleY y = scaleXY 1 y
scaleY a = transform $ scalingY a

{-| Rotate an image. A positive vale will result in a counterclockwise rotation and negative value in a clockwise rotation. -}
rotate :: Angle Double -> Drawing -> Drawing
-- rotate (Radians a) = transform $ matrix (cos a, 0 - sin a, sin a, cos a, 0, 0)
-- NOTE The b,c, signs are inverted because of the reverse y polarity.
rotate a = transform $ rotation a

{-| Shear an image. -}
-- shear :: Double -> Double -> Drawing -> Drawing
-- shear a b = transform $ matrix (1, b, a, 1, 0, 0)
shearX a = transform $ shearingX a
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
smokeBackground :: Drawing
smokeBackground = fillColor Colors.whitesmoke $ scale 5000 $ square

{-| Draw the X and Y axis inside the unit square (their intersection is the origin). -}
xyAxis :: Drawing
xyAxis = strokeColor Colors.darkgreen $ strokeWidth 0.5 $
  mconcat [horizontalLine, verticalLine]

{-| Draw the X and Y axis inside the unit square, unit circle and unit square. -}
xyCoords :: Drawing
xyCoords = fillColorA (Colors.black `withOpacity` 0) $ strokeColor Colors.darkgreen $
  strokeWidth 0.5 $ mconcat [horizontalLine, verticalLine, circle, square]

-- | Draw the unit vector.
showUnitX :: Drawing
showUnitX = strokeColor Colors.red $ strokeWidth 3 $ translateX 0.5 horizontalLine

-- | Draw an image representing a direction.
showDirection :: Direction V2 Double -> Drawing
showDirection dir = scale 100 $ strokeColor Colors.red $ strokeWidth 3 $ fillColorA tp $ segments [fromDirection dir]
  where
    tp = Colors.black `withOpacity` 0

-- | Draw an image representing two boundaries of an image along the given line.
--
-- See also 'boundaries'.
showBoundaries :: V2 Double -> Drawing -> Drawing
showBoundaries v dr = case boundaries v (envelope dr) of
  Nothing -> dr
  Just (lo, hi) -> strokeColor Colors.blue (showPoint lo) <> scale 2 (showPoint hi) <> dr

-- | Draw an image representing a point.
showPoint :: Point V2 Double -> Drawing
showPoint p = translate (p .-. origin) base
  where
    base = strokeColor Colors.red $ fillColorA (Colors.black `withOpacity` 0) $strokeWidth 2 $ scale 1 $ circle

-- | Draw an image representing an envelope.
showEnvelope :: V2 Double -> Drawing -> Drawing
showEnvelope v drawing = case envelopeVMay v drawing of
  Nothing -> drawing
  Just v2 -> (rotate (angleBetween v2 unitX) $ scale (norm v2) $ unitX_T) <> drawing
  where
    -- A sideways T in the unit square, with the "top" pointing
    -- in the direction of unitX
    unitX_T = strokeWidth 2 $ strokeColor Colors.red $ fillColorA tp $ segments [V2 0 0, V2 1 0, V2 0 0.5, V2 0 (-1)]
    tp = Colors.black `withOpacity` 0

{-| Apply a style to a drawing. -}
style :: Style -> Drawing -> Drawing
style _ Em = Em
style s dr = Style s dr

{-| -}
fillColor :: Colour Double -> Drawing -> Drawing
fillColor x = style (styleNamed "fill" $ showColor x)

{-| -}
strokeColor :: Colour Double -> Drawing -> Drawing
strokeColor x = style (styleNamed "stroke" $ showColor x)

showColor = packStr . Data.Colour.SRGB.sRGB24show

{-| -}
fillColorA :: AlphaColour Double -> Drawing -> Drawing
fillColorA x = fillColor c . alpha a
  where
    alpha a = style (styleNamed "fill-opacity" $ toStr a)
    c = Data.Colour.over x Colors.black
    a = Data.Colour.alphaChannel x

-- {-| -}
strokeColorA :: AlphaColour Double -> Drawing -> Drawing
strokeColorA x = strokeColor c . alpha a
  where
    alpha a = style (styleNamed "stroke-opacity" $ toStr a)
    c = Data.Colour.over x Colors.black
    a = Data.Colour.alphaChannel x

{-| Set the stroke width. By default stroke is /not/ affected by scaling or other transformations.
-}
strokeWidth :: Double -> Drawing -> Drawing
strokeWidth x = style (styleNamed "stroke-width" (toStr x <> "px"))
-- TODO this can be overriden by setting the non-scaling-stroke attribute. Wrap in nice API?


{-| Where to place origo in the generated SVG. -}
data OriginPlacement
  = TopLeft
  | BottomLeft
  | Center
  deriving (Eq, Ord, Show)

{-| Specifies how to generate an SVG from a Drawing. -}
data RenderingOptions = RenderingOptions
  { dimensions      :: !(P2 Double)                -- ^ Dimensions. Describes a rectangle from (0,0) to the given point (x,y).
  , originPlacement :: !OriginPlacement          -- ^ Where to place origo in the generated image.
  }
  deriving (Eq, Ord, Show)

-- | Left-biased. Mainly here for the 'mempty'.
instance Monoid RenderingOptions where
  mempty  = RenderingOptions (P $ V2 800 800) Center
  mappend = const


renderDrawing :: RenderingOptions -> Drawing -> RDrawing
renderDrawing (RenderingOptions {dimensions, originPlacement}) drawing = drawingToRDrawing $ placeOrigo $ scaleY (-1) $ drawing
  where
    placeOrigo :: Drawing -> Drawing
    -- placeOrigo = id
    placeOrigo = case originPlacement of
      TopLeft     -> id
      Center      -> translateX (dx/2) . translateY (dy/2)
      BottomLeft  -> translateY (dy)

    P (V2 dx dy) = dimensions


#ifdef __GHCJS__
{-| Generate an SVG from a drawing. -}
toSvg :: RenderingOptions -> Drawing -> Svg
toSvg opts d = emitDrawing opts $ renderDrawing opts d

{-| Generate an SVG from a drawing. TODO rename emitDrawingSvg or similar -}
emitDrawing :: RenderingOptions -> RDrawing -> Svg
emitDrawing (RenderingOptions {dimensions, originPlacement}) !drawing =
  svgTopNode
    (toStr $ floor x)
    (toStr $ floor y)
    ("0 0 " <> toStr (floor x) <> " " <> toStr (floor y))
    (single $ toSvg1 $ drawing)
  where
    svgTopNode :: Str -> Str -> Str -> [Svg] -> Svg
    svgTopNode w h vb = E.svg
      [ A.width (toJSString w)
      , A.height (toJSString h)
      , A.viewBox (toJSString vb) ]

    pointsToSvgString :: [P2 Double] -> Str
    pointsToSvgString ps = toJSString $ mconcat $ Data.List.intersperse " " $ Data.List.map pointToSvgString ps
      where
        toJSString = packStr
        pointToSvgString (P (V2 x y)) = show x ++ "," ++ show y

    embedToSvg :: Embed -> Svg
    embedToSvg (EmbedContent str) = E.text (toJSString str)
    embedToSvg (EmbedNode name attrs children) =
      VD.node (toJSString name)
        (fmap (\(name, value) -> VD.attribute (toJSString name) (toJSString value)) attrs)
        (fmap embedToSvg children)

    single x = [x]
    noScale = VD.attribute "vector-effect" "non-scaling-stroke"
    -- negY (a,b,c,d,e,f) = (a,b,c,d,e,negate f)

    offsetVectorsWithOrigin p vs = p : offsetVectors p vs
    P (V2 x y) = dimensions

    -- TODO scaling all points
    -- reflY_ (V2 adx ady) = V2 adx (negate ady)
    reflY_ = id
    {-
      How to alter coordinates from Math to SVG?

      Alt 1: change all transformations
        (scalingY (-1) <>)
      and all images
        (scaleY (-1))
      (or simply emit according to SVG conventions)

      Alt 2: emit AS IS and add an extra outer transformation node that affects the WHOLE image
    -}
    toSvg1 :: RDrawing -> Svg
    toSvg1 drawing = case drawing of
      RPrim nodeInfo prim -> case prim of
        RCircle -> E.circle
          (nodeInfoToProperties nodeInfo ++ [A.r "0.5", noScale])
          []
        RRect -> E.rect
          (nodeInfoToProperties nodeInfo ++ [A.x "-0.5", A.y "-0.5", A.width "1", A.height "1", noScale])
          []
        RLine -> E.line
          ([A.x1 "0", A.y1 "0", A.x2 "1", A.y2 "0", noScale] ++ nodeInfoToProperties nodeInfo)
          []
        (RLines closed vs) -> (if closed then E.polygon else E.polyline)
          ([A.points (toJSString $ pointsToSvgString $ offsetVectorsWithOrigin (P $ V2 0 0) (map reflY_ vs)), noScale] ++ nodeInfoToProperties nodeInfo)
          []
        (RText s) -> E.g (nodeInfoToProperties nodeInfo) $ pure $ E.text'
          ([A.x "0", A.y "0", A.transform "matrix(1,0,0,-1,0,0)"])
          [E.text $ toJSString s]
        -- TODO need extra group for nodeInfo etc
        (REmbed e) -> embedToSvg e
      RMany nodeInfo rdrawings -> case fmap toSvg1 rdrawings of
        -- TODO use seq in virtual-dom too!
        nodes -> E.g (nodeInfoToProperties nodeInfo) (toList nodes)

styleToProperty :: Style -> E.Property
styleToProperty s = A.style $ toJSString $ styleToAttrString s
{-# INLINABLE styleToProperty #-}

{-
transformationToProperty :: Transformation Double -> E.Property
-- transformationToProperty t = A.transform $ "matrix" <> (toJSString . toStr) (negY $ transformationToMatrix t) <> ""
--   where
--     negY (a,b,c,d,e,f) = (a,b,c,d,e,negate f)

transformationToProperty (TF (V3 (V3 a c e) (V3 b d f) _)) = A.transform $ toJSString $ "matrix("<>toStr a<>","<>toStr b<>","<>toStr c<>","<>toStr d<>","<>toStr e<>","<>toStr (negate f)<>")"
{-# INLINABLE transformationToProperty #-}
-}

transformationToProperty :: Transformation Double -> E.Property
transformationToProperty !(TF (V3 (V3 a c e) (V3 b d f) _)) =
  VD.attribute "transform" (js_transformationToProperty_opt a b c d e f)
{-# INLINABLE transformationToProperty #-}

nodeInfoToProperties :: RNodeInfo -> [E.Property]
nodeInfoToProperties (RNodeInfo style transf handlers) =
  transformationToProperty transf : styleToProperty style : handlersToProperties handlers
{-# INLINABLE nodeInfoToProperties #-}


#else
toSvg :: RenderingOptions -> Drawing -> ()
toSvg _ _ = ()

emitDrawing :: RenderingOptions -> RDrawing -> ()
emitDrawing _ _ = mempty
#endif


{-| Generate an SVG from a drawing. -}
toSvgAny
  :: RenderingOptions
  -> Drawing
  -> (Str -> n)
  -> (Str -> [(Str,Str)] -> [n] -> n)
  -> n
toSvgAny (RenderingOptions {dimensions, originPlacement}) drawing mkT mkN =
  svgTopNode
    (toStr $ floor x)
    (toStr $ floor y)
    ("0 0 " <> toStr (floor x) <> " " <> toStr (floor y))
    (toSvg1 [] $ placeOrigo $ drawing)
  where
    mkA k v = (k, v)

    P (V2 x y) = dimensions

    -- svgTopNode :: Str -> Str -> Str -> [Svg] -> Svg
    svgTopNode w h vb = mkN "svg"
      [ mkA "width" w
      , mkA "height" h
      , mkA "viewBox" vb
      -- Needed for static SVG and doesn't do any harm in the DOM
      , mkA "xmlns:svg" "http://www.w3.org/2000/svg"
      , mkA "xmlns" "http://www.w3.org/2000/svg"
      ]

    placeOrigo :: Drawing -> Drawing
    placeOrigo = case originPlacement of
      TopLeft     -> id
      Center      -> translateX (x/2) . translateY (y/(-2))
      BottomLeft  -> translateY (y*(-1))

    pointsToSvgString :: [P2 Double] -> Str
    pointsToSvgString ps = toJSString $ mconcat $ Data.List.intersperse " " $ Data.List.map pointToSvgString ps
      where
        toJSString = packStr
        pointToSvgString (P (V2 x y)) = show x ++ "," ++ show y

    -- embedToSvg :: Embed -> n
    embedToSvg (EmbedContent x)    = mkT x
    embedToSvg (EmbedNode n as ns) = mkN n as (fmap embedToSvg ns)

    -- toSvg1 :: [(Str, Str)] -> Drawing -> [Svg]
    toSvg1 ps x = let
        single x = [x]
        noScale = mkA "vector-effect" "non-scaling-stroke"
        negY (a,b,c,d,e,f) = (a,b,c,d,e,negate f)
        offsetVectorsWithOrigin p vs = p : offsetVectors p vs
        reflY (V2 adx ady) = V2 adx (negate ady)
      in case x of
          Circle     -> single $ mkN "circle"
            ([mkA "r" "0.5", noScale]++ps)
            []
          Rect       -> single $ mkN "rect"
            ([mkA "x" "-0.5", mkA "y" "-0.5", mkA "width" "1", mkA "height" "1", noScale]++ps)
            []
          Line -> single $ mkN "line"
            ([mkA "x1" "0", mkA "y1" "0", mkA "x2" "1", mkA "y2" "0", noScale]++ps)
            []
          (Lines closed vs) -> single $ (if closed then mkN "polygon" else mkN "polyline")
            ([mkA "points" (pointsToSvgString $ offsetVectorsWithOrigin (P $ V2 0 0) (fmap reflY vs)), noScale]++ps)
            []
          Text s -> single $ mkN "text"
            ([mkA "x" "0", mkA "y" "0"]++ps)
            [mkT s]
          Embed e -> single $ embedToSvg e

          -- Don't render properties applied to Transf/Style on the g node, propagate to lower level instead
          -- As long as it is just event handlers, it doesn't matter
          Transf t x -> single $ mkN "g"
            [mkA "transform" $ "matrix" <> toStr (negY $ transformationToMatrix t) <> ""]
            (toSvg1 ps x)
          Style s x  -> single $ mkN "g"
            [mkA "style" $ styleToAttrString s]
            (toSvg1 ps x)
          -- Ignore event handlers
          Handlers _ x -> toSvg1 ps x

          -- No point in embedding handlers to empty groups, but render anyway
          Em         -> single $ mkN "g" ps []
          -- Event handlers applied to a group go on the g node
          -- Ap x y     -> single $ mkN "g" ps (toSvg1 [] y ++ toSvg1 [] x)
          Ap xs      -> single $ mkN "g" ps (mconcat $ fmap (toSvg1 []) $ reverse xs)


toSvgStr :: RenderingOptions -> Drawing -> Str
toSvgStr st dr = toSvgAny st dr id $
        \name attrs nodes -> "<" <> name <> " "
          <> mconcat (Data.List.intersperse " " $ fmap (\(k,v) -> k <> "=\"" <> v <> "\"") attrs)
          <> ">"
          <> mconcat nodes <> "</" <> name <> ">"



#ifdef __GHCJS__

foreign import javascript unsafe "'matrix('+$1+','+$2+','+$3+','+$4+','+$5+','+$6+')'"
  js_transformationToProperty_opt :: Double -> Double -> Double -> Double -> Double -> Double -> JSString

#endif

-- Evaluate a list (but not its elements) before returning second argument
seqList !xs b = case xs of { [] -> b ; (x:xs) -> seqList xs b }
-- Evaluate a list (and its elements to WHNF) before returning second argument
seqListE !xs b = case xs of { [] -> b ; (x:xs) -> seq x (seqListE xs b) }
