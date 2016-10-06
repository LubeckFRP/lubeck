
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
  , NoImplicitPrelude
  , GeneralizedNewtypeDeriving
  , CPP
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
  -O3
  #-}

module Lubeck.Drawing.Internal.Backend.SVG
  ( SVGDrawing(..)
  , Embed(..)
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

#ifdef __GHCJS__
import GHCJS.Types(JSVal, JSString)
import qualified Web.VirtualDom.Svg as VD
import qualified Web.VirtualDom as VD
import qualified Web.VirtualDom.Svg as VdSvg
import qualified Web.VirtualDom.Svg.Attributes as VdSvg
#endif



{-|
Embedded SVG node.
-}
data Embed
  = EmbedNode Str [(Str, Str)] [Embed]
  | EmbedContent Str
  deriving (Eq, Show)


{-|
  A drawing is an infinite two-dimensional image, which supports arbitrary scaling transparency.

  Semantically
  @
  -- TODO specify full semantics for mouse/touch interaction
  Drawing ~ ( P2 -> AlphaColour, Behavior P2 -> Events () -> ?)
  @s

  TODO semantics for mouse interaction.

  Because the image is infinite, basic images have simple proportions, for example 'circle', 'square',
  and 'horizontalLine' all have a width of one. To obtain other sizes, use the 'transform' or 'scale' functions.

  Every image has a notion of a local origin, or "midpoint". Transformations such as scaling, rotation and
  reflection are carried out with respect to the local origin. By default, most shapes are centered around the
  local origin (or to put it differently: the local origin is the midpoint of the image). To move an image, use
  the 'translate' functions.

  Images can be composed using the 'Monoid' instance, which overlays the two images so that their origins match exactly.
-}
data SVGDrawing
  = Circle
  | CircleSector !(Angle Double) !(Angle Double)
  | Rect
  | RectRounded !Double !Double !Double !Double
  -- A line along the unit vector
  | Line
  -- A sequence of straight lines, closed or not. For closed lines, there is no need
  -- to return the original point (i.e. the sum of the vectors does not have to be zeroV).
  | Lines !Bool ![V2 Double]
  | Text !Str

  | Embed Embed

  | Mask !SVGDrawing !SVGDrawing
  | Transf !(Transformation Double) !SVGDrawing
  | Style !Style !SVGDrawing
  | SpecialStyle !SpecialStyle !SVGDrawing
  | Handlers !Handlers !SVGDrawing

  | Em
  -- Compose drawings
  -- Left-most is top-most.
  | Ap ![SVGDrawing]

data SVGDrawingTreeInfo = SVGDrawingTreeInfo
  { numberOfNodes :: Int -- ^ Number of nodes in the internal tree representing the drawing, not counting embedded SVGs.
  }
  deriving (Show)

drawingTreeInfo :: SVGDrawing -> SVGDrawingTreeInfo
drawingTreeInfo drawing = SVGDrawingTreeInfo (numNodes drawing)
  where
    numNodes (Mask a b) = numNodes a + numNodes b
    numNodes (Ap as)    = sum $ fmap numNodes as
    numNodes _          = 1

instance Monoid SVGDrawing where
  mempty  = Em

  mappend Em x = x
  mappend x Em = x
  mappend (Ap xs) (Ap ys) = Ap (xs <> ys)
  mappend x (Ap ys) = Ap (x : ys)
  mappend (Ap xs) y = Ap (xs <> pure y)
  mappend x y = Ap [x, y]
  mconcat = Ap


#ifdef __GHCJS__
-- | Does nothing in GHC. In GHCJS, render a 'SVGDrawing' as a virtual dom tree.
toSvg :: RenderingOptions -> SVGDrawing -> VD.Svg
toSvg opts d = toSvgAny opts d
  (VdSvg.text . toJSString)
  (\n ps -> VdSvg.node (toJSString n) (g <$> ps))
  where
    g (name, val) = VD.attribute (toJSString name) (toJSString val)

#else
-- | Does nothing in GHC. In GHCJS, render a 'SVGDrawing' as a virtual dom tree.
toSvg :: RenderingOptions -> SVGDrawing -> ()
toSvg _ _ = ()
#endif

{-| Generate an SVG from a drawing. -}
toSvgAny
  :: RenderingOptions
  -> SVGDrawing
  -> (Str -> n)
  -> (Str -> [(Str,Str)] -> [n] -> n)
  -> n
toSvgAny (RenderingOptions {dimensions, originPlacement}) drawing mkT mkN =
  svgTopNode
    (toStr $ floor x)
    (toStr $ floor y)
    ("0 0 " <> toStr (floor x) <> " " <> toStr (floor y))
    (toSvg1 [] $ placeOrigo $ scaleXY (V2 1 (-1)) $ drawing)
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

    placeOrigo :: SVGDrawing -> SVGDrawing
    placeOrigo = case originPlacement of
      -- _ -> id
      TopLeft     -> id
      Center      -> translate $ V2 (x/2) (y/2)
      BottomLeft  -> translate $ V2 0 y

    pointsToSvgString :: [P2 Double] -> Str
    pointsToSvgString ps = toJSString $ mconcat $ Data.List.intersperse " " $ Data.List.map pointToSvgString ps
      where
        toJSString = packStr
        pointToSvgString (P (V2 x y)) = show x ++ "," ++ show y

    -- embedToSvg :: Embed -> n
    embedToSvg (EmbedContent x)    = mkT x
    embedToSvg (EmbedNode n as ns) = mkN n as (fmap embedToSvg ns)

    -- toSvg1 :: [(Str, Str)] -> SVGDrawing -> [Svg]
    toSvg1 ps x = let
        single x = [x]
        noScale = mkA "vector-effect" "non-scaling-stroke"
        -- negY (a,b,c,d,e,f) = (a,b,c,d,e,negate f)
        offsetVectorsWithOrigin p vs = p : offsetVectors p vs
        -- reflY (V2 adx ady) = V2 adx (negate ady)
      in case x of
          Circle     -> single $ mkN "circle"
            ([mkA "r" "0.5", noScale]++ps)
            []
          CircleSector a1 a2 ->
            let (s, o) = anglesToPolarScaleOffset a1 a2
            in
               single $ mkN "circle"
                  [ mkA "r" "0.5", mkA "stroke-width" "1"
                  , mkA "stroke-dasharray" $ (toStr (s*pi)) <> " " <> (toStr pi)
                  , mkA "transform" $ "rotate("<>(toStr $ o * 360) <> ")"
                  , noScale
                  ]
              []
          Rect       -> single $ mkN "rect"
            ([mkA "x" "-0.5", mkA "y" "-0.5", mkA "width" "1", mkA "height" "1", noScale]++ps)
            []
          RectRounded w h rx ry -> single $ mkN "rect"
            (
              [ mkA "x" (toStr $ negate w / 2)
              , mkA "y" (toStr $ negate h / 2)
              , mkA "width" (toStr w)
              , mkA "height" (toStr h)
              , mkA "rx" (toStr rx)
              , mkA "ry" (toStr ry)
              , noScale
              ]++ps)
            []
          Line -> single $ mkN "line"
            ([mkA "x1" "0", mkA "y1" "0", mkA "x2" "1", mkA "y2" "0", noScale]++ps)
            []
          (Lines closed vs) -> single $ (if closed then mkN "polygon" else mkN "polyline")
            ([mkA "points" (pointsToSvgString $ offsetVectorsWithOrigin (P $ V2 0 0) (vs)), noScale]++ps)
            []

          Text s -> single $ mkN "text"
            ([ mkA "x" "0", mkA "y" "0"
             , mkA "transform" $ "scale(1,-1)"
             ]++ps)
            [mkT s]
          Embed e -> single $ embedToSvg e

          -- Don't render properties applied to Transf/Style on the g node, propagate to lower level instead
          -- As long as it is just event handlers, it doesn't matter
          Mask _ x ->
            (toSvg1 ps x)
          Transf t x -> single $ mkN "g"
            [mkA "transform" $ "matrix" <> toStr (transformationToMatrix t) <> ""]
            (toSvg1 ps x)
          Style s x  -> single $ mkN "g"
            [mkA "style" $ styleToAttrString s]
            (toSvg1 ps x)

          -- TODO should render special styles (such as gradients)
          -- Ignored until #133
          SpecialStyle _ x -> toSvg1 ps x
          -- Ignore event handlers
          Handlers _ x -> toSvg1 ps x

          -- No point in embedding handlers to empty groups, but render anyway
          Em         -> single $ mkN "g" ps []
          -- Event handlers applied to a group go on the g node
          -- Ap x y     -> single $ mkN "g" ps (toSvg1 [] y ++ toSvg1 [] x)
          Ap xs      -> single $ mkN "g" ps (mconcat $ fmap (toSvg1 []) $ reverse xs)


toSvgStr :: RenderingOptions -> SVGDrawing -> Str
toSvgStr st dr = toSvgAny st dr id $
        \name attrs nodes -> "<" <> name <> " "
          <> mconcat (Data.List.intersperse " " $ fmap (\(k,v) -> k <> "=\"" <> v <> "\"") attrs)
          <> ">"
          <> mconcat nodes <> "</" <> name <> ">"









-- #ifdef __GHCJS__
--
-- foreign import javascript unsafe "'matrix('+$1+','+$2+','+$3+','+$4+','+$5+','+$6+')'"
--   js_transformationToProperty_opt :: Double -> Double -> Double -> Double -> Double -> Double -> JSString
--
-- #endif

-- | Evaluate a list (but not its elements) before returning second argument
seqList :: [a] -> b -> b
seqList !xs b = case xs of { [] -> b ; (x:xs) -> seqList xs b }

-- | Evaluate a list (and its elements to WHNF) before returning second argument
seqListE :: [a] -> b -> b
seqListE !xs b = case xs of { [] -> b ; (x:xs) -> seq x (seqListE xs b) }

-- | Fast equivalent of @\f -> map f . reverse@
mapReverse :: (a -> b) -> [a] -> [b]
mapReverse f l =  rev l mempty
  where
    -- rev :: [a] -> [b] -> [b]
    rev []     a = a
    rev (x:xs) a = rev xs (f x : a)

mapMReverse :: Monad m => (a -> m b) -> [a] -> m [b]
mapMReverse f xs = mapM f (reverse xs)
-- TODO faster version a la the above

-- SLOW
showColor :: Colour Double -> Str
showColor = packStr . Data.Colour.SRGB.sRGB24show






transform :: Transformation Double -> SVGDrawing -> SVGDrawing
transform t dr = Transf t dr
{-# INLINABLE transform #-}

{-| Translate (move) an image. -}
translate :: V2 Double -> SVGDrawing -> SVGDrawing
translate (V2 a b) = transform $ matrix (1,0,0,1,a,b)

{-| Scale (stretch) an image. -}
scaleXY :: V2 Double -> SVGDrawing -> SVGDrawing
scaleXY (V2 x y) = transform $ matrix (x,0,0,y,0,0)

-- {-| Scale (stretch) an image, preserving its horizontal/vertical proportion. -}
-- scale :: Double -> SVGDrawing -> SVGDrawing
-- -- scale x = scaleXY x x
-- scale a = transform $ scaling a


-- {-| Rotate an image. A positive vale will result in a counterclockwise rotation and negative value in a clockwise rotation. -}
-- rotate :: Angle Double -> SVGDrawing -> SVGDrawing
-- -- rotate (Radians a) = transform $ matrix (cos a, 0 - sin a, sin a, cos a, 0, 0)
-- -- NOTE The b,c, signs are inverted because of the reverse y polarity.
-- rotate a = transform $ rotation a
--
-- {-| Shear an image. -}
-- -- shear :: Double -> Double -> SVGDrawing -> SVGDrawing
-- -- shear a b = transform $ matrix (1, b, a, 1, 0, 0)
-- shearX :: Double -> SVGDrawing -> SVGDrawing
-- shearX a = transform $ shearingX a
--
-- shearY :: Double -> SVGDrawing -> SVGDrawing
-- shearY a = transform $ shearingY a
