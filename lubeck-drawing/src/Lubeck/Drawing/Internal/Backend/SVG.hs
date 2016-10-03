
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

  -- ** Debug
  , drawingTreeInfo
  , SVGDrawingTreeInfo(..)

  -- ** High-performance
  , RenderedSVGThing
  , RD
  , runRD
  , renderSVGDrawing
  , emitSVGDrawing
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

-- import Control.Monad
-- import GHCJS.Types(JSVal, JSString)
-- import Data.JSString(pack)
--
-- import BasePrelude hiding (empty, rotate)
-- import System.Mem.Weak(addFinalizer)
-- import System.Mem
-- import Control.Monad.Random
-- import Control.Monad.Random.Class
-- import GHCJS.Foreign.Callback as CB
-- import Control.Monad.Reader
-- import Control.Monad.Reader.Class
-- import Linear.Vector
-- import Linear.Affine
-- import Linear.Matrix hiding (translation)
-- import Linear.Metric
-- import Linear.V0
-- import Linear.V1
-- import Linear.V2
-- import Linear.V3
-- import Linear.V4
-- import qualified Linear.V1
-- import qualified Linear.V2
-- import qualified Linear.V3
-- import qualified Linear.V4
-- #if MIN_VERSION_linear(1,20,0)
-- #else
-- import Linear.Epsilon
-- #endif

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
import Web.VirtualDom.Svg (Svg)
import qualified Web.VirtualDom as VD
import qualified Web.VirtualDom.Svg as E
import qualified Web.VirtualDom.Svg.Attributes as A
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


-- Rendering

{-|

RenderedSVGThing is a tree similar to SVGDrawing, with some differences:

- Child nodes (in RMany) are stored in order bottom-top instead of top-bottom

- Instead of having explicit nodes for transform/style/handlers, each node
  contains a RNodeInfo object storing all three of them.

  When converting SVGDrawings to RenderedSVGThings, we collapse all style/transform/handlers
  to the the next descending group or node, for example:

    T t1 (T t2 (H h (Ap [...]))) -> RMany (t <> t2, mempty, h)

- Masks and Gradients are hoisted to the top and assigned unique identities.


When rendering to SVG
- Each RTopInfo corresponds to a sub-tree <defs> entry
- Each RenderedSVGThing corresponds to a node in the SVG tree

-}

data RTopInfo
  = RTopGradient !Str !Gradient

data RenderedSVGThing
  = RPrim !RNodeInfo !RPrim
  | RMany !RNodeInfo ![RenderedSVGThing]
   deriving (Show)

instance Monoid RenderedSVGThing where
  mempty = RMany mempty mempty

  -- TODO this could emit extra nodes
  mappend x y = RMany mempty (y : pure x)
  mconcat     = RMany mempty . mapReverse id

data RPrim
   = RCircle
   | RCircleSector !(Angle Double) !(Angle Double)
   | RRect
   | RRectRounded !Double !Double !Double !Double
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
    , rFill    :: First Str
    }
   deriving (Show)

instance Monoid RNodeInfo where
  mempty = RNodeInfo mempty mempty mempty mempty
  mappend (RNodeInfo a1 a2 a3 a4) (RNodeInfo b1 b2 b3 b4) =
    RNodeInfo (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)

-- | Render monad
-- Writer for RTopInfo values
--
newtype RD a = RD { runRD_ :: WriterT [RTopInfo] (StateT Int Identity) a }
  deriving (Functor, Applicative, Monad, MonadWriter [RTopInfo], MonadState Int)

runRD :: RD a -> (a, [RTopInfo])
runRD x = fst $ runState (runWriterT $ runRD_ x) 0

newId :: RD Str
newId = do
  i <- get
  put $ i + 1
  pure $ "id" <> toStr i

writeTopInfo :: RTopInfo -> RD ()
writeTopInfo x = tell [x]


renderSVGDrawing :: RenderingOptions -> SVGDrawing -> RD RenderedSVGThing
renderSVGDrawing (RenderingOptions {dimensions, originPlacement}) drawing = drawingToRenderedSVGThing' mempty $ placeOrigo $ scaleY (-1) $ drawing
  where
    placeOrigo :: SVGDrawing -> SVGDrawing
    -- placeOrigo = id
    placeOrigo = case originPlacement of
      TopLeft     -> id
      Center      -> translateX (dx/2) . translateY (dy/2)
      BottomLeft  -> translateY (dy)

    P (V2 dx dy) = dimensions

    {-
      TODO this needs to be mapped in an Int state (counting maskIds) and a writer (of [(Int, SVGDrawing)]) for emitted masks
    -}
    drawingToRenderedSVGThing' :: RNodeInfo -> SVGDrawing -> RD RenderedSVGThing
    drawingToRenderedSVGThing' nodeInfo x = case x of
        Circle                 -> pure $ RPrim nodeInfo RCircle
        CircleSector r1 r2     -> pure $ RPrim nodeInfo $ RCircleSector r1 r2
        Rect                   -> pure $ RPrim nodeInfo RRect
        RectRounded x y rx ry  -> pure $ RPrim nodeInfo $ RRectRounded x y rx ry
        Line                   -> pure $ RPrim nodeInfo RLine
        (Lines closed vs)      -> pure $ RPrim nodeInfo (RLines closed vs)
        (Text t)               -> pure $ RPrim nodeInfo (RText t)
        (Embed e)              -> pure $ RPrim nodeInfo (REmbed e)

        -- TODO render masks properly
        -- This code just renders it as a group
        (Mask x y)             -> do
          ys <- mapMReverse recur [x, y]
          pure $ RMany nodeInfo ((\x -> seqListE x x) $ ys)
          where
            recur = drawingToRenderedSVGThing' mempty

        (Transf t x)           -> drawingToRenderedSVGThing' (nodeInfo <> transformationToNodeInfo t) x
        (Style s x)            -> drawingToRenderedSVGThing' (nodeInfo <> styleToNodeInfo s) x
        (SpecialStyle (FillGradient g) x) -> do
          s <- newId
          writeTopInfo $ RTopGradient s g
          drawingToRenderedSVGThing' (nodeInfo <> fillToNodeInfo s) x

        (Handlers h x)         -> drawingToRenderedSVGThing' (nodeInfo <> handlerToNodeInfo h) x
        Em                     -> pure mempty

        -- TODO could probably be optimized by some clever redifinition of the SVGDrawing monoid
        -- current RNodeInfo data render on this node alone, so further invocations uses (recur mempty)
        -- TODO return empty if concatMap returns empty list
        (Ap xs)   -> do
          ys <- mapMReverse recur xs
          pure $ RMany nodeInfo ((\x -> seqListE x x) ys)
          where
            recur = drawingToRenderedSVGThing' mempty

    fillToNodeInfo ::  Str -> RNodeInfo
    fillToNodeInfo s = mempty { rFill = First (Just s) }
    {-# INLINABLE fillToNodeInfo #-}

    transformationToNodeInfo :: Transformation Double -> RNodeInfo
    transformationToNodeInfo t = mempty { rTransf = t }
    {-# INLINABLE transformationToNodeInfo #-}

    styleToNodeInfo :: Style -> RNodeInfo
    styleToNodeInfo t = mempty { rStyle = t }
    {-# INLINABLE styleToNodeInfo #-}

    handlerToNodeInfo :: Handlers -> RNodeInfo
    handlerToNodeInfo t = mempty { rHandler = t }
    {-# INLINABLE handlerToNodeInfo #-}



#ifdef __GHCJS__
{-| Generate an SVG from a drawing. -}
toSvg :: RenderingOptions -> SVGDrawing -> Svg
toSvg opts d =
  let (rd, rt) = runRD $ renderSVGDrawing opts d
  in emitSVGDrawing opts rt rd

{-| Generate an SVG from a drawing. TODO rename emitSVGDrawingSvg or similar -}
emitSVGDrawing :: RenderingOptions -> [RTopInfo] -> RenderedSVGThing -> Svg
emitSVGDrawing (RenderingOptions {dimensions, originPlacement}) !topInfo !drawing =
  svgTopNode
    (toStr $ floor x)
    (toStr $ floor y)
    ("0 0 " <> toStr (floor x) <> " " <> toStr (floor y))
    [ topInfoToDefs topInfo
    , toSvg1 drawing
    ]
  where
    svgTopNode :: Str -> Str -> Str -> [Svg] -> Svg
    svgTopNode w h vb children = E.svg
      [ A.width (toJSString w)
      , A.height (toJSString h)
      , A.viewBox (toJSString vb) ] children

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

    -- single x = [x]
    noScale = VD.attribute "vector-effect" "non-scaling-stroke"
    offsetVectorsWithOrigin p vs = p : offsetVectors p vs
    P (V2 x y) = dimensions

    topInfoToDefs :: [RTopInfo] -> Svg
    topInfoToDefs ts = E.defs [] (fmap topInfoToDef ts)

    topInfoToDef :: RTopInfo -> Svg
    topInfoToDef (RTopGradient name (LinearGradient stops)) =
        E.node "linearGradient" [A.id $ toJSString name] (fmap g stops)
      where
        g (GradientStop o c) = E.node "stop"
          [ A.offset (toJSString (toStr o) <> "%")
          , A.stopColor (toJSString $ showColor c <> "")
          ] []

    toSvg1 :: RenderedSVGThing -> Svg
    toSvg1 drawing = case drawing of
      RPrim nodeInfo prim -> case prim of
        RCircle -> E.circle
          (nodeInfoToProperties nodeInfo ++ [A.r "0.5", noScale])
          []
        -- Where S and O in [0..1] , <circle r="R" stroke-width="R*2" stroke-dasharray="S*(pi*R*2) (pi*R*2)" transform="rotate(O*360)">
        RCircleSector a1 a2 ->
          let (s, o) = anglesToPolarScaleOffset a1 a2
          in
            E.g (nodeInfoToProperties nodeInfo) $ pure
              $ E.circle
                [ A.r "0.5", A.strokeWidth "1"
                , A.strokeDasharray $ (toJSString $ toStr (s*pi)) <> " " <> (toJSString $ toStr pi)
                , A.transform $ "rotate("<>(toJSString $ toStr $ o * 360) <> ")"
                ]
            []
        RRect -> E.rect
          (nodeInfoToProperties nodeInfo ++ [A.x "-0.5", A.y "-0.5", A.width "1", A.height "1", noScale])
          []
        RRectRounded w h rx ry -> E.rect
          (nodeInfoToProperties nodeInfo ++
            [ A.x (toJSString $ toStr $ negate w / 2)
            , A.y (toJSString $ toStr $ negate h / 2)
            , A.width (toJSString $ toStr $ w)
            , A.height (toJSString $ toStr $ h)
            , A.rx (toJSString $ toStr $ rx)
            , A.ry (toJSString $ toStr $ ry)
            , noScale]
            )
          []
        RLine -> E.line
          ([A.x1 "0", A.y1 "0", A.x2 "1", A.y2 "0", noScale] ++ nodeInfoToProperties nodeInfo)
          []
        (RLines closed vs) -> (if closed then E.polygon else E.polyline)
          ([A.points (toJSString $ pointsToSvgString $ offsetVectorsWithOrigin (P $ V2 0 0) vs), noScale] ++ nodeInfoToProperties nodeInfo)
          []
        (RText s) -> E.g (nodeInfoToProperties nodeInfo) $ pure $ E.text'
          ([A.x "0", A.y "0", A.transform "matrix(1,0,0,-1,0,0)"])
          [E.text $ toJSString s]
        -- TODO need extra group for nodeInfo etc
        (REmbed e) -> embedToSvg e
      RMany nodeInfo rdrawings -> case fmap toSvg1 rdrawings of
        -- TODO use seq in virtual-dom too!
        nodes -> E.g (nodeInfoToProperties nodeInfo) (toList nodes)

{-# INLINEABLE renderSVGDrawing #-}

nodeInfoToProperties :: RNodeInfo -> [E.Property]
nodeInfoToProperties (RNodeInfo style transf handlers fill) =
  transformationToProperty transf : styleToProperty style <> fillToProperty fill <> handlersToProperties handlers
  where
    styleToProperty :: Style -> [E.Property]
    styleToProperty s
      -- TODO handle null case, see #132
      -- | Map.null s = []
      = [A.style $ toJSString $ styleToAttrString s]

    transformationToProperty :: Transformation Double -> E.Property
    transformationToProperty !(TF (V3 (V3 a c e) (V3 b d f) _)) =
      VD.attribute "transform" (js_transformationToProperty_opt a b c d e f)

    fillToProperty :: First Str -> [E.Property]
    fillToProperty (First Nothing)   = []
    fillToProperty (First (Just id)) = [VD.attribute "fill" ("url(#" <> toJSString id <> ")")]

{-# INLINABLE nodeInfoToProperties #-}


#else
toSvg :: RenderingOptions -> SVGDrawing -> ()
toSvg _ _ = ()

emitSVGDrawing :: RenderingOptions -> RenderedSVGThing -> ()
emitSVGDrawing _ _ = mempty
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

    placeOrigo :: SVGDrawing -> SVGDrawing
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

    -- toSvg1 :: [(Str, Str)] -> SVGDrawing -> [Svg]
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
          CircleSector a1 a2 ->
            let (s, o) = anglesToPolarScaleOffset a1 a2
            in
               single $ mkN "circle"
                  [ mkA "r" "0.5", mkA "stroke-width" "1"
                  , mkA "stroke-dasharray" $ (toStr (s*pi)) <> " " <> (toStr pi)
                  , mkA "transform" $ "rotate("<>(toStr $ o * 360) <> ")"
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
            ([mkA "points" (pointsToSvgString $ offsetVectorsWithOrigin (P $ V2 0 0) (fmap reflY vs)), noScale]++ps)
            []
          Text s -> single $ mkN "text"
            ([mkA "x" "0", mkA "y" "0"]++ps)
            [mkT s]
          Embed e -> single $ embedToSvg e

          -- Don't render properties applied to Transf/Style on the g node, propagate to lower level instead
          -- As long as it is just event handlers, it doesn't matter
          Mask _ x ->
            (toSvg1 ps x)
          Transf t x -> single $ mkN "g"
            [mkA "transform" $ "matrix" <> toStr (negY $ transformationToMatrix t) <> ""]
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









#ifdef __GHCJS__

foreign import javascript unsafe "'matrix('+$1+','+$2+','+$3+','+$4+','+$5+','+$6+')'"
  js_transformationToProperty_opt :: Double -> Double -> Double -> Double -> Double -> Double -> JSString

#endif

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
-- translate (V2 dx dy) = transform $ matrix (1,0,0,1,dx,dy)
translate v = transform $ translation v

{-| Translate (move) an image along the horizonal axis.
A positive argument will move the image to the right. -}
translateX :: Double -> SVGDrawing -> SVGDrawing
-- translateX x = translate (V2 x 0)
translateX x = transform $ translationX x

{-| Translate (move) an image along the vertical axis.
A positive argument will move the image upwards (as opposed to standard SVG behavior). -}
translateY :: Double -> SVGDrawing -> SVGDrawing
-- translateY y = translate (V2 0 y)
translateY y = transform $ translationY y

{-| Scale (stretch) an image. -}
scaleXY :: V2 Double -> SVGDrawing -> SVGDrawing
scaleXY (V2 x y) = transform $ matrix (x,0,0,y,0,0)

{-| Scale (stretch) an image, preserving its horizontal/vertical proportion. -}
scale :: Double -> SVGDrawing -> SVGDrawing
-- scale x = scaleXY x x
scale a = transform $ scaling a

{-| Scale (stretch) an image horizontally. -}
scaleX :: Double -> SVGDrawing -> SVGDrawing
-- scaleX x = scaleXY x 1
scaleX a = transform $ scalingX a

{-| Scale (stretch) an image vertically. -}
scaleY :: Double -> SVGDrawing -> SVGDrawing
-- scaleY y = scaleXY 1 y
scaleY a = transform $ scalingY a

{-| Rotate an image. A positive vale will result in a counterclockwise rotation and negative value in a clockwise rotation. -}
rotate :: Angle Double -> SVGDrawing -> SVGDrawing
-- rotate (Radians a) = transform $ matrix (cos a, 0 - sin a, sin a, cos a, 0, 0)
-- NOTE The b,c, signs are inverted because of the reverse y polarity.
rotate a = transform $ rotation a

{-| Shear an image. -}
-- shear :: Double -> Double -> SVGDrawing -> SVGDrawing
-- shear a b = transform $ matrix (1, b, a, 1, 0, 0)
shearX :: Double -> SVGDrawing -> SVGDrawing
shearX a = transform $ shearingX a

shearY :: Double -> SVGDrawing -> SVGDrawing
shearY a = transform $ shearingY a
