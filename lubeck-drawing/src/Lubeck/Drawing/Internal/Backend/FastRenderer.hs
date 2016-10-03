
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

#ifdef __GHCJS__
module Lubeck.Drawing.Internal.Backend.FastRenderer
  -- ( FastDrawing
  -- )
  where

import GHCJS.Types(JSVal, JSString)

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

import Data.JSString(pack)
import System.Mem.Weak(addFinalizer)
import System.Mem
import GHCJS.Foreign.Callback as CB
import Control.Monad.Reader
import Control.Monad.Reader.Class

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

import Lubeck.Drawing.Types hiding (rect)
import Lubeck.Drawing.Style
import Lubeck.Drawing.Handlers
import Lubeck.Drawing.Text
import Lubeck.Drawing.Transformation

import GHCJS.Types(JSVal, JSString)


newtype CanvasElement = DOMCanvasElement JSVal
newtype Context = Context JSVal
newtype Renderer = Renderer JSVal

-- TODO this name is wrong, Rendered should be Transfered
newtype RenderedFastSegment = RenderedFastSegment JSVal
newtype TransferedFastDrawing = TransferedFastDrawing JSVal

newtype MouseEvent = MouseEvent JSVal
foreign import javascript unsafe "$1.movementX"
  movementX :: MouseEvent -> Double
foreign import javascript unsafe "$1.movementY"
  movementY :: MouseEvent -> Double
foreign import javascript unsafe "$1.screenX"
  screenX :: MouseEvent -> Double
foreign import javascript unsafe "$1.screenY"
  screenY :: MouseEvent -> Double
foreign import javascript unsafe "$1.offsetX"
  offsetX :: MouseEvent -> Double
foreign import javascript unsafe "$1.offsetY"
  offsetY :: MouseEvent -> Double


foreign import javascript unsafe
  "var n = document.createElement('canvas'); n.id = 'canvas'; n.width = 800; n.height = 800; document.getElementsByTagName('body')[0].appendChild(n)"
  createCanvasNode :: IO ()
foreign import javascript unsafe
  "var loop = function() { update(); requestAnimationFrame(loop) } ; loop()"
  startLoop :: IO ()
foreign import javascript unsafe "window.update = $1"
  setUpdateCB :: (Callback (IO ())) -> IO ()
foreign import javascript unsafe "$1.onmousemove = $2"
  setMousemoveCB :: CanvasElement -> (Callback (JSVal -> IO ())) -> IO ()
foreign import javascript unsafe "$1.onmouseup = $2"
  setMouseupCB :: CanvasElement -> (Callback (JSVal -> IO ())) -> IO ()
foreign import javascript unsafe "$1.onmousedown = $2"
  setMousedownCB :: CanvasElement -> (Callback (JSVal -> IO ())) -> IO ()
-- TODO variants of asyncCallback et al to allow arbitrary newtype wrappers


foreign import javascript unsafe "document.getElementById('canvas')"
  getCanvas :: IO CanvasElement
foreign import javascript unsafe "$1.getContext('2d')"
  get2DContext :: CanvasElement -> IO Context
foreign import javascript unsafe "createRenderer($1)"
  createRenderer :: Context -> IO Renderer
foreign import javascript unsafe "$1.clearRect($2,$3,$4,$5);"
  clearRect :: Context -> Double -> Double -> Double -> Double -> IO ()


foreign import javascript unsafe "console.log($1)"
  showRenderer :: Renderer -> IO ()
foreign import javascript unsafe "console.log($1)"
  showTransferedFastDrawing :: TransferedFastDrawing -> IO ()

foreign import javascript unsafe "$1.render(0,$2)"
  renderTransferedFastDrawing :: Renderer -> TransferedFastDrawing -> IO ()
{-# INLINABLE renderTransferedFastDrawing #-}

foreign import javascript unsafe "$1.getPointTag_(0,$2,$3,$4)"
  getPointTag' :: Renderer -> TransferedFastDrawing -> Double -> Double -> IO Int
{-# INLINABLE getPointTag #-}

-- Never call these from high-level API, should be managed by addFinalizer

foreign import javascript unsafe "$1.claim($2)"
  claimD :: Renderer -> TransferedFastDrawing -> IO ()
{-# INLINABLE claimD #-}
foreign import javascript unsafe "$1.release($2)"
  releaseD :: Renderer -> TransferedFastDrawing -> IO ()
{-# INLINABLE releaseD #-}
foreign import javascript unsafe "$1.release($2)"
  releaseS :: Renderer -> RenderedFastSegment -> IO ()
{-# INLINABLE releaseS #-}


type WithRenderer2 a = Renderer -> IO a
type WithRenderer3 a = ReaderT Renderer IO a
newtype FastDrawing = FastDrawing { getFastDrawing :: WithRenderer3 TransferedFastDrawing }
newtype FastSegment = FastSegment { getFastSegment :: WithRenderer3 RenderedFastSegment }

-- x y w h draws a rectangle in the firt quadrant if all arguments are positive
rect :: Double -> Double -> Double -> Double -> FastDrawing
rect !x !y !w !h = FastDrawing $ (ReaderT $ \r -> rect' x y w h r) >>= finWithRenderer3

-- text x y t draws the text "t" at the given point, using the current align/baseline conventions
text :: Double -> Double -> Str -> FastDrawing
text !x !y !txt = flipY $ FastDrawing $ (ReaderT $ \r -> text' x y (toJSString txt) r) >>= finWithRenderer3
  where
    flipY = scaleXY 1 (-1)

segment :: Double -> Double -> FastSegment -> FastSegment
segment !x !y (FastSegment rtail) = FastSegment $ do
  tail <- rtail
  res <- (ReaderT $ \re -> segment' x y tail re)
  finWithRenderer3_S res

-- segment3 :: Double -> Double -> Double -> Double -> Double -> Double -> FastSegment -> FastSegment
-- segment3 !x1 !y1 !x2 !y2 !x3 !y3 (FastSegment rtail) = FastSegment $ do
--   tail <- rtail
--   res <- (ReaderT $ \re -> segment3' x1 y1 x2 y2 x3 y3 tail re)
--   finWithRenderer3_S res

-- Absolute co-ordinates, open/close
linePath :: Foldable t => Bool -> t (Double, Double) -> FastSegment
linePath close = foldr (\(x,y) r -> segment x y r) (segmentEnd close)

linePathV2 :: Foldable t => Bool -> t (V2 Double) -> FastSegment
linePathV2 close = foldr (\(V2 !x !y) r -> segment x y r) (segmentEnd close)

path :: Double -> Double -> FastSegment -> FastDrawing
path !x !y (FastSegment rpath) = FastDrawing $ do
  path <- rpath
  res <- (ReaderT $ \re -> path' x y path re)
  finWithRenderer3 res

segmentEnd :: Bool -> FastSegment
segmentEnd !close = FastSegment $ do
  res <- (ReaderT $ \re -> segmentEnd' close re)
  finWithRenderer3_S res

-- segmentSubpath :: Bool -> Double -> Double -> RenderedFastSegment -> RenderedFastSegment

-- circle x y r draws a circle with the given center and radius
circle :: Double -> Double -> Double -> FastDrawing
circle !x !y !rad = FastDrawing $ (ReaderT $ \r -> circle' x y rad r) >>= finWithRenderer3
fillColor :: Double
                 -> Double -> Double -> Double -> FastDrawing -> FastDrawing

fillColor !r !g !b !a (FastDrawing rd1) = FastDrawing $ do
  d1 <- rd1
  res <- (ReaderT $ \re -> fillColor' r g b a d1 re)
  finWithRenderer3 res
strokeColor :: Double
                 -> Double -> Double -> Double -> FastDrawing -> FastDrawing

strokeColor !r !g !b !a (FastDrawing rd1) = FastDrawing $ do
  d1 <- rd1
  res <- (ReaderT $ \re -> strokeColor' r g b a d1 re)
  finWithRenderer3 res
transf :: Double
                 -> Double
                 -> Double
                 -> Double
                 -> Double
                 -> Double
                 -> FastDrawing
                 -> FastDrawing
transf !a !b !c !d !e !f (FastDrawing rd1) = FastDrawing $ do
  d1 <- rd1
  res <- (ReaderT $ \re -> transf' a b c d e f d1 re)
  finWithRenderer3 res

-- as expected
scaleXY :: Double -> Double -> FastDrawing -> FastDrawing
scaleXY !a !b (FastDrawing rd1) = FastDrawing $ do
  d1 <- rd1
  res <- (ReaderT $ \re -> scaleXY' a b d1 re)
  finWithRenderer3 res

-- as expected
scale x = scaleXY x x

-- as expected
translate :: Double -> Double -> FastDrawing -> FastDrawing
translate !a !b (FastDrawing rd1) = FastDrawing $ do
  d1 <- rd1
  res <- (ReaderT $ \re -> translate' a b d1 re)
  finWithRenderer3 res

-- rotate x rotates by x radians, counterclockwise
rotate :: Double -> FastDrawing -> FastDrawing
rotate !a (FastDrawing rd1) = FastDrawing $ do
  d1 <- rd1
  res <- (ReaderT $ \re -> rotate' (negate a) d1 re)
  finWithRenderer3 res

textFont :: JSString -> FastDrawing -> FastDrawing
textFont !font (FastDrawing rd1) = FastDrawing $ do
  d1 <- rd1
  res <- (ReaderT $ \r -> textFont' font d1 r)
  finWithRenderer3 res

data TextAlign = TextAlignStart | TextAlignEnd | TextAlignLeft | TextAlignRight | TextAlignCenter -- TODO
  deriving (Eq, Ord, Enum, Show)
data TextBaseline
  = TextBaselineTop | TextBaselineHanging | TextBaselineMiddle
  | TextBaselineAlphabetic | TextBaselineIdeographic | TextBaselineBottom -- TODO
  deriving (Eq, Ord, Enum, Show)

textAlign :: TextAlign -> FastDrawing -> FastDrawing
textAlign !x (FastDrawing rd1) = FastDrawing $ do
  d1 <- rd1
  res <- (ReaderT $ \r -> textAlign' (fromEnum x) d1 r)
  finWithRenderer3 res

textBaseline :: TextBaseline -> FastDrawing -> FastDrawing
textBaseline !x (FastDrawing rd1) = FastDrawing $ do
  d1 <- rd1
  res <- (ReaderT $ \r -> textBaseline' (fromEnum x) d1 r)
  finWithRenderer3 res

data LineCap = LineCapButt | LineCapRound | LineCapSquare
  deriving (Eq, Ord, Enum, Show)
data LineJoin = LineJoinBevel | LineJoinRound | LineJoinMiter
  deriving (Eq, Ord, Enum, Show)

lineWidth :: Double -> FastDrawing -> FastDrawing
lineWidth !x (FastDrawing rd1) = FastDrawing $ do
  d1 <- rd1
  res <- (ReaderT $ \r -> lineWidth' x d1 r)
  finWithRenderer3 res

lineCap :: LineCap -> FastDrawing -> FastDrawing
lineCap !x (FastDrawing rd1) = FastDrawing $ do
  d1 <- rd1
  res <- (ReaderT $ \r -> lineCap' (fromEnum x) d1 r)
  finWithRenderer3 res

lineJoin :: LineJoin -> FastDrawing -> FastDrawing
lineJoin !x (FastDrawing rd1) = FastDrawing $ do
  d1 <- rd1
  res <- (ReaderT $ \r -> lineJoin' (fromEnum x) d1 r)
  finWithRenderer3 res

tag :: Int -> FastDrawing -> FastDrawing
tag !tag (FastDrawing rd2)
  | tag < 0   = error "tag: Tag must be positive"
  | otherwise = FastDrawing $ do
  d2 <- rd2
  res <- (ReaderT $ \re -> tag' (abs tag + 2) d2 re)
  finWithRenderer3 res

-- ap2 x y draws y on top of x
ap2 :: FastDrawing -> FastDrawing -> FastDrawing
ap2 (FastDrawing rd1) (FastDrawing rd2) = FastDrawing $ do
  d1 <- rd1
  d2 <- rd2
  res <- (ReaderT $ \re -> ap2' d1 d2 re)
  finWithRenderer3 res


transparent :: FastDrawing
transparent = rect 0 0 0 0

instance Monoid FastDrawing where
  mappend = flip ap2 -- Flip to get TransferedFastDrawing composition order
  mempty = transparent
  mconcat = foldi mappend mempty

foldt            :: (FastDrawing -> FastDrawing -> FastDrawing) -> FastDrawing -> [FastDrawing] -> FastDrawing
foldt f z []     = z
foldt f z [x]    = x
foldt f z xs     = foldt f z (pairs f xs)

foldi            :: (FastDrawing -> FastDrawing -> FastDrawing) -> FastDrawing -> [FastDrawing] -> FastDrawing
foldi f z []     = z
foldi f z (x:xs) = f x (foldi f z (pairs f xs))

pairs            :: (FastDrawing -> FastDrawing -> FastDrawing) -> [FastDrawing] -> [FastDrawing]
pairs f (x:y:t)  = f x y : pairs f t
pairs f t        = t
{-# INLINABLE pairs #-}
{-# INLINABLE foldt #-}
{-# INLINABLE foldi #-}
-- TODO for path/segment interface, use something like http://stackoverflow.com/questions/17055527/lifting-foldr-to-monad

foreign import javascript unsafe "$5.primRect($1,$2,$3,$4)"
  rect' :: Double -> Double -> Double -> Double -> WithRenderer2 TransferedFastDrawing
foreign import javascript unsafe "$4.primCircle($1,$2,$3)"
  circle' :: Double -> Double -> Double -> WithRenderer2 TransferedFastDrawing
foreign import javascript unsafe "$4.text($1,$2,$3)"
  text' :: Double -> Double -> JSString -> WithRenderer2 TransferedFastDrawing
foreign import javascript unsafe "$3.textFont($1,$2)"
  textFont' :: JSString -> TransferedFastDrawing -> WithRenderer2 TransferedFastDrawing


-- Styles
foreign import javascript unsafe "$6.primFillColor($1,$2,$3,$4,$5)"
  fillColor' :: Double -> Double -> Double -> Double -> TransferedFastDrawing -> WithRenderer2 TransferedFastDrawing
foreign import javascript unsafe "$6.primStrokeColor($1,$2,$3,$4,$5)"
  strokeColor' :: Double -> Double -> Double -> Double -> TransferedFastDrawing -> WithRenderer2 TransferedFastDrawing

foreign import javascript unsafe "$3.primLineWidth($1,$2)"
  lineWidth' :: Double -> TransferedFastDrawing -> WithRenderer2 TransferedFastDrawing
foreign import javascript unsafe "$3.primLineCap($1,$2)"
  lineCap' :: Int -> TransferedFastDrawing -> WithRenderer2 TransferedFastDrawing
foreign import javascript unsafe "$3.primLineJoin($1,$2)"
  lineJoin' :: Int -> TransferedFastDrawing -> WithRenderer2 TransferedFastDrawing
foreign import javascript unsafe "$3.primTextAlignment($1,$2)"
  textAlign' :: Int -> TransferedFastDrawing -> WithRenderer2 TransferedFastDrawing
foreign import javascript unsafe "$3.primTextBaseline($1,$2)"
  textBaseline' :: Int -> TransferedFastDrawing -> WithRenderer2 TransferedFastDrawing

-- Transformation
foreign import javascript unsafe "$8.primTransf($1,$2,$3,$4,$5,$6,$7)"
  transf' :: Double -> Double -> Double -> Double -> Double -> Double -> TransferedFastDrawing -> WithRenderer2 TransferedFastDrawing
foreign import javascript unsafe "$4.scaleXY($1,$2,$3)"
  scaleXY' :: Double -> Double -> TransferedFastDrawing -> WithRenderer2 TransferedFastDrawing
foreign import javascript unsafe "$3.rotate($1,$2)"
  rotate' :: Double -> TransferedFastDrawing -> WithRenderer2 TransferedFastDrawing
foreign import javascript unsafe "$4.translate($1,$2,$3)"
  translate' :: Double -> Double -> TransferedFastDrawing -> WithRenderer2 TransferedFastDrawing
-- Composition
foreign import javascript unsafe "$3.primAp2($1,$2)"
  ap2' :: TransferedFastDrawing -> TransferedFastDrawing -> WithRenderer2 TransferedFastDrawing

foreign import javascript unsafe "$3.primTag($1,$2)"
  tag' :: Int -> TransferedFastDrawing -> WithRenderer2 TransferedFastDrawing


foreign import javascript unsafe "$4.primSegment($1,$2,$3)"
  segment' :: Double -> Double -> RenderedFastSegment -> WithRenderer2 RenderedFastSegment
foreign import javascript unsafe "$8.primSegment($1,$2,$3,$4,$5,$6,$7)"
  segment3' :: Double -> Double -> Double -> Double -> Double -> Double -> RenderedFastSegment -> WithRenderer2 RenderedFastSegment
foreign import javascript unsafe "$4.primPath($1,$2,$3)"
  path' :: Double -> Double -> RenderedFastSegment -> WithRenderer2 TransferedFastDrawing
foreign import javascript unsafe "$2.primSegmentEnd($1)"
  segmentEnd' :: Bool -> WithRenderer2 RenderedFastSegment
foreign import javascript unsafe "$5.primSegmentSubpath($1,$2,$3,$4)"
  segmentSubpath' :: Bool -> Double -> Double -> RenderedFastSegment -> WithRenderer2 RenderedFastSegment


{-# INLINABLE rect' #-}
{-# INLINABLE circle' #-}
{-# INLINABLE text' #-}
{-# INLINABLE textFont' #-}
{-# INLINABLE fillColor' #-}
{-# INLINABLE strokeColor' #-}
{-# INLINABLE transf' #-}
{-# INLINABLE scaleXY' #-}
{-# INLINABLE rotate' #-}
{-# INLINABLE translate' #-}
{-# INLINABLE ap2' #-}
{-# INLINABLE tag' #-}
{-# INLINABLE rect #-}
{-# INLINABLE circle #-}
{-# INLINABLE strokeColor #-}

{-# INLINABLE transf #-}
{-# INLINABLE scaleXY #-}
{-# INLINABLE rotate #-}
{-# INLINABLE translate #-}



-- Output
{-
NOTE behavior is undefined if the result of calling prerender is passed to
renderTransferedFastDrawing using a different renderer (including if it is passed to usePrerendered
and the result of that is passed to renderFastDrawing).
-}
prerender :: FastDrawing -> Renderer -> IO TransferedFastDrawing
prerender p re = runReaderT (getFastDrawing p) re

usePrerendered :: TransferedFastDrawing -> FastDrawing
usePrerendered d = FastDrawing $ pure d

renderFastDrawing :: Renderer -> FastDrawing -> IO ()
renderFastDrawing r p = do
  d <- prerender ({-adaptCoordinates opts-} p) r
  renderTransferedFastDrawing r d

data TagResult = Outside | NoTag | Tag Int
  deriving (Eq, Ord, Show)

getPointTag :: Renderer -> TransferedFastDrawing -> Double -> Double -> IO TagResult
getPointTag !r !d !x !y = do
  r <- getPointTag' r d x y
  case r of
    0 -> pure NoTag
    1 -> pure Outside
    n -> pure $ Tag (n - 2)

  {-
  Incoming picture uses has origin in "middle" and Y positive/down.
  - First correct Y axis (scaleY (-1))
  - Then move based on originPlacement. This transformation is described using Canvas conventions, as we already flipped the Y axis
  -}
adaptCoordinates :: RenderingOptions -> FastDrawing -> FastDrawing
adaptCoordinates (RenderingOptions (P (V2 dx dy)) op _) = t op . flipY
  where
    t TopLeft    = id
    t BottomLeft = translate 0 dy
    t Center     = translate (dx/2) (dy/2)
    flipY = scaleXY 1 (-1)

finWithRenderer3 :: TransferedFastDrawing -> WithRenderer3 TransferedFastDrawing
finWithRenderer3 !d = ReaderT $ \r -> do
  addFinalizer d (releaseD r d)
  pure d

finWithRenderer3_S :: RenderedFastSegment -> WithRenderer3 RenderedFastSegment
finWithRenderer3_S !d = ReaderT $ \r -> do
  addFinalizer d (releaseS r d)
  pure d




#else
module Lubeck.Drawing.Internal.Backend.FastRenderer where
import Prelude
newtype FastDrawing = FastDrawing ()
  deriving (Monoid)
circle = undefined
rect = undefined
text = undefined
transf = undefined
fillColor = undefined
strokeColor = undefined
lineWidth = undefined
path = undefined
linePathV2 = undefined
tag = tag
#endif
