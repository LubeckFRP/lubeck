
{-# LANGUAGE ScopedTypeVariables, BangPatterns, OverloadedStrings, NoImplicitPrelude, GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC
  -O3
  #-}

module Main where

import Control.Monad
import GHCJS.Types(JSVal, JSString)
import Data.JSString(pack)
-- import Foreign.Ptr
-- import Foreign.ForeignPtr
-- import Unsafe.Coerce(unsafeCoerce)

import BasePrelude hiding (empty, rotate)
import System.Mem.Weak(addFinalizer)
import System.Mem
import Control.Monad.Random
import Control.Monad.Random.Class
import GHCJS.Foreign.Callback as CB
import Control.Monad.Reader
import Control.Monad.Reader.Class

newtype Thing = Thing JSVal
foreign import javascript unsafe "{value:$1}"
  makeThing :: Int -> IO Thing
foreign import javascript unsafe "console.log($1)"
  showThing :: Thing -> IO ()
foreign import javascript unsafe "console.log($1,'dead')"
  finalizeThing :: Thing -> IO ()



-- makeThing' :: IO (Ptr Thing)
-- makeThing' = do
--   x <- makeThing
--   pure $ unsafeCoerce x
-- thingPtrToThing :: Ptr Thing -> Thing
-- thingPtrToThing = unsafeCoerce

newtype CanvasElement = DOMCanvasElement JSVal
newtype Context = Context JSVal
newtype Renderer = Renderer JSVal
newtype Segment = Segment JSVal
newtype Drawing = Drawing JSVal
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
-- FIXME variants of asyncCallback et al to allow arbitrary newtype wrappers


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
  showDrawing :: Drawing -> IO ()

foreign import javascript unsafe "$1.render(0,$2)"
  renderDrawing :: Renderer -> Drawing -> IO ()
{-# INLINABLE renderDrawing #-}

foreign import javascript unsafe "$1.getPointTag_(0,$2,$3,$4)"
  getPointTag' :: Renderer -> Drawing -> Double -> Double -> IO Int
{-# INLINABLE getPointTag #-}

data TagResult = Outside | NoTag | Tag Int
  deriving (Eq, Ord, Show)

getPointTag :: Renderer -> Drawing -> Double -> Double -> IO TagResult
getPointTag !r !d !x !y = do
  r <- getPointTag' r d x y
  case r of
    0 -> pure NoTag
    1 -> pure Outside
    n -> pure $ Tag (n - 2)

-- Never call these from high-level API, should be managed by addFinalizer
foreign import javascript unsafe "$1.claim($2)"
  claimD :: Renderer -> Drawing -> IO ()
{-# INLINABLE claimD #-}
foreign import javascript unsafe "$1.release($2)"
  releaseD :: Renderer -> Drawing -> IO ()
{-# INLINABLE releaseD #-}
foreign import javascript unsafe "$1.release($2)"
  releaseS :: Renderer -> Segment -> IO ()
{-# INLINABLE releaseS #-}

-- type R a = Renderer -> a
-- foreign import javascript unsafe "$5.primRect($1,$2,$3,$4)"
--   rect' :: Double -> Double -> Double -> Double -> R Drawing
-- foreign import javascript unsafe "$4.primCircle($1,$2,$3)"
--   circle' :: Double -> Double -> Double -> R Drawing
-- -- foreign import javascript unsafe "$4.text($1,$2,$3)"
--   -- text' :: Double -> Double -> JSString -> R Drawing
--
--
-- -- Styles
-- foreign import javascript unsafe "$2.red($1)"
--   red' :: Drawing -> R Drawing
-- foreign import javascript unsafe "$2.redA($1)"
--   redA' :: Drawing -> R Drawing
-- foreign import javascript unsafe "$2.blueA($1)"
--   blueA' :: Drawing -> R Drawing
-- foreign import javascript unsafe "$6.primFillColor($1,$2,$3,$4,$5)"
--   fillColor' :: Double -> Double -> Double -> Double -> Drawing -> R Drawing
-- foreign import javascript unsafe "$6.primStrokeColor($1,$2,$3,$4,$5)"
--   strokeColor' :: Double -> Double -> Double -> Double -> Drawing -> R Drawing
-- foreign import javascript unsafe "$3.primLineWidth($1,$2)"
--   lineWidth' :: Double -> Drawing -> R Drawing
--
--
-- -- Transformation
-- foreign import javascript unsafe "$8.primTransf($1,$2,$3,$4,$5,$6,$7)"
--   transf' :: Double -> Double -> Double -> Double -> Double -> Double -> Drawing -> R Drawing
-- foreign import javascript unsafe "$4.scaleXY($1,$2,$3)"
--   scaleXY' :: Double -> Double -> Drawing -> R Drawing
-- foreign import javascript unsafe "$3.rotate($1,$2)"
--   rotate' :: Double -> Drawing -> R Drawing
-- foreign import javascript unsafe "$4.translate($1,$2,$3)"
--   translate' :: Double -> Double -> Drawing -> R Drawing
-- -- Composition
-- foreign import javascript unsafe "$3.primAp2($1,$2)"
--   ap2' :: Drawing -> Drawing -> R Drawing
--
-- -- {-# INLINABLE text' #-}
-- {-# INLINABLE rect' #-}
-- {-# INLINABLE circle' #-}
-- {-# INLINABLE red' #-}
-- {-# INLINABLE redA' #-}
-- {-# INLINABLE strokeColor' #-}
--
-- {-# INLINABLE transf' #-}
-- {-# INLINABLE scaleXY' #-}
-- {-# INLINABLE rotate' #-}
-- {-# INLINABLE translate' #-}
--





type R2 a = Renderer -> IO a
type R3 a = ReaderT Renderer IO a
newtype Picture = Picture { getPicture :: R3 Drawing }
newtype Spline = Spline { getSpline :: R3 Segment }

rect :: Double -> Double -> Double -> Double -> Picture
rect !x !y !w !h = Picture $ (ReaderT $ \r -> rect'' x y w h r) >>= finR3

text :: Double -> Double -> JSString -> Picture
text !x !y !txt = Picture $ (ReaderT $ \r -> text'' x y txt r) >>= finR3

segment :: Double -> Double -> Spline -> Spline
segment !x !y (Spline rtail) = Spline $ do
  tail <- rtail
  res <- (ReaderT $ \re -> segment'' x y tail re)
  finR3_S res

-- segment3 :: Double -> Double -> Double -> Double -> Double -> Double -> Spline -> Spline
-- segment3 !x1 !y1 !x2 !y2 !x3 !y3 (Spline rtail) = Spline $ do
--   tail <- rtail
--   res <- (ReaderT $ \re -> segment3'' x1 y1 x2 y2 x3 y3 tail re)
--   finR3_S res

-- Absolute co-ordinates, open/close
linePath :: Foldable t => Bool -> t (Double, Double) -> Spline
linePath close = foldr (\(x,y) r -> segment x y r) (segmentEnd close)

path :: Double -> Double -> Spline -> Picture
path !x !y (Spline rpath) = Picture $ do
  path <- rpath
  res <- (ReaderT $ \re -> path'' x y path re)
  finR3 res

segmentEnd :: Bool -> Spline
segmentEnd !close = Spline $ do
  res <- (ReaderT $ \re -> segmentEnd'' close re)
  finR3_S res

-- segmentSubpath :: Bool -> Double -> Double -> Segment -> Segment

circle :: Double -> Double -> Double -> Picture
circle !x !y !rad = Picture $ (ReaderT $ \r -> circle'' x y rad r) >>= finR3
fillColor :: Double
                 -> Double -> Double -> Double -> Picture -> Picture

fillColor !r !g !b !a (Picture rd1) = Picture $ do
  d1 <- rd1
  res <- (ReaderT $ \re -> fillColor'' r g b a d1 re)
  finR3 res
strokeColor :: Double
                 -> Double -> Double -> Double -> Picture -> Picture

strokeColor !r !g !b !a (Picture rd1) = Picture $ do
  d1 <- rd1
  res <- (ReaderT $ \re -> strokeColor'' r g b a d1 re)
  finR3 res
transf :: Double
                 -> Double
                 -> Double
                 -> Double
                 -> Double
                 -> Double
                 -> Picture
                 -> Picture
transf !a !b !c !d !e !f (Picture rd1) = Picture $ do
  d1 <- rd1
  res <- (ReaderT $ \re -> transf'' a b c d e f d1 re)
  finR3 res
scaleXY :: Double -> Double -> Picture -> Picture
scaleXY !a !b (Picture rd1) = Picture $ do
  d1 <- rd1
  res <- (ReaderT $ \re -> scaleXY'' a b d1 re)
  finR3 res
translate :: Double -> Double -> Picture -> Picture
translate !a !b (Picture rd1) = Picture $ do
  d1 <- rd1
  res <- (ReaderT $ \re -> translate'' a b d1 re)
  finR3 res
rotate :: Double -> Picture -> Picture
rotate !a (Picture rd1) = Picture $ do
  d1 <- rd1
  res <- (ReaderT $ \re -> rotate'' a d1 re)
  finR3 res

textFont :: JSString -> Picture -> Picture
textFont !font (Picture rd1) = Picture $ do
  d1 <- rd1
  res <- (ReaderT $ \r -> textFont'' font d1 r)
  finR3 res

data TextAlign = TextAlignStart | TextAlignEnd | TextAlignLeft | TextAlignRight | TextAlignCenter -- TODO
  deriving (Eq, Ord, Enum, Show)
data TextBaseline
  = TextBaselineTop | TextBaselineHanging | TextBaselineMiddle
  | TextBaselineAlphabetic | TextBaselineIdeographic | TextBaselineBottom -- TODO
  deriving (Eq, Ord, Enum, Show)

textAlign :: TextAlign -> Picture -> Picture
textAlign !x (Picture rd1) = Picture $ do
  d1 <- rd1
  res <- (ReaderT $ \r -> textAlign'' (fromEnum x) d1 r)
  finR3 res

textBaseline :: TextBaseline -> Picture -> Picture
textBaseline !x (Picture rd1) = Picture $ do
  d1 <- rd1
  res <- (ReaderT $ \r -> textBaseline'' (fromEnum x) d1 r)
  finR3 res

data LineCap = LineCapButt | LineCapRound | LineCapSquare
  deriving (Eq, Ord, Enum, Show)
data LineJoin = LineJoinBevel | LineJoinRound | LineJoinMiter
  deriving (Eq, Ord, Enum, Show)

lineWidth :: Double -> Picture -> Picture
lineWidth !x (Picture rd1) = Picture $ do
  d1 <- rd1
  res <- (ReaderT $ \r -> lineWidth'' x d1 r)
  finR3 res

lineCap :: LineCap -> Picture -> Picture
lineCap !x (Picture rd1) = Picture $ do
  d1 <- rd1
  res <- (ReaderT $ \r -> lineCap'' (fromEnum x) d1 r)
  finR3 res

lineJoin :: LineJoin -> Picture -> Picture
lineJoin !x (Picture rd1) = Picture $ do
  d1 <- rd1
  res <- (ReaderT $ \r -> lineJoin'' (fromEnum x) d1 r)
  finR3 res

ap2 :: Picture -> Picture -> Picture
ap2 (Picture rd1) (Picture rd2) = Picture $ do
  d1 <- rd1
  d2 <- rd2
  res <- (ReaderT $ \re -> ap2'' d1 d2 re)
  finR3 res

tag :: Int -> Picture -> Picture
tag !tag (Picture rd2)
  | tag < 0   = error "tag: Tag must be positive"
  | otherwise = Picture $ do
  d2 <- rd2
  res <- (ReaderT $ \re -> tag'' (abs tag + 2) d2 re)
  finR3 res

empty :: Picture
empty = rect 0 0 0 0

-- TODO for path/segment interface, use something like http://stackoverflow.com/questions/17055527/lifting-foldr-to-monad

foreign import javascript unsafe "$5.primRect($1,$2,$3,$4)"
  rect'' :: Double -> Double -> Double -> Double -> R2 Drawing
foreign import javascript unsafe "$4.primCircle($1,$2,$3)"
  circle'' :: Double -> Double -> Double -> R2 Drawing
foreign import javascript unsafe "$4.text($1,$2,$3)"
  text'' :: Double -> Double -> JSString -> R2 Drawing
foreign import javascript unsafe "$3.textFont($1,$2)"
  textFont'' :: JSString -> Drawing -> R2 Drawing


-- Styles
foreign import javascript unsafe "$6.primFillColor($1,$2,$3,$4,$5)"
  fillColor'' :: Double -> Double -> Double -> Double -> Drawing -> R2 Drawing
foreign import javascript unsafe "$6.primStrokeColor($1,$2,$3,$4,$5)"
  strokeColor'' :: Double -> Double -> Double -> Double -> Drawing -> R2 Drawing

foreign import javascript unsafe "$3.primLineWidth($1,$2)"
  lineWidth'' :: Double -> Drawing -> R2 Drawing
foreign import javascript unsafe "$3.primLineCap($1,$2)"
  lineCap'' :: Int -> Drawing -> R2 Drawing
foreign import javascript unsafe "$3.primLineJoin($1,$2)"
  lineJoin'' :: Int -> Drawing -> R2 Drawing
foreign import javascript unsafe "$3.primTextAlignment($1,$2)"
  textAlign'' :: Int -> Drawing -> R2 Drawing
foreign import javascript unsafe "$3.primTextBaseline($1,$2)"
  textBaseline'' :: Int -> Drawing -> R2 Drawing

-- Transformation
foreign import javascript unsafe "$8.primTransf($1,$2,$3,$4,$5,$6,$7)"
  transf'' :: Double -> Double -> Double -> Double -> Double -> Double -> Drawing -> R2 Drawing
foreign import javascript unsafe "$4.scaleXY($1,$2,$3)"
  scaleXY'' :: Double -> Double -> Drawing -> R2 Drawing
foreign import javascript unsafe "$3.rotate($1,$2)"
  rotate'' :: Double -> Drawing -> R2 Drawing
foreign import javascript unsafe "$4.translate($1,$2,$3)"
  translate'' :: Double -> Double -> Drawing -> R2 Drawing
-- Composition
foreign import javascript unsafe "$3.primAp2($1,$2)"
  ap2'' :: Drawing -> Drawing -> R2 Drawing

foreign import javascript unsafe "$3.primTag($1,$2)"
  tag'' :: Int -> Drawing -> R2 Drawing


foreign import javascript unsafe "$4.primSegment($1,$2,$3)"
  segment'' :: Double -> Double -> Segment -> R2 Segment
foreign import javascript unsafe "$8.primSegment($1,$2,$3,$4,$5,$6,$7)"
  segment3'' :: Double -> Double -> Double -> Double -> Double -> Double -> Segment -> R2 Segment
foreign import javascript unsafe "$4.primPath($1,$2,$3)"
  path'' :: Double -> Double -> Segment -> R2 Drawing
foreign import javascript unsafe "$2.primSegmentEnd($1)"
  segmentEnd'' :: Bool -> R2 Segment
foreign import javascript unsafe "$5.primSegmentSubpath($1,$2,$3,$4)"
  segmentSubpath'' :: Bool -> Double -> Double -> Segment -> R2 Segment


{-# INLINABLE rect'' #-}
{-# INLINABLE circle'' #-}
{-# INLINABLE text'' #-}
{-# INLINABLE textFont'' #-}
{-# INLINABLE fillColor'' #-}
{-# INLINABLE strokeColor'' #-}
{-# INLINABLE transf'' #-}
{-# INLINABLE scaleXY'' #-}
{-# INLINABLE rotate'' #-}
{-# INLINABLE translate'' #-}
{-# INLINABLE ap2'' #-}
{-# INLINABLE tag'' #-}
{-# INLINABLE rect #-}
{-# INLINABLE circle #-}
-- {-# INLINABLE red #-}
-- {-# INLINABLE redA #-}
{-# INLINABLE strokeColor #-}

{-# INLINABLE transf #-}
{-# INLINABLE scaleXY #-}
{-# INLINABLE rotate #-}
{-# INLINABLE translate #-}

-- finIO :: Renderer -> Drawing -> IO Drawing
-- finIO !r !d = addFinalizer d (release r d) >> pure d

finR3 :: Drawing -> R3 Drawing
finR3 !d = ReaderT $ \r -> do
  addFinalizer d (releaseD r d)
  pure d

finR3_S :: Segment -> R3 Segment
finR3_S !d = ReaderT $ \r -> do
  addFinalizer d (releaseS r d)
  pure d

instance Monoid Picture where
  mappend = ap2
  mempty = empty
  mconcat = foldi mappend mempty

foldt            :: (Picture -> Picture -> Picture) -> Picture -> [Picture] -> Picture
foldt f z []     = z
foldt f z [x]    = x
foldt f z xs     = foldt f z (pairs f xs)

foldi            :: (Picture -> Picture -> Picture) -> Picture -> [Picture] -> Picture
foldi f z []     = z
foldi f z (x:xs) = f x (foldi f z (pairs f xs))

pairs            :: (Picture -> Picture -> Picture) -> [Picture] -> [Picture]
pairs f (x:y:t)  = f x y : pairs f t
pairs f t        = t
{-# INLINABLE pairs #-}
{-# INLINABLE foldt #-}
{-# INLINABLE foldi #-}

{-
NOTE behavior is undefined if the result of calling prerender is passed to
renderDrawing using a different renderer (including if it is passed to usePrerendered
and the result of that is passed to renderPicture).
-}
prerender :: Picture -> Renderer -> IO Drawing
prerender p re = runReaderT (getPicture p) re

usePrerendered :: Drawing -> Picture
usePrerendered d = Picture $ pure d

renderPicture :: Renderer -> Picture -> IO ()
renderPicture r p = do
  d <- prerender p r
  renderDrawing r d



-- Testing

-- Randomize a line with n points
randLine :: Double -> Double -> Int -> Rand StdGen Picture
randLine xDist ySpan n = g <$> replicateM n getRandom
  where
    g rands =
      strokeColor 1 0 0 1 $
      -- fillColor 1 0 0 1 $
        path 0 0 $ linePath False $ zip [0,xDist..] (fmap (* ySpan) rands)


randPict :: Bool -> Int -> Rand StdGen Picture
randPict col n = setCol col <$> mconcat <$> replicateM n g
  where
    g = do
      x <- getRandom
      y <- getRandom
      shape <- fmap (\x -> if x > (0.5::Double) then circle else square) getRandom
      pure $ shape (400*x) (400*y) 1
    square x y r = rect x y (r*2) (r*2)
    -- square x y r = translate x y $ scale r $ text 0 0 "H"
    scale x = scaleXY x x
    setCol col = if col then strokeColor 0 0 1 1 else fillColor 1 0 0 1

randPictWithTags :: Bool -> Int -> Rand StdGen (Picture)
randPictWithTags col n = setCol col <$> mconcat <$> replicateM n g
  where
    g = do
      x <- getRandom
      y <- getRandom
      shape <- fmap (\x -> if x > (0.5::Double) then circle else square) getRandom
      pure $ shape (400*x) (400*y) 25
    square x y r = rect x y (r*2) (r*2)
    setCol col = if col then fillColor 0 0 1 0.05 else fillColor 1 0 0 0.05

tagTest d c = tagTest2 d c <> translate 0 50 (rotate (tau/12) $ scaleXY 2 2 $ tagTest2 d c)

tagTest2 :: Bool -> Maybe Int -> Picture
tagTest2 down cur = tag 555 $ mconcat $ fmap g [0..50]
  where
    g n = translate 50 50 $ rotate (-0.1*tau) $ tag n $ col n $ mconcat
      [ square (fromIntegral n*baseSize) 0 baseSize
      , textFont "Arial 12px" $ text (fromIntegral n*baseSize) 0 (pack $ show n)
      ]
    baseSize = 15
    col n = case (down, Just n == cur) of
      (True,  True) -> fillColor  0.5 0 0.5 1
      (False, True) -> fillColor  0 0.5 0.5 1
      (True,  False) -> fillColor 0 0 1 1
      (False, False) -> fillColor 1 0 0 1
    tau = 2*pi
    square :: Double -> Double -> Double -> Picture
    -- square x y s = rect x y s s
    square x y s = path x y $
      linePath True [(x+s,y), (x+s,y+s), (x,y+s)]
      -- linePath True []


-- tagTest = tag 555 $ g 0
--   where
--     g n = strokeColor 0 0 1 1 $ square (n*50) 0 50 -- <> text (n*50) 0 (pack $ show n)
--     square :: Double -> Double -> Double -> Picture
--     square x y s = path x y $ linePath True [(x+s,y), (x+s,y+s), (x,y+s)]

main = do
  createCanvasNode
  e <- getCanvas
  ct <- get2DContext e
  r <- createRenderer ct
  showRenderer r


  (pict :: Picture) <- evalRandIO $ randPict False 150
  -- (pict2 :: Picture) <- evalRandIO $ fmap (lineWidth 5) $ fmap (lineWidth 2) $ randPict True 50
  -- (pict2 :: Picture) <- evalRandIO $ randLine 5 50 100
  (d1 :: Drawing) <- prerender pict r
  (d1_copy :: Drawing) <- prerender pict r

  (pict2 :: Picture) <- evalRandIO $ randLine 0.2 550 5000
  (d2 :: Drawing) <- prerender pict2 r

  rotation <- newIORef 0
  isDown <- newIORef False
  current <- newIORef Nothing
  ttr <- prerender (tagTest False Nothing) r

  let handler down e = do
          let x = (offsetX $ MouseEvent e)
          let y = (offsetY $ MouseEvent e)
          writeIORef isDown down

          tag <- getPointTag r ttr x y
          when down $ print tag
          case tag of
            Tag n -> writeIORef current (Just n)
            _     -> writeIORef current Nothing


          writeIORef rotation (screenX (MouseEvent e)/400)
  let update = do
          -- print "Updating..."
          -- print " Clearing"
          clearRect ct 0 0 800 800

          n <- readIORef rotation
          -- modifyIORef' rotation (+ 0.02)

          c <- readIORef current
          d <- readIORef isDown
          -- print (c,d)

          renderPicture r (tagTest d c)
          -- renderPicture r (mconcat
          --   [ mempty
          --   -- , fillColor 1 0 1 1 $ circle 200 200 20
          --   , translate 200 200 $ scaleXY 4 4 $ mconcat
          --     [ mempty
          --
          --     , fillColor 1 0 0 0.2 $ rect 0 0 20 20
          --     , fillColor 0 0 0 1 $ textFont "10px Arial, sans-serif" $ text 0 0 "H"
          --     , fillColor 0 1 0 1 $ textAlign TextAlignCenter $ textBaseline TextBaselineTop $ textFont "10px Arial, sans-serif" $ text 0 0 "H"
          --
          --     ]
          --
          --
          --
          --   -- Compare (pict) and (usePrerendered d1)
          --   , translate 400 400 $ rotate (n*1.003*pi*2)
          --       -- pict
          --       -- mempty
          --       $ usePrerendered d1
          --   , usePrerendered d2
          --   -- , pict2
          --   , textFont "italic bold 10px Georgia, serif" $ translate 200 200 $ scaleXY n n $ scaleXY 5 5 $ fillColor 1 0 1 1 $ text 0 0 (pack $ show n)
          --   ])

          performMajorGC

          -- Keep d1_copy alive
          r :: Double <- evalRandIO $ getRandom
          when (r > 2) $ showDrawing $ d1_copy

          -- render r drawing2

          -- render r $ (fin . translate' 400 400 =<< fin . rotate' (n*pi*2) =<< pure {-dr_-}
          --   dr22) r

  updateCB <- CB.syncCallback CB.ThrowWouldBlock update
  -- updateCB <- CB.asyncCallback update
  setUpdateCB updateCB

  -- handlerCB <- CB.asyncCallback1 handler
  setMousemoveCB e =<< (CB.syncCallback1 CB.ThrowWouldBlock $ handler False)
  setMouseupCB e =<< (CB.syncCallback1 CB.ThrowWouldBlock $ handler False)
  setMousedownCB e =<< (CB.syncCallback1 CB.ThrowWouldBlock $ handler True)

  update
  startLoop

  print "Hello again 0129"

tau = 2*pi
