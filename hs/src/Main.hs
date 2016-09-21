
{-# LANGUAGE ScopedTypeVariables, BangPatterns, NoImplicitPrelude, GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC
  -fno-cse
  -fno-full-laziness
  #-}

module Main where

import Control.Monad
import GHCJS.Types(JSVal)
-- import Foreign.Ptr
-- import Foreign.ForeignPtr
-- import Unsafe.Coerce(unsafeCoerce)

import BasePrelude hiding (empty, rotate)
import System.Mem.Weak(addFinalizer)
import System.Mem
import System.IO.Unsafe(unsafePerformIO)
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
newtype Drawing = Drawing JSVal


--

foreign import javascript unsafe
  "var n = document.createElement('canvas'); n.id = 'canvas'; n.width = 800; n.height = 800; document.getElementsByTagName('body')[0].appendChild(n)"
  createCanvasNode :: IO ()
foreign import javascript unsafe
  "var loop = function() { update(); requestAnimationFrame(loop) } ; loop()"
  startLoop :: IO ()



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

foreign import javascript unsafe "$1.render(0,$2)"
  render :: Renderer -> Drawing -> IO ()
{-# INLINABLE render #-}
foreign import javascript unsafe "$1.claim($2)"
  claim :: Renderer -> Drawing -> IO ()
{-# INLINABLE claim #-}
foreign import javascript unsafe "$1.release($2)"
  release :: Renderer -> Drawing -> IO ()
{-# INLINABLE release #-}

type R a = Renderer -> a
foreign import javascript unsafe "$5.primRect($1,$2,$3,$4)"
  rect' :: Double -> Double -> Double -> Double -> R Drawing
foreign import javascript unsafe "$4.primCircle($1,$2,$3)"
  circle' :: Double -> Double -> Double -> R Drawing


-- Styles
foreign import javascript unsafe "$2.red($1)"
  red' :: Drawing -> R Drawing
foreign import javascript unsafe "$2.redA($1)"
  redA' :: Drawing -> R Drawing
foreign import javascript unsafe "$2.blueA($1)"
  blueA' :: Drawing -> R Drawing
foreign import javascript unsafe "$6.primFillColor($1,$2,$3,$4,$5)"
  fillColor' :: Double -> Double -> Double -> Double -> Drawing -> R Drawing
foreign import javascript unsafe "$6.primStrokeColor($1,$2,$3,$4,$5)"
  strokeColor' :: Double -> Double -> Double -> Double -> Drawing -> R Drawing
foreign import javascript unsafe "$3.primLineWidth($1,$2)"
  lineWidth' :: Double -> Drawing -> R Drawing


-- Transformation
foreign import javascript unsafe "$8.primTransf($1,$2,$3,$4,$5,$6,$7)"
  transf' :: Double -> Double -> Double -> Double -> Double -> Double -> Drawing -> R Drawing
foreign import javascript unsafe "$4.scaleXY($1,$2,$3)"
  scaleXY' :: Double -> Double -> Drawing -> R Drawing
foreign import javascript unsafe "$3.rotate($1,$2)"
  rotate' :: Double -> Drawing -> R Drawing
foreign import javascript unsafe "$4.translate($1,$2,$3)"
  translate' :: Double -> Double -> Drawing -> R Drawing
-- Composition
foreign import javascript unsafe "$3.primAp2($1,$2)"
  ap2' :: Drawing -> Drawing -> R Drawing

{-# INLINABLE rect' #-}
{-# INLINABLE circle' #-}
{-# INLINABLE red' #-}
{-# INLINABLE redA' #-}
{-# INLINABLE strokeColor' #-}

{-# INLINABLE transf' #-}
{-# INLINABLE scaleXY' #-}
{-# INLINABLE rotate' #-}
{-# INLINABLE translate' #-}







type R2 a = Renderer -> IO a
type R3 a = ReaderT Renderer IO a
newtype Picture = Picture { getPicture :: R3 Drawing }

runPicture :: Picture -> Renderer -> IO Drawing
runPicture p re = runReaderT (getPicture p) re

rect :: Double -> Double -> Double -> Double -> Picture
rect !x !y !w !h = Picture $ (ReaderT $ \r -> rect'' x y w h r) >>= finR3
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
ap2 :: Picture -> Picture -> Picture
ap2 (Picture rd1) (Picture rd2) = Picture $ do
  d1 <- rd1
  d2 <- rd2
  res <- (ReaderT $ \re -> ap2'' d1 d2 re)
  finR3 res
empty :: Picture
empty = rect 0 0 0 0

instance Monoid Picture where
  mempty = empty
  mappend = ap2

foreign import javascript unsafe "$5.primRect($1,$2,$3,$4)"
  rect'' :: Double -> Double -> Double -> Double -> R2 Drawing
foreign import javascript unsafe "$4.primCircle($1,$2,$3)"
  circle'' :: Double -> Double -> Double -> R2 Drawing


-- Styles
foreign import javascript unsafe "$6.primFillColor($1,$2,$3,$4,$5)"
  fillColor'' :: Double -> Double -> Double -> Double -> Drawing -> R2 Drawing
foreign import javascript unsafe "$6.primStrokeColor($1,$2,$3,$4,$5)"
  strokeColor'' :: Double -> Double -> Double -> Double -> Drawing -> R2 Drawing
foreign import javascript unsafe "$3.primLineWidth($1,$2)"
  lineWidth'' :: Double -> Drawing -> R2 Drawing
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

{-# INLINABLE rect'' #-}
{-# INLINABLE circle'' #-}
{-# INLINABLE fillColor'' #-}
{-# INLINABLE strokeColor'' #-}
{-# INLINABLE transf'' #-}
{-# INLINABLE scaleXY'' #-}
{-# INLINABLE rotate'' #-}
{-# INLINABLE translate'' #-}
{-# INLINABLE ap2'' #-}
-- {-# INLINABLE rect #-}
-- {-# INLINABLE circle #-}
-- {-# INLINABLE red #-}
-- {-# INLINABLE redA #-}
-- {-# INLINABLE strokeColor #-}
--
-- {-# INLINABLE transf #-}
-- {-# INLINABLE scaleXY #-}
-- {-# INLINABLE rotate #-}
-- {-# INLINABLE translate #-}
-- foreign import javascript unsafe "$1.EMPTY_DRAWING"
  -- empty' :: R Drawing
empty' = rect' 0 0 0 0 -- FIXME

foreign import javascript unsafe "window.update = $1"
  setUpdateCB :: (Callback (IO ())) -> IO ()

fin :: (Renderer -> Drawing) -> Renderer -> Drawing
fin !rd !r = let (d :: Drawing) = rd r in unsafePerformIO (addFinalizer d (release r d) >> pure d)
{-# NOINLINE fin #-}

fin_ :: Renderer -> Drawing -> Drawing
fin_ !r !d = unsafePerformIO (addFinalizer d (release r d) >> pure d)
{-# NOINLINE fin_ #-}

finIO :: Renderer -> Drawing -> IO Drawing
finIO !r !d = addFinalizer d (release r d) >> pure d
-- fin = Prelude.id

finR3 :: Drawing -> R3 Drawing
finR3 !d = ReaderT $ \r -> addFinalizer d (release r d) >> pure d
-- finR3 :: Drawing -> R3 Drawing
-- finR3 = ReaderT (flip finIO)


-- newtype Picture = Picture { getPicture :: R Drawing }
--
-- rect :: Double -> Double -> Double -> Double -> Picture
-- rect !x !y !w !h = Picture $ fin $ rect' x y w h
--
-- circle :: Double -> Double -> Double -> Picture
-- circle !x !y !r = Picture $ fin $ circle' x y r
--
-- red :: Picture -> Picture
-- red !(Picture rdr) = Picture $ fin $ do
--   !dr <- rdr
--   red' dr
--
-- redA :: Picture -> Picture
-- redA !(Picture rdr) = Picture $ fin $ do
--   !dr <- rdr
--   redA' dr
--
-- blueA :: Picture -> Picture
-- blueA !(Picture rdr) = Picture $ fin $ do
--   !dr <- rdr
--   blueA' dr
--
--
-- -- blueA :: Picture -> Picture
-- fillColor :: Double
--                  -> Double -> Double -> Double -> Picture -> Picture
-- fillColor !r !g !b !a !(Picture rdr) = Picture $ fin $ do
--   !dr <- rdr
--   fillColor' r g b a dr
--
-- strokeColor :: Double
--                  -> Double -> Double -> Double -> Picture -> Picture
-- strokeColor !r !g !b !a !(Picture rdr) = Picture $ fin $ do
--   !dr <- rdr
--   strokeColor' r g b a dr
--
-- lineWidth :: Double -> Picture -> Picture
-- lineWidth !w !(Picture rdr) = Picture $ fin $ do
--   !dr <- rdr
--   lineWidth' w dr
--
-- transf :: Double -> Double -> Double -> Double -> Double -> Double -> Picture -> Picture
-- transf !a !b !c !d !e !f !(Picture rdr) = Picture $ fin $ do
--   !dr <- rdr
--   transf' a b c d e f dr
--
-- scaleXY :: Double -> Double -> Picture -> Picture
-- scaleXY !a !b !(Picture rdr) = Picture $ fin $ do
--   !dr <- rdr
--   scaleXY' a b dr
--
-- translate :: Double -> Double -> Picture -> Picture
-- translate !a b (Picture rdr) = Picture $ fin $ do
--   !dr <- rdr
--   translate' a b dr
--
-- rotate :: Double -> Picture -> Picture
-- rotate !a !(Picture rdr) = Picture $ fin $ do
--   !dr <- rdr
--   rotate' a dr
--
-- ap2 :: Picture -> Picture -> Picture
-- ap2 (Picture rdr1) (Picture rdr2) = Picture $ fin $ do
--   !dr1 <- rdr1
--   !dr2 <- rdr2
--   ap2' dr1 dr2
--
-- empty :: Picture
-- empty = rect 0 0 0 0
-- -- empty = Picture $ fin $ empty'
--
-- instance Monoid Picture where
--   mappend = ap2
--   mempty = empty
--   mconcat = foldi mappend mempty
--
-- foldt            :: (Picture -> Picture -> Picture) -> Picture -> [Picture] -> Picture
-- foldt f z []     = z
-- foldt f z [x]    = x
-- foldt f z xs     = foldt f z (pairs f xs)
--
-- foldi            :: (Picture -> Picture -> Picture) -> Picture -> [Picture] -> Picture
-- foldi f z []     = z
-- foldi f z (x:xs) = f x (foldi f z (pairs f xs))
--
-- pairs            :: (Picture -> Picture -> Picture) -> [Picture] -> [Picture]
-- pairs f (x:y:t)  = f x y : pairs f t
-- pairs f t        = t
-- {-# INLINABLE pairs #-}
-- {-# INLINABLE foldt #-}
-- {-# INLINABLE foldi #-}
--
renderPicture :: Renderer -> Picture -> IO ()
renderPicture r p = do
  d <- runPicture p r
  render r d

-- -- TODO is it faster to use a single global renderer?
--
-- foreign import javascript unsafe "r.render(0,$1)"
--   render :: Renderer -> Drawing -> IO ()

-- r.render(0,r.redA(r.randPosCircle()))

  -- var canvas = document.getElementById('canvas');
  -- var context = ;

-- makeAThingAndAddF = do
--   t1 <- makeThing
--   showThing t1
--   addFinalizer t1 $ finalizeThing t1
--   performMajorGC

randPict :: Int -> Rand StdGen Picture
randPict n = fillColor 0 0 1 0.5 <$> mconcat <$> replicateM n g
  where
    g = do
      x <- getRandom
      y <- getRandom
      shape <- fmap (\x -> if x > (0.5::Double) then circle else square) getRandom
      pure $ shape (400*x) (400*y) 5
    square x y r = rect x y r r


-- dr :: IO Picture
-- dr = mconcat <$> replicateM 200 g
--   where
--     g = do
--       x <- randomIO
--       y <- randomIO
--       pure $ red $ strokeColor 0 0 0 0 $ rect (1000*x) (1000*y) 10 10

-- dr :: Picture
-- dr = mconcat $ replicate 2 g
--   where
--     g = mconcat [blueA $ strokeColor 1 0 0 1 $ lineWidth 5 $ rect (1000*x) (1000*y) 10 10]
--     x = 0
--     y = 0

main = do
  -- print "Without"
  -- replicateM_ 100000 $ makeThing
  -- print "With"
  -- replicateM_ 100000 $ fmap (\t -> addFinalizer t (print "F")) makeThing
  -- print "Done"

  -- forM_ [1,2,3,4,5,6,7,1,1] $ \n -> do
  --   let t = makeThing2 n
  --   showThing t
  --
  -- -- makeAThingAndAddF
  -- performMajorGC

  createCanvasNode
  e <- getCanvas
  ct <- get2DContext e
  r <- createRenderer ct
  showRenderer r




  rotation <- newIORef 0

  (pict :: Picture) <- evalRandIO $ randPict 20
  -- NOT OK
  -- let !dr1 = getPicture dr' r
  -- OK
  let !dr21 = fin_ r $ empty' r
  -- OK
  let !dr22 = fin_ r $ fillColor' 1 0 0 1 (fin_ r $ rect' 10 10 10 10 r) r
  -- OK
  let !dr2 = (fin_ r $ fillColor' 0 1 0 1 (fin_ r $ ap2' (fin_ r $ rect' 100 100 50 50 r) (fin_ r $ empty' r) r) r)
  -- OK
  let !dr3 = (fin_ r $ fillColor' 0 0 1 1 (fin_ r $ ap2' (fin_ r $ rect' 100 100 50 50 r) (fin_ r $ circle' 150 150 30 r) r) r)

  let update = do
          -- print "Updating..."
          -- print " Clearing"

          clearRect ct 0 0 800 800
          -- dr' <- evalRandIO dr
          -- dr' <- dr
          -- let dr' = dr
          -- print " Rendering"
          n <- readIORef rotation
          modifyIORef' rotation (+ 0.02)

          (pict2 :: Picture) <- evalRandIO $ randPict 20

          renderPicture r (mconcat [translate 400 400 $ rotate (n*1.003*pi*2) pict, pict2])

          -- FIXME commented out gives us a double free
          render r $ (fin . translate' 400 400 =<< fin . rotate' (n*pi*2) =<< pure {-dr_-}
            dr22) r

          -- renderPicture r (translate 800 800 $ rotate (n*pi*2) dr')


          -- print " Rendering 2 (exacly the same)"
          -- renderPicture r dr'
          -- print " Rendering 3 (slightly transformed)"
          -- renderPicture r $ scaleXY 2 2 dr'
          -- print " Done"
  update
  updateCB <- CB.syncCallback CB.ThrowWouldBlock update
  setUpdateCB updateCB

  startLoop

  print "Hello again 0129"
