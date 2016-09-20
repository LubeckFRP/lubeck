
module Main where

import Control.Monad
import GHCJS.Types(JSVal)
-- import Foreign.Ptr
-- import Foreign.ForeignPtr
-- import Unsafe.Coerce(unsafeCoerce)

import BasePrelude hiding (empty)
import System.Mem.Weak(addFinalizer)
import System.Mem
import System.IO.Unsafe(unsafePerformIO)
import Control.Monad.Random
import Control.Monad.Random.Class
import GHCJS.Foreign.Callback as CB

newtype Thing = Thing JSVal
foreign import javascript unsafe "{value:$1}"
  makeThing :: Int -> IO Thing
foreign import javascript unsafe "console.log($1)"
  showThing :: Thing -> IO ()
foreign import javascript unsafe "console.log($1,'dead')"
  finalizeThing :: Thing -> IO ()

-- foreign import javascript unsafe "{value:Math.random()}"
  -- makeThing2' :: Thing
makeThing2 :: Int -> Thing
makeThing2 n = unsafePerformIO $ do
  t <- makeThing n
  addFinalizer t (finalizeThing t)
  pure t


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
  "var n = document.createElement('canvas'); n.id = 'canvas'; n.width = 1900; n.height = 1500; document.getElementsByTagName('body')[0].appendChild(n)"
  createCanvasNode :: IO ()

foreign import javascript unsafe "document.getElementById('canvas')"
  getCanvas :: IO CanvasElement
foreign import javascript unsafe "$1.getContext('2d')"
  get2DContext :: CanvasElement -> IO Context
foreign import javascript unsafe "createRenderer($1)"
  createRenderer :: Context -> IO Renderer
foreign import javascript unsafe "$1.clearRect($2,$3,$4,$5);"
  clearRect :: Context -> Double -> Double -> Double -> Double -> IO Renderer


foreign import javascript unsafe "console.log($1)"
  showRenderer :: Renderer -> IO ()

foreign import javascript unsafe "$1.render(0,$2)"
  render :: Renderer -> Drawing -> IO ()

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
foreign import javascript unsafe "$4.translate($1,$2,$3)"
  translate' :: Double -> Double -> Drawing -> R Drawing
-- Composition
foreign import javascript unsafe "$3.primAp2($1,$2)"
  ap2' :: Drawing -> Drawing -> R Drawing
-- foreign import javascript unsafe "$1.EMPTY_DRAWING"
  -- empty' :: R Drawing
empty' = rect' 0 0 0 0 -- FIXME

foreign import javascript unsafe "window.update = $1"
  setUpdateCB :: (Callback (IO ())) -> IO ()


newtype Picture = Picture { getPicture :: R Drawing }

rect :: Double -> Double -> Double -> Double -> Picture
rect x y w h = Picture $ rect' x y w h

circle :: Double -> Double -> Double -> Picture
circle x y r = Picture $ circle' x y r

red :: Picture -> Picture
red (Picture rdr) = Picture $ do
  dr <- rdr
  red' dr

redA :: Picture -> Picture
redA (Picture rdr) = Picture $ do
  dr <- rdr
  redA' dr

blueA :: Picture -> Picture
blueA (Picture rdr) = Picture $ do
  dr <- rdr
  blueA' dr


-- blueA :: Picture -> Picture
fillColor r g b a (Picture rdr) = Picture $ do
  dr <- rdr
  fillColor' r g b a dr

strokeColor r g b a (Picture rdr) = Picture $ do
  dr <- rdr
  strokeColor' r g b a dr

lineWidth w (Picture rdr) = Picture $ do
  dr <- rdr
  lineWidth' w dr

transf :: Double -> Double -> Double -> Double -> Double -> Double -> Picture -> Picture
transf a b c d e f (Picture rdr) = Picture $ do
  dr <- rdr
  transf' a b c d e f dr

scaleXY :: Double -> Double -> Picture -> Picture
scaleXY a b (Picture rdr) = Picture $ do
  dr <- rdr
  scaleXY' a b dr
translate :: Double -> Double -> Picture -> Picture
translate a b (Picture rdr) = Picture $ do
  dr <- rdr
  translate' a b dr

ap2 :: Picture -> Picture -> Picture
ap2 (Picture rdr1) (Picture rdr2) = Picture $ do
  dr1 <- rdr1
  dr2 <- rdr2
  ap2' dr1 dr2

empty :: Picture
empty = Picture $ empty'

instance Monoid Picture where
  mappend = ap2
  mempty = empty


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

-- dr :: Picture
dr :: Rand StdGen Picture
dr = mconcat <$> replicateM 34000 g
  where
    g = do
      x <- getRandom
      y <- getRandom
      pure $ mconcat [blueA $ strokeColor 1 0 0 1 $ lineWidth 5 $ rect (1000*x) (1000*y) 10 10]

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

  let update = do
          print "Updating..."
          print " Clearing"
          clearRect ct 0 0 4000 4000
          dr' <- evalRandIO dr
          print " Rendering"
          render r $ getPicture dr' r
          print " Rendering 2"
          render r $ getPicture (scaleXY 2 2 dr') r
          print " Done"
  update
  updateCB <- CB.syncCallback CB.ThrowWouldBlock update
  setUpdateCB updateCB

  print "Hello again 9021"
