
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


--

foreign import javascript unsafe
  "var n = document.createElement('canvas'); n.id = 'canvas'; n.width = 800; n.height = 800; document.getElementsByTagName('body')[0].appendChild(n)"
  createCanvasNode :: IO ()
foreign import javascript unsafe
  "var loop = function() { update(); requestAnimationFrame(loop) } ; loop()"
  startLoop :: IO ()
foreign import javascript unsafe "window.update = $1"
  setUpdateCB :: (Callback (IO ())) -> IO ()
foreign import javascript unsafe "$1.onmousemove = $2"
  setHandlerCB :: CanvasElement -> (Callback (JSVal -> IO ())) -> IO ()
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
-- foreign import javascript unsafe "$4.text($1,$2,$3)"
  -- text' :: Double -> Double -> JSString -> R Drawing


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

-- {-# INLINABLE text' #-}
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

text :: Double -> Double -> JSString -> Picture
text !x !y !txt = Picture $ (ReaderT $ \r -> text'' x y txt r) >>= finR3

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


ap2 :: Picture -> Picture -> Picture
ap2 (Picture rd1) (Picture rd2) = Picture $ do
  d1 <- rd1
  d2 <- rd2
  res <- (ReaderT $ \re -> ap2'' d1 d2 re)
  finR3 res
empty :: Picture
empty = rect 0 0 0 0

foreign import javascript unsafe "$5.primRect($1,$2,$3,$4)"
  rect'' :: Double -> Double -> Double -> Double -> R2 Drawing
foreign import javascript unsafe "$4.primCircle($1,$2,$3)"
  circle'' :: Double -> Double -> Double -> R2 Drawing
foreign import javascript unsafe "$4.text($1,$2,$3)"
  text'' :: Double -> Double -> JSString -> R2 Drawing
foreign import javascript unsafe "$3.textFont($1,$2)"
  textFont'' :: JSString -> Drawing -> R2 Drawing

foreign import javascript unsafe "$3.primTextAlignment($1,$2)"
  textAlign'' :: Int -> Drawing -> R2 Drawing
foreign import javascript unsafe "$3.primTextBaseline($1,$2)"
  textBaseline'' :: Int -> Drawing -> R2 Drawing


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
{-# INLINABLE text'' #-}
{-# INLINABLE textFont'' #-}
{-# INLINABLE fillColor'' #-}
{-# INLINABLE strokeColor'' #-}
{-# INLINABLE transf'' #-}
{-# INLINABLE scaleXY'' #-}
{-# INLINABLE rotate'' #-}
{-# INLINABLE translate'' #-}
{-# INLINABLE ap2'' #-}
{-# INLINABLE rect #-}
{-# INLINABLE circle #-}
-- {-# INLINABLE red #-}
-- {-# INLINABLE redA #-}
{-# INLINABLE strokeColor #-}

{-# INLINABLE transf #-}
{-# INLINABLE scaleXY #-}
{-# INLINABLE rotate #-}
{-# INLINABLE translate #-}

finIO :: Renderer -> Drawing -> IO Drawing
finIO !r !d = addFinalizer d (release r d) >> pure d

finR3 :: Drawing -> R3 Drawing
finR3 !d = ReaderT $ \r -> addFinalizer d (release r d) >> pure d

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

renderPicture :: Renderer -> Picture -> IO ()
renderPicture r p = do
  d <- runPicture p r
  render r d



-- Testing

randPict :: Bool -> Int -> Rand StdGen Picture
randPict col n = setCol col <$> mconcat <$> replicateM n g
  where
    g = do
      x <- getRandom
      y <- getRandom
      shape <- fmap (\x -> if x > (0.5::Double) then circle else square) getRandom
      pure $ shape (400*x) (400*y) 25
    -- square x y r = rect x y (r*2) (r*2)
    square x y r = translate x y $ scale r $ text 0 0 "H"
    scale x = scaleXY x x
    setCol col = if col then strokeColor 0 0 1 0.5 else fillColor 1 0 0 0.5

randPictWithTags :: Bool -> Int -> Rand StdGen (Picture)
randPictWithTags col n = setCol col <$> mconcat <$> replicateM n g
  where
    g = do
      x <- getRandom
      y <- getRandom
      shape <- fmap (\x -> if x > (0.5::Double) then circle else square) getRandom
      pure $ shape (400*x) (400*y) 25
    square x y r = rect x y (r*2) (r*2)
    setCol col = if col then fillColor 0 0 1 0.5 else fillColor 1 0 0 0.5

main = do
  createCanvasNode
  e <- getCanvas
  ct <- get2DContext e
  r <- createRenderer ct
  showRenderer r

  rotation <- newIORef 0

  (pict :: Picture) <- evalRandIO $ randPict False 50
  (pict2 :: Picture) <- evalRandIO $ randPict True 50
  (d1 :: Drawing) <- runPicture pict r
  (d2 :: Drawing) <- runPicture pict2 r

  let handler e = do
          writeIORef rotation (screenX (MouseEvent e)/400)
  let update = do
          -- print "Updating..."
          -- print " Clearing"
          clearRect ct 0 0 800 800

          n <- readIORef rotation
          -- modifyIORef' rotation (+ 0.02)


          renderPicture r (mconcat
            [ mempty
            -- , fillColor 1 0 1 1 $ circle 200 200 20
            , translate 200 200 $ scaleXY 4 4 $ mconcat
              [ mempty

              , fillColor 1 0 0 0.2 $ rect 0 0 20 20
              , fillColor 0 0 0 1 $ textFont "10px Arial, sans-serif" $ text 0 0 "H"
              , fillColor 0 1 0 1 $ textAlign TextAlignCenter $ textBaseline TextBaselineTop $ textFont "10px Arial, sans-serif" $ text 0 0 "H"

              ]


            -- , textFont "italic bold 10px Georgia, serif" $ translate 200 200 $ scaleXY n n $ fillColor 1 0 1 1 $ text 0 0 (pack $ show n)
            -- , translate 400 400 $ rotate (n*1.003*pi*2) pict
            -- , pict2
            ])
          performMajorGC

          -- render r drawing2

          -- render r $ (fin . translate' 400 400 =<< fin . rotate' (n*pi*2) =<< pure {-dr_-}
          --   dr22) r

  update
  -- updateCB <- CB.syncCallback CB.ThrowWouldBlock update
  updateCB <- CB.asyncCallback update
  setUpdateCB updateCB
  handlerCB <- CB.asyncCallback1 handler
  setHandlerCB e handlerCB

  startLoop

  print "Hello again 0129"
