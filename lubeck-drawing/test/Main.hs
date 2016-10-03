
-- main = print "Lubeck drawing tests TODO"

{-# LANGUAGE ScopedTypeVariables, BangPatterns, OverloadedStrings, NoImplicitPrelude, GeneralizedNewtypeDeriving, CPP #-}

#ifndef __GHCJS__
import Prelude
main = error "ghcjs only"
#else

{-# OPTIONS_GHC
  -O3
  #-}

module Main where

import Lubeck.Str
import Lubeck.Drawing hiding (path, rect)
import Lubeck.Drawing.Internal.Backend.FastRenderer (adaptCoordinates, prerender, usePrerendered
  , TransferedFastDrawing
  , createCanvasNode
  , getCanvas
  , get2DContext
  , createRenderer
  , showRenderer
  , renderFastDrawing

  , getPointTag
  , TagResult(..)
  , TextAlign(..)
  , TextBaseline(..)
  , MouseEvent(..)
  , offsetX
  , offsetY
  , screenX
  , screenY
  , clearRect
  , setMouseupCB
  , setMousedownCB
  , setMousemoveCB
  , setUpdateCB
  , startLoop
  )

import Control.Monad
import GHCJS.Types(JSVal, JSString)
import Data.JSString(pack)

import BasePrelude hiding (empty, rotate)
import System.Mem.Weak(addFinalizer)
import System.Mem
import Control.Monad.Random
import Control.Monad.Random.Class
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
import Data.Colour.SRGB(sRGB)
import Data.Colour(withOpacity)
#if MIN_VERSION_linear(1,20,0)
#else
import Linear.Epsilon
#endif



-- TODO port these
textFont _ x = x
textAlign _ x = x
textBaseline _ x = x

rgb = sRGB
rgba r g b a = sRGB r g b `withOpacity` a

rect x y w h = translate (V2 x y) $ scaleXY (V2 w h) $ square

text' :: (Backend a) => Double -> Double -> Str -> Draft a
text' x y t = translate (V2 x y) $ text t

-- Testing

-- Randomize a line with n points
randLine :: Double -> Double -> Int -> Rand StdGen (Draft Fast)
randLine xDist ySpan n = g <$> replicateM n getRandom
  where
    g rands =
      strokeColor (rgb 1 0 0) $
      -- fillColor 1 0 0 1 $
        segments $ zipWith V2 [0,xDist..] (fmap (* ySpan) rands)


randPict :: Bool -> Int -> Rand StdGen (Draft Fast)
randPict col n = setCol col <$> mconcat <$> replicateM n g
  where
    g = do
      x <- getRandom
      y <- getRandom
      shape <- fmap (\r x y s -> translate (V2 x y) $ scale s $ (if r > (0.5::Double) then circle else square)) getRandom
      pure $ shape (400*x) (400*y) 1
    -- square x y r = rect x y (r*2) (r*2)
    -- square x y r = translate x y $ scale r $ text 0 0 "H"
    setCol col = if col then strokeColor (rgb 0 0 1) else fillColor (rgb 1 0 0)

randPictWithTags :: Bool -> Int -> Rand StdGen ((Draft Fast))
randPictWithTags col n = setCol col <$> mconcat <$> replicateM n g
  where
    g = do
      x <- getRandom
      y <- getRandom
      shape <- fmap (\r x y s -> translate (V2 x y) $ scale s $ if r > (0.5::Double) then circle else square) getRandom
      pure $ shape (400*x) (400*y) 25
    -- square x y r = rect x y (r*2) (r*2)
    setCol col = if col then fillColorA (rgba 0 0 1 0.05) else fillColorA (rgba 1 0 0 0.05)

tagTest d c = scaleXY (V2 4 4) $ tagTest2 d c <> translate (V2 0 50) (rotate (tau/12) $ scaleXY (V2 2 2) $ tagTest2 d c)

tagTest2 :: Bool -> Maybe Int -> (Draft Fast)
tagTest2 down cur = tag 555 $ mconcat $ fmap g [0..50]
  where
    g n = translate (V2 50 50) $ rotate (-0.1*tau) $ tag n $ col n $ mconcat
      [ square (fromIntegral n*baseSize) 0 baseSize
      , textFont "Arial 12px" $ text' (fromIntegral n*baseSize) 0 (toStr n)
      ]
    baseSize = 15
    col n = case (down, Just n == cur) of
      (True,  True) -> fillColorA  $ rgba 0.5 0 0.5 1
      (False, True) -> fillColorA  $ rgba 0 0.5 0.5 1
      (True,  False) -> fillColorA $ rgba 0 0 1 1
      (False, False) -> fillColorA $ rgba 1 0 0 1
    tau = 2*pi
    square :: Double -> Double -> Double -> (Draft Fast)
    -- square x y s = rect x y s s
    square x y s = polygon [(V2 x y), (V2 (x+s) y), (V2 (x+s) (y+s)), (V2 x (y+s))]
      -- linePath True []

translateXY x y = translate (V2 x y)

main = do
  createCanvasNode
  e <- getCanvas
  ct <- get2DContext e
  r <- createRenderer ct
  showRenderer r


  (pict :: (Draft Fast)) <- evalRandIO $ randPict False 150
  -- (pict2 :: (Draft Fast)) <- evalRandIO $ fmap (lineWidth 5) $ fmap (lineWidth 2) $ randPict True 50
  -- (pict2 :: (Draft Fast)) <- evalRandIO $ randLine 5 50 100
  (d1 :: TransferedFastDrawing) <- prerender (getDraft pict) r
  (d1_copy :: TransferedFastDrawing) <- prerender (getDraft pict) r

  (pict2 :: (Draft Fast)) <- evalRandIO $ randLine 0.2 550 5000
  (d2 :: TransferedFastDrawing) <- prerender (getDraft pict2) r

  rotation <- newIORef (0 :: Angle Double)
  isDown <- newIORef False
  current <- newIORef Nothing

  -- let myPict = fillColor 0 0 1 1 $ tag 31 $ rect (-100) (-100) 200 200
  let myPict = rotate (tau/12) $ mconcat $ reverse
              [ fillColorA (rgba 0 0 1 1) $ tag 31 $ rect (-100) (-100) 200 200
              , translateXY 0 (-0)    $ fillColorA (rgba 0 1 0 1) $ tag 32 $ rect 0 0 90 90
              , translateXY 100 (-50) $ fillColorA (rgba 1 0 0 1) $ tag 32 $ rect 0 0 80 80

              , translateXY 0 (-0) $ fillColorA (rgba 1 0.2 0.2 1) $ tag 32 $ scale 20 $ circle


              , translateXY 0 0   $ fillColorA (rgba 0 0 0 0.8) $ textFont "30px Arial" $ textBaseline TextBaselineAlphabetic $ textAlign TextAlignLeft $ text' (-100) 0 "(-100,0)"
              , translateXY 0 0   $ fillColorA (rgba 0 0 0 0.8) $ textFont "30px Arial" $ textBaseline TextBaselineAlphabetic $ textAlign TextAlignLeft $ text' 0 (-100) "(0,-100)"

              , translateXY 0 0   $ fillColorA (rgba 0 0 0 0.8) $ textFont "30px Arial" $ textBaseline TextBaselineAlphabetic $ textAlign TextAlignLeft $ text' 0 0 "(0,0)"
              , translateXY 30 30 $ fillColorA (rgba 0 0 0 0.8) $ textFont "30px Arial" $ textBaseline TextBaselineAlphabetic $ textAlign TextAlignLeft $ text' 0 0 "(30,30)"
              , translateXY (-200) (-200) $ fillColorA (rgba 0 0 0 0.8) $ textFont "30px Arial" $ textBaseline TextBaselineAlphabetic $ textAlign TextAlignRight $ text' 0 0 "(-200,-200) right"
              ]


  let opts = RenderingOptions (P (V2 800 800)) Center False -- FIXME

  let handler down e = do
          let x = (offsetX $ MouseEvent e)
          let y = (offsetY $ MouseEvent e)

          writeIORef isDown down
          n <- readIORef rotation
          ttr <- prerender (adaptCoordinates opts $ getDraft $ rotate (n ** 1.9) myPict) r
          tag <- getPointTag r ttr x y

          when down $ print (x, y, down, tag)
          case tag of
            Tag n -> writeIORef current (Just n)
            _     -> writeIORef current Nothing


          writeIORef rotation (realToFrac $ screenX (MouseEvent e)/400)
  let update = do
          -- print "Updating..."
          -- print " Clearing"
          clearRect ct 0 0 800 800

          n <- readIORef rotation
          -- modifyIORef' rotation (+ 0.02)

          c <- readIORef current
          d <- readIORef isDown
          -- print (c,d)


          renderFastDrawing r (adaptCoordinates opts $ getDraft $ scale (if d then 1.15 else 1) $ rotate (n ** 1.9) myPict)

          -- renderFastTransferedFastDrawing r (tagTest d c)

          -- renderFastTransferedFastDrawing r (mconcat
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
          -- r :: Double <- evalRandIO $ getRandom
          -- when (r > 2) $ showTransferedFastDrawing $ d1_copy

          -- render r drawing2

          -- render r $ (fin . translate' 400 400 =<< fin . rotate' (n*pi*2) =<< pure {-dr_-}
          --   dr22) r

  -- updateCB <- CB.syncCallback CB.ThrowWouldBlock update
  updateCB <- CB.asyncCallback update
  setUpdateCB updateCB

  -- handlerCB <- CB.asyncCallback1 handler
  setMousemoveCB e =<< (CB.asyncCallback1 $ handler False)
  setMouseupCB e =<< (CB.asyncCallback1  $ handler False)
  setMousedownCB e =<< (CB.asyncCallback1 $ handler True)

  update
  startLoop



tau = 2*pi

-- main = print "TODO restore"

#endif
