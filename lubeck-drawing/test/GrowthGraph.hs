
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
  , runRenderingLoop
  , Context
  , MouseEventType(..)

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
import qualified Data.Colour.Names as Colors
import Data.Colour.SRGB(sRGB)
import Data.Colour(withOpacity)
#if MIN_VERSION_linear(1,20,0)
#else
import Linear.Epsilon
#endif



-- TODO move
showLocalOrigin x = showPoint (P (V2 0 0)) <> x


-- growthGraph :: Behavior (Draft Fast)

-- Using top-left local origin
growthGraph :: Draft Fast
growthGraph = translate (V2 50 (-50))
  $ mconcat
  [ mempty
  , title
  , translate (V2 400 0) moreFollowersThisPeriod
  --
  , translateY (-50) $ mconcat
    [ mempty
    , showLocalOrigin $ followersLikeCommentFilter
    , translateX 200 periodShortCut
    ]
  , translateY (-100) $ mconcat
    [ mempty
    , growthPlotUsingDV -- TODO connect to overlays
    -- , maybeOverlay -- The whole thing, with count, likes/comm, image, larger point, alternative pointer
    ]
  , translateY (-350) $ mconcat
    [ mempty
    , bottomSelector
    ]
  ]


stdFont        = mempty { fontFamily = First (Just "Verdana, sans-serif"), fontSize = First (Just "16px"), fontWeight = FontWeightNormal }
stdFontLarger  = stdFont { fontSize = First (Just "18px")}
stdFontSmaller = stdFont { fontSize = First (Just "15px")}

title :: Draft Fast
title = titleText <> translate (V2 120 0) explanation
  where
    titleText = fillColor Colors.black $ textWithOptions stdFontLarger "Followers over time"
    explanation = mconcat
      [ fillColor Colors.white $ textWithOptions stdFont "?"
      , fillColor Colors.lightgrey $ scale 10 circle
      ]
    -- TODO pop-up
    popUpText = "The growth of your followers during a selected time range"

moreFollowersThisPeriod :: Draft Fast
moreFollowersThisPeriod = mconcat [a, translateX 50 b, translateX 150 c]
  where
    a = fillColor Colors.lightgreen $ textWithOptions stdFont "2.3M"
    b = fillColor Colors.white $ textWithOptions stdFontSmaller "+30.97%"<> (fillColor Colors.lightgreen $ scaleXY (V2 40 30) square)
    c = fillColor Colors.black $ textWithOptions (stdFont { fontWeight = FontWeightLighter }) "MORE FOLLOWERS THIS PERIOD"

followersLikeCommentFilter :: Draft Fast
followersLikeCommentFilter = catH [b "Followers", translateX (110+10) $ b "Likes", translateX ((110+10)*2) $ b "Comments"]
  where
    -- TODO auto-match box size with text
    -- TODO hover/interact
    b t = mconcat
      [ fillColor Colors.white $ textWithOptions stdFont t
      , fillColor Colors.lightgrey (scaleXY (V2 110 40) squareL)
      ]

periodShortCut :: Draft Fast
periodShortCut = catH [b "1d", b "5d", b "1m", b "3m", b "6m", b "YTD", b "ALL"]
  where
    -- TODO extra sapce
    b t = fillColor Colors.lightgrey (textWithOptions stdFont t)

growthPlotUsingDV :: Draft Fast
growthPlotUsingDV = fillColorA (Colors.black `withOpacity` 0.1) $ scaleXY (V2 800 200) squareTL
-- TODO

countAtTime :: Draft Fast
countAtTime = l <> t
  where
    -- TODO showWithThounsandSeparators
    -- TODO dashing
    -- TODO auto-match box size with text
    -- TODO move to current mouseX (relative plot)
    l = (strokeColor Colors.white $ strokeWidth 2 $ scaleY 100 horizontalLine)
    t = fillColor Colors.white (textWithOptions stdFont "1,000,000") <> fillColor Colors.lightblue (scaleXY (V2 110 40) squareTL)

maybeOverlay :: Draft Fast
maybeOverlay = catV [image, spaceV 20, bottom]
  where
    -- TODO correct image
    image = (fillColor Colors.red $ scaleXY (V2 90 90) square)
    -- TODO move to correct position
    bottom = catH [commentD, textWithOptions stdFontSmaller "1,023", likesD, textWithOptions stdFontSmaller "299,222"] <> (fillColor Colors.lightgrey $ scaleXY (V2 110 30) squareTL)
    -- TODO proper comment/like image
    commentD = fillColor Colors.blue $ scale 10 circle
    likesD = fillColor Colors.red $ scale 10 circle

bottomSelector :: Draft Fast
bottomSelector = fillColor Colors.lightblue $ scaleXY (V2 800 30) squareTL


squareL = translate (V2 0.5 0) $ square
squareTL = translate (V2 0.5 (-0.5)) $ square
spaceH _ = mempty -- TODO
spaceV _ = mempty -- TODO
catV = mconcat -- TODO proper
catH = mconcat -- TODO proper



data State = State Context (IORef Double)

main = do
  runRenderingLoop init handleInput render
  print "GG"
  where
    opts = RenderingOptions (P (V2 800 800)) TopLeft False -- FIXME
    init canvE ctxt renderer = do
      r <- newIORef 0
      return $ State ctxt r
    handleInput (State _ r) renderer eventType event = do
      {-when (eventType == MouseDown) $-}
      -- writeIORef r (offsetX event)
      return ()
    render (State ctxt r) renderer = do
      clearRect ctxt 0 0 1400 800
      n <- readIORef r
      renderFastDrawing renderer (adaptCoordinates opts $ getDraft $ rotate (realToFrac (n/800*(2*pi/5))) growthGraph)
      performMajorGC
      return ()
#endif
