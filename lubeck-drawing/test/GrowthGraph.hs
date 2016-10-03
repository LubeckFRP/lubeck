
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
import qualified Data.Colour.Names as Colors
import Data.Colour.SRGB(sRGB)
import Data.Colour(withOpacity)
#if MIN_VERSION_linear(1,20,0)
#else
import Linear.Epsilon
#endif




-- growthGraph :: Behavior (Draft Fast)


growthGraphDummy :: Draft Fast
growthGraphDummy = mconcat
  [ title
  , moreFollowersThisPeriod

  , followersLikeCommentFilter
  , periodShortCut
  , growthPlotUsingDV -- TODO connect to overlays
  , maybeOverlay -- The whole thing, with count, likes/comm, image, larger point, alternative pointer
  , bottomSelector
  ]

title = titleText <> translate (V2 80 0) explanation
  where
    titleText = text "Followers over time2.3M"
    explanation = text "?" <> (fillColor Colors.grey $ scale 10 circle)
    -- TODO pop-up
    popUpText = "The growth of your followers during a selected time range"

moreFollowersThisPeriod = mempty
  where
    a = "2.3M"
    b = "+30.97%"
    c = "MORE FOLLOWERS THIS PERIOD"

followersLikeCommentFilter = catH [b "Followers", b "Likes", b "Comments"]
  where
    -- TODO auto-match box size with text
    -- TODO hover/interact
    b t = text t <> fillColor Colors.grey (scaleXY (V2 110 40) square)

periodShortCut = catH [b "1d", b "5d", b "1m", b "3m", b "6m", b "YTD", b "ALL"]
  where
    -- TODO extra sapce
    b t = fillColor Colors.grey (text t)

growthPlotUsingDV = mempty
-- TODO

countAtTime :: Draft Fast
countAtTime = l <> t
  where
    -- TODO showWithThounsandSeparators
    -- TODO dashing
    -- TODO auto-match box size with text
    -- TODO move to current mouseX (relative plot)
    l = (strokeColor Colors.white $ strokeWidth 2 $ scaleY 100 horizontalLine)
    t = fillColor Colors.white (text "1,000,000") <> fillColor Colors.lightblue (scaleXY (V2 110 40) square)

maybeOverlay = catV [image, spaceV 20, bottom]
  where
    -- TODO correct image
    image = (fillColor Colors.red $ scaleXY (V2 90 90) square)
    -- TODO move to correct position
    bottom = catH [commentD, text "1,023", likesD, text "299,222"] <> (fillColor Colors.lightgrey $ scaleXY (V2 110 30) square)
    -- TODO proper comment/like image
    commentD = fillColor Colors.blue $ scale 10 circle
    likesD = fillColor Colors.red $ scale 10 circle

bottomSelector = mempty


spaceH _ = mempty -- TODO
spaceV _ = mempty -- TODO
catV = mconcat -- TODO proper
catH = mconcat -- TODO proper


main = print "GG"
#endif
