
-- main = print "Lubeck drawing tests TODO"

{-# LANGUAGE
    ScopedTypeVariables
  , BangPatterns
  , OverloadedStrings
  , NoImplicitPrelude
  , GeneralizedNewtypeDeriving
  , CPP
  , NoMonomorphismRestriction
  #-}

#ifndef __GHCJS__
import Prelude
main = error "ghcjs only"
#else

{-# OPTIONS_GHC
  -O3
  #-}

module Main where

import Lubeck.Str
import Lubeck.FRP ()
import Lubeck.DV (plot, drawPlot, line, x, y, getStyled, (<~), renderingRectangle, linePlotStrokeColor, linePlotStrokeWidth, singleColour, paletteToInteractive)
import qualified Lubeck.DV.Styling as DV
import Control.Lens (_1, _2, to, (.~))
import qualified Lubeck.Drawing as D
import Lubeck.Drawing hiding (path, rect)
import Lubeck.Drawing.Internal.Backend.FastRenderer
  ( adaptCoordinates
  , prerender
  , usePrerendered
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
import Data.Colour.SRGB(sRGB, sRGB24)
import Data.Colour(withOpacity)
import qualified StandardDrawingTests as Tests
#if MIN_VERSION_linear(1,20,0)
#else
import Linear.Epsilon
#endif

import Lubeck.FRP
-- import Lubeck.App(runAppReactive)


-- TODO move
showLocalOrigin x = showPoint (P (V2 0 0)) <> x

center800 = translate (V2 400 (-400))

-- growthGraph :: Behavior (Draft Fast)

-- Using top-left local origin
-- Tests.dt_drawing Tests.foo

mainD :: Int -> Draft Fast
mainD n = growthGraph n



strokeTest :: Draft Fast
strokeTest = translate (V2 400 (-400)) $ fillColor Colors.pink $ strokeColor Colors.red $ mconcat [sh 10, translateX 20 (sh 20), translateX 40 (sh 10)]
  where
    sh s = polygon [V2 s 0, V2 0 s, V2 (-s) 0, V2 0 (-s)]

-- mainD = translate (V2 400 (-400)) $ strokeColor Colors.red $ scale 10 $ strokeWidth (1/10) circle
-- mainD = translate (V2 400 (-400)) foo

-- growthGraph :: Draft Fast
growthGraph n = translate (V2 50 (-50))
  $ mconcat
  [ mempty
  , title
  , translate (V2 450 0) moreFollowersThisPeriod
  --
  , translateY (-50) $ mconcat
    [ mempty
    , followersLikeCommentFilter
    , translateX 550 periodShortCut
    ]
  , translateY (-300) $ mconcat
    [ mempty
    , growthPlotUsingDV n -- TODO connect to overlays
    -- , maybeOverlay -- The whole thing, with count, likes/comm, image, larger point, alternative pointer
    ]
  , translateY (-350) $ mconcat
    [ mempty
    , bottomSelector
    ]
  ]


stdFontFamily  = First (Just "Gill Sans, sans-serif")
stdFont        = mempty { fontFamily = stdFontFamily, fontSize = First (Just "16px"), fontWeight = FontWeightN 500 }
stdFontLarger  = stdFont { fontSize = First (Just "19px")}
stdFontEvenLarger  = stdFont { fontSize = First (Just "24px")}
stdFontSmaller = stdFont { fontSize = First (Just "14px")}
greenColor = sRGB24 0x0d 0xe1 0x5b -- #0de15b
lightgreyColor = sRGB24 0xa5 0xa5 0xa5 -- #a5a5a5
lightergreyColor = sRGB24 188 188 188 -- #a5a5a5
bluishColor = sRGB24 88 180 232

title :: Draft Fast
title = titleText <> translateX 174 explanation
  where
    titleText = fillColor Colors.black $ textWithOptions stdFontLarger "Followers over time"
    explanation = translateY 7 $ mconcat
      [ fillColor Colors.white $ textWithOptions (stdFont
        { textAnchor = TextAnchorMiddle
        , alignmentBaseline = AlignmentBaselineMiddle })
         "?"
      , fillColor lightergreyColor $ scale 19 circle
      ]
    -- TODO pop-up
    popUpText = "The growth of your followers during a selected time range"

moreFollowersThisPeriod :: Draft Fast
moreFollowersThisPeriod = mconcat [a, translateX 45 b, translateX (90+5) c]
  where
    a = fillColor greenColor   $ textWithOptions (stdFontEvenLarger { textAnchor = TextAnchorEnd }) "2.3M"
    b = fillColor Colors.white $ textWithOptions (stdFontSmaller { textAnchor = TextAnchorMiddle }) "+30.97%"<> (fillColor greenColor $ translateY 5 $ scaleXY (V2 62 21) square)
    c = fillColor Colors.black $ textWithOptions (stdFont { fontWeight = FontWeightLighter }) "MORE FOLLOWERS THIS PERIOD"

followersLikeCommentFilter :: Draft Fast
followersLikeCommentFilter = catH [b "Followers", translateX (110+10) $ b "Likes", translateX ((110+10)*2) $ b "Comments"]
  where
    -- TODO auto-match box size with text
    -- TODO hover/interact
    b t = mconcat
      [ translateX (85/2)
        $ fillColor Colors.white
        $ textWithOptions (stdFont
          { textAnchor = TextAnchorMiddle
          , alignmentBaseline = AlignmentBaselineMiddle })
        $ t
      , fillColor lightgreyColor (scaleXY (V2 85 28) squareL)
      ]

periodShortCut :: Draft Fast
periodShortCut = foldr (\x y -> x <> translateX 35 y) mempty [b "1d", b "5d", b "1m", b "3m", b "6m", b "YTD", b "ALL"]
  where
    -- TODO extra sapce
    b t = fillColor Colors.lightgrey (textWithOptions stdFont t)

growthPlotUsingDV :: Int -> Draft Fast
growthPlotUsingDV n = flip getStyled (id
  $ renderingRectangle .~ V2 800 200
  $ linePlotStrokeColor .~ singleColour (bluishColor `withOpacity` 1)
  $ linePlotStrokeWidth .~ 3
  -- $ DV.basicTickLength .~ 0
  $ DV.basicTickColor .~ transp
  $ DV.backgroundTickStrokeColorX .~ transp
  $ DV.axisTextFontFamily .~ stdFontFamily
  $ mempty) $ drawPlot $ plot dat [x<~to (!! 0), y<~ to (!! 1)] line
  where
    transp = Colors.black `withOpacity` 0
    -- Just some data
    dat = take n dt
    dt = take 120 Tests.dataset1

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
      when (eventType == MouseDown) $ do
        print "Down"
      writeIORef r (screenX event)
      return ()
    render (State ctxt r) renderer = do
      clearRect ctxt 0 0 1400 800
      n <- readIORef r
      renderFastDrawing renderer (adaptCoordinates opts $ getDraft $ mainD (round (n/4)))
      performMajorGC
      return ()
#endif
