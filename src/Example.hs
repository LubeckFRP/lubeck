
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import Data.Monoid ((<>))

import GHCJS.Types(JSString, jsval)
import qualified Web.VirtualDom.Html as H
import qualified Web.VirtualDom.Html.Attributes as H
import qualified Web.VirtualDom.Html.Events as H
import qualified Web.VirtualDom.Svg.Events as SvgEv
import qualified Data.JSString

import Lubeck.FRP
import Lubeck.App (Html, runAppReactive)
import Lubeck.Forms
  -- (Widget, Widget', component, bothWidget)
import Lubeck.Forms.Basic
import Lubeck.Drawing
import qualified Lubeck.Drawing

import Data.Colour (withOpacity)
import qualified Data.Colour.Names as Colors


-- TODO to many variants of these
-- componentRW as currently implemented arguably does the wrong thing, by forwarding incoming events
-- (why wrong: this can easily be accomplished by merging with the input, while the other way around is nothing
-- so straightforward)

-- The most general version:
-- (b -> a -> a) -> (c -> a -> a) -> a -> WT r a b -> E c -> IO (S r, S a)

compo :: WidgetT r a a -> a -> Events a -> IO (Signal r, Events a)
compo widget z input = do
  (view, output, inputSink) <- componentRW z widget
  subscribeEvent input inputSink
  return (view, output)

compo_ :: WidgetT r a a -> a -> Events a -> IO (Signal r)
compo_ w z i = fmap fst $ compo w z i

compoS_ :: WidgetT r a a -> Signal a -> IO (Signal r)
compoS_ w i = do
  z <- pollBehavior $ current i
  let u = updates i
  compo_ w z u


-- TODO nicer conventions for event listeners
-- * Standarize Names
-- * Add more of them
-- * Variants without the event (i.e. for "mouseover", "mouseout" etc)

-- Basic non-interactive plots
circleWithMouseOver :: WidgetT Drawing Bool Bool
circleWithMouseOver output state =
  addProperty (SvgEv.onMouseOver $ const $ output True) $
  addProperty (SvgEv.onMouseOut $ const $ output False) $
  mconcat
    [ fillColorA ((if state then Colors.lightblue else Colors.blue) `withOpacity` 0.5) $ scale 250 circle
    , axisY
    , axisX
    ]


labeledAxis :: JSString -> JSString -> Drawing
labeledAxis labelX labelY = mconcat
  [ axis
  , translateY 150 $ translateX (-20) $ rotate (turn/4) $ textMiddle labelY
  , translateX 150 $ translateY (-20) $ textMiddle labelX]
axis = mconcat [axisY, axisX]
axisY = strokeWidth 2 $ strokeColor Colors.black $ scale 300 $ translateY 0.5 verticalLine
axisX = strokeWidth 2 $ strokeColor Colors.black $ scale 300 $ translateX 0.5 horizontalLine



-- drawing :: Sink Int -> Int -> Drawing
-- drawing output n = mempty
--   <> circles
--   <> addProperty (SvgEv.onClick $ \_ -> output (succ n)) (scale 0.8 $ redCircle $ negate n)
--   <> blueRect
--   <> addProperty (SvgEv.onClick $ \_ -> output (pred n)) (redCircle n)
--   <> shearXY (fromIntegral n/200) 0 (scale 40 (unselectable $ Lubeck.Drawing.text "Hans"))
--   <> scale 10 xyAxis
--   <> scale 10 smokeBackground
--   where
--     nCircles = 40
--     circles = mconcat $ fmap (\i -> rotate (turn/fromIntegral nCircles*fromIntegral (negate i)) $ translateX (100+fromIntegral i+fromIntegral n) ci) [1..nCircles]
--       where ci = fillColorA (Colors.green `withOpacity` 0.5) $ scale 10 $ scaleX 0.5 $ square
--     blueRect = fillColorA (Colors.blue `withOpacity` 0.5) $ scale 50 $ scaleX 1.2 $ square
--     redCircle n = fillColor Colors.red $
--       translateY (negate $ 3 * fromIntegral n) $ translateX (4 * fromIntegral n) $
--       scale (50 + 4 * fromIntegral n) $
--       circle

-- unselectable = style $ mconcat $ fmap (uncurry $ styleNamed)
--   [ ("-webkit-touch-callout", "none")
--   , ("-webkit-user-select",   "none")
--   , ("-khtml-user-select",    "none")
--   , ("-moz-user-select",      "none")
--   , ("-ms-user-select",       "none")
--   , ("-o-user-select",        "none")
--   , ("user-select",           "none")
--   -- No mouse pointer
--   , ("pointer-events",        "none")
--   ]


-- MAIN

main :: IO ()
main = do
  -- x <- compoS_ circleWithMouseOver (pure False)
  let x = pure (labeledAxis "usually time" "interesting stuff")
  runAppReactive $ fmap (toSvg defaultRenderingOptions) x
  -- (view, _) <- component 1 render
  -- runAppReactive view
