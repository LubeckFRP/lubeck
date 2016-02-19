
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, OverloadedStrings, NamedFieldPuns, QuasiQuotes, CPP, NoMonomorphismRestriction #-}

-- | Basic plotting.
--
-- /DEPRECATED/
module Lubeck.Plots.Test
    ( DataPlot(..)
    , Fit(..)
    , Plot(..)
    , drawDataPlot
    , basicDataGrowth
    , plotDrawingToSvg
    ) where

import Lubeck.Drawing(Drawing)
import Web.VirtualDom.Svg(Svg)
import qualified Lubeck.Drawing as D

import Data.Monoid
import Control.Applicative
import Data.Maybe(fromMaybe)
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point hiding (Point)
import Data.Colour (Colour)
-- import qualified Data.Colour
-- import qualified Data.Colour.SRGB
-- import qualified Data.String
import qualified Data.Colour.Names as Colors
import qualified Data.JSString
import Data.Map(Map)
import qualified Data.Map
import qualified Data.List
-- import qualified Data.Colour.Names as C

-- TODO use type class methods where appropriate, i.e. mconcat instead of D.mconcat

{-|
  A way to draw normalized data.

  The data is assumed to be points in some affine space, normalized so that the data to be plotted is
  in the range (0<x<1). Data outside this range is not necessarily drawn.

  Currently, this always maps
    o     -> o
    (1,1) -> (600,300)

  I.e the boundaries of the actual plotted data is (0,0) (600,300).
-}
type Plot a = a -> Drawing

-- TODO rename
type Color = Colour Double

-- TODO rename
scaleVector (D.Vector ax ay) (D.Vector bx by) = D.Vector (ax * bx) (ay * by)
-- TODO rename
scalePoint (D.Vector dx dy) (D.Point x y) = D.Point (dx*x) (dy*y)

(%%) = D.Vector
(~~) = D.Point
zeroP = D.Point 0 0

plotPoints :: Maybe Color -> Plot ([D.Point])
plotPoints mColor xs = let
    circleColor = fromMaybe Colors.red mColor
  in mconcat $ fmap (\p -> D.translate (scaleVector (600%%300) (p .-. zeroP)) $ (D.scale 5 $ D.fillColor circleColor D.circle)) $ xs

plotGrowth :: Maybe Color -> Plot ([D.Point])
plotGrowth mColor xs = let
    lineColor = fromMaybe Colors.blue mColor
    init = case xs of
      []    -> zeroV -- OK?
      (p:_) -> p .-. zeroP
  in
  D.translate (scaleVector (600%%300) init) $ D.style (D.styleNamed "fill-opacity" "0") $ D.strokeColor lineColor $
    D.strokeWidth 1.5 $ D.segments $
    fmap (scaleVector (600%%300)) $ D.betweenPoints xs


{-|
  A way to normalize data of type a into the expected range (0<x<1) of type b, see above, and to provide an axis of type c.
-}
type Fit a b c = a -> (b, c)

{-| -}
data DataPlot a b c = DataPlot {
  dp_name     :: String,
  dp_data     :: a,
  dp_fit      :: Fit a b c,
  dp_plot     :: Plot b,
  dp_axisPlot :: Plot c
  }


-- Should not be given []
fitSq :: [D.Point] -> D.Point -> D.Point
fitSq ps p = let
  mx = minimum $ fmap D.x ps
  my = minimum $ fmap D.y ps
  nx = maximum $ fmap D.x ps
  ny = maximum $ fmap D.y ps
  sq = let {dx=1/(nx-mx); dy=1/(ny-my)} in D.Vector dx dy
  in scalePoint sq $ p .+^ (let {dx=negate mx; dy=negate my} in D.Vector dx dy)

{-| -}
drawDataPlot :: DataPlot a b c -> Drawing
drawDataPlot dp = let
  (dat,axis) = dp_fit dp (dp_data dp)
  in (dp_plot dp dat <> dp_axisPlot dp axis)

-- {-| -}
basicDataGrowth :: (a -> D.Point) -> [a] -> DataPlot [a] [D.Point] ()
basicDataGrowth f x = let
    ps = fmap f x
      in
    DataPlot {
      dp_name       = "",
      dp_data       = x,
      dp_fit        = const (fmap (fitSq ps) ps, ()),
      dp_plot       = plotGrowth Nothing,
      dp_axisPlot   = const D.transparent
      }

-- {-| -}
plotDrawingToSvg :: Drawing -> Svg
plotDrawingToSvg x = D.toSvg (D.RenderingOptions { D.origoPlacement = D.BottomLeft, D.dimensions = (640~~340) }) $ D.translate (20%%20) x


{-| -}
data GrowthPlot = GrowthPlot {
    gp_xName  :: String,
    gp_xScale :: [(Float, String)],
    gp_yName  :: String,
    gp_yScale :: [(Float, String)],
    gp_data   :: [(String, Color, [] (Float, Float))] -- (x,y)
  }

  {-

-- All Floats below are in (0 < x < 1), cartesian relative plot size


{-| -}
examplePlot : GrowthPlot
examplePlot =
  {
    xName = "A", yName = "B", xScale = [], yScale = [], data = []
  }
-}




{-| -}
plotTest :: a -> Svg
plotTest _ = D.toSvg (D.RenderingOptions { D.origoPlacement = D.BottomLeft, D.dimensions = (640~~340) }) $
  D.translate (20%%20) $ D.scale 1 $
      D.transparent
      -- (plotPoints {color=Just "red"}
      --   (List.take 200 $ List.map (scalePoint $ (1/10)^(1/10)) $ List.map2 Point [0..10] [0..10])
      --   )
      --   `over`
      -- (plotPoints {color=Just "yellow"}
      --   (List.take 200 $ List.map ((\x-> x .+^ (0.1^0.05)) << scalePoint ((1/10)^(1/10))) $ List.map2 Point [0..10] [0..10])
      --   )
      --
      --   `over`
      -- (plotGrowth {color=Just "blue"}
      --   (List.take 200 $ List.map (scalePoint $ (1/10)^(1/10)) $ List.map2 Point [0..12] [6,3,10,5,4,2,1,2.2,3,-2,100000,1000,0])
      --   )
      --   `over`
      -- (plotGrowth {color=Just "darkblue"}
      --   [1&1,0&0,0&0.5,0.85&0.7,0.65&0.35]
      --   )
      --   `over`
      -- (plotGrowth {color=Just "pink"}
      --   [0.0&1.0, 0.1&0.1, 0.22&0.2, 0.3&0.32]
      --   )

        -- [
        -- {dx=10  , dy=10},
        -- {dx=3   , dy=-5},
        -- {dx=13  , dy=5},
        -- {dx=3   , dy=-5},
        -- {dx=10  , dy=1},
        -- {dx=10  , dy=-10},
        -- {dx=50  , dy=0},
        -- {dx=20  , dy=21},
        -- {dx=3   , dy=-5},
        -- {dx=3   , dy=-5},
        -- {dx=3   , dy=-7}
        -- ]
        <>
      -- let plot = (translateX (-200) $ scale 4 $ style (styleNamed "fill-opacity" (toString 0.5)) $ fillColor "lightblue" $
        -- strokeColor "blue" $ strokeWidth 1.5 $ Lines False
      --       [
      --         {dx=10  , dy=10},
      --         {dx=10  , dy=1},
      --         {dx=3   , dy=-5},
      --         {dx=13  , dy=5},
      --         {dx=10  , dy=-10},
      --         {dx=3   , dy=-5},
      --         {dx=20  , dy=21},
      --         {dx=3   , dy=-5},
      --         {dx=50  , dy=0},
      --         {dx=3   , dy=-5},
      --         {dx=3   , dy=-7}
      --       ])
      --   in (stack [plot, scale 1.1 plot, scale 1.3 plot])
      --   `over`
      (D.scale 600 D.xyAxis)
        <>
      D.smokeBackground
        -- (translateX 1 $ Style "fill: red" Circle)
