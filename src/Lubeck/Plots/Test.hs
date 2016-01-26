
{-|


Plotting conventions:

  All data points/values are received unformatted. Each axis has a definition with sensible defaults that tells us
    - The bounds of the axis (min,max).
    - The type of scale (only linear for now)
    - Where the labels go.
  The axis and data give rise to a number of points inside the square (0,0) to (1,1).
    This is scaled to the requested dimensions of the plotting area (not necessarily a square).
  Axes defs are used to generate legend (if necessary).
  Axes are generated from their defs and the plotting area.
  Data plotting:
    Points:   List Point
    Lines:    LinearEquation
    Growth:   List Point
      (show as line segments, with or without area)
 -}
module Lubeck.Plots.Test
    (
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

-- TODO use type class methods where appropriate, i.e. mconcat instead of D.stack

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

(%%) = D.Vector
(~~) = D.Point
zeroP = D.Point 0 0

plotPoints :: Maybe Color -> Plot ([] D.Point)
plotPoints mColor xs = let
    circleColor = fromMaybe Colors.red mColor
  in D.stack $ fmap (\p -> D.translate (scaleVector (600%%300) (p .-. zeroP)) $ (D.scale 5 $ D.fillColor circleColor D.circle)) $ xs
--
-- plotGrowth : { color : Maybe String } -> Plot (List Point)
-- plotGrowth opts xs = let
--     lineColor = Maybe.withDefault "blue" opts.color
--     init = case List.head xs of
--       Nothing -> zeroV -- OK?
--       Just p  -> p .-. zeroP
--   in
--   translate (scaleVector (600^300) init) $ style (styleNamed "fill-opacity" (toString 0)) $ strokeColor lineColor $
-- strokeWidth 1.5 $ segments $
--   List.map (scaleVector (600^300)) $ betweenPoints xs


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
-- fitSq : List Point -> Point -> Point
-- fitSq ps p = let
  -- mx = Maybe.withDefault 0 $ List.minimum $ List.map .x ps
  -- my = Maybe.withDefault 0 $ List.minimum $ List.map .y ps
  -- nx = Maybe.withDefault 0 $ List.maximum $ List.map .x ps
  -- ny = Maybe.withDefault 0 $ List.maximum $ List.map .y ps
  -- sq = {dx=1/(nx-mx), dy=1/(ny-my)}
  -- in scalePoint sq $ p .+^ {dx=-mx, dy=-my}

-- basicAdPlot : DataPlot (List (Date.Date, Float)) (List Point) ()
-- basicAdPlot = uf

{-| -}
-- drawDataPlot : DataPlot a b c -> Drawing
-- drawDataPlot dp = let
--   (data,axis) = dp.fit dp.data
--   in (dp.plot data `over` dp.axisPlot axis)
--
-- {-| -}
-- basicDataGrowth : (a -> Point) -> List a -> DataPlot (List a) (List Point) ()
-- basicDataGrowth f x = let
--     ps = List.map f x
--
--   in {
--   name = "",
--   data = x,
--   fit xs = (List.map (fitSq ps) ps, ()),
--   plot = plotGrowth { color = Nothing },
--   axisPlot _ = transparent
--   }
--
-- {-| -}
-- plotDrawingToSvg : Drawing -> Svg
-- plotDrawingToSvg x = toSvg { origoPlacement = BottomLeft, dimensions = { x = 640, y = 340 } } $ translate (20^20) x


{-| -}
data GrowthPlot = GrowthPlot {
    gp_xName  :: String,
    gp_xScale :: [] (Float, String),
    gp_yName  :: String,
    gp_yScale :: [] (Float, String),
    gp_data   :: [] (String, Color, [] (Float, Float)) -- (x,y)
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
        `D.over`
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
      D.xyAxis
        `D.over`
      D.smokeBackground
        -- (translateX 1 $ Style "fill: red" Circle)
