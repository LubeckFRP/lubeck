
{-# LANGUAGE
    RankNTypes
  , NoMonomorphismRestriction
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  , TupleSections
  , FlexibleContexts
  , NoImplicitPrelude
  , MultiParamTypeClasses
  , DeriveGeneric
  , DeriveFunctor
  , StandaloneDeriving
  , ScopedTypeVariables
  #-}


{-# OPTIONS_GHC
  -fno-warn-name-shadowing
  -fwarn-incomplete-patterns
  #-}

{-|
A plotting/data visualization library.

Tutorial TODO

For examples, see the @test@ directory.

-}
module Lubeck.DV.New
  (
  -- * Data/Variables
  -- $data


  -- * Algebra
    blendId
  , blend
  , crossWith


  -- * Scales
  , Scale
  , linear
  , linearIntegral
  , linearWithOptions
  , LinearPlotBounds(..)
  , categorical
  , categoricalEnum
  , categoricalWithOptions
  , CategoricalPlotBounds(..)
  , timeScale
  , timeScaleWithOptions
  , TimeRendering(..)

  -- ** HasScale type class
  , HasScale(..)

  -- ** Overriding scales
  , Scaled
  , withScale


  -- * Statistics
  -- $statistics


  -- * Geometry
  , Geometry
  , pointG
  , line
  , area
  , area2
  -- interval
  , fill
  , bars
  , xIntercept
  , yIntercept
  , xInterceptAlways
  , yInterceptAlways
  -- path
  -- schema
  , labelG
  , imageG

  -- ** Legacy
  -- , ifG

  -- * Coordinates
  -- $coordinates




  -- * Aesthetics
  , Key
  , Aesthetic
  -- * Position
  , x
  , y
  -- * Color
  , color
  , strokeColor
  , fillColor
  -- * Geometrical
  , size
  , shape
  , thickness
  , lineType
  -- * Intervals/PlotBounds
  , yMin
  , bound
  , crossLineX
  , crossLineY

  -- ** Special
  , label
  , image

  -- ** Special
  , customAesthetic
  , customAesthetic'

  -- ** Mapping aesthetics
  , (<~)





  -- * Top-level
  , Plot
  , plot
  , plotWithTitles
  , drawPlot
  -- ** Helpers
  , plotLabel
  , plotImage
  , plotXIntercept
  -- ** Top-level (old)
  , visualize
  , visualizeWithStyle

  -- * Debug
  , visualizeTest
  , exportTestDrawing
  )
where

import BasePrelude
import Control.Lens(Getter, to)
import Control.Lens.Operators hiding ((<~))
import Data.Functor.Contravariant (Contravariant(..))
import Data.Map.Strict(Map)
import Data.Time(UTCTime)

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.List
import qualified Data.Map.Strict
import qualified Data.Map.Strict as Map
import qualified Data.Time
import qualified Data.Time.Format
import qualified Text.PrettyPrint.Boxes as B
import System.Directory (createDirectoryIfMissing)

import Generics.Deriving.Base (Generic)
import Generics.Deriving.Monoid

import Linear.Affine (Point(..))
import Linear.V1 (V1(..))
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))

import Lubeck.Str (Str, toStr, packStr, unpackStr)
import Lubeck.Drawing (Drawing, RenderingOptions(..), Rect(..), LineSeg(..))
import Lubeck.Drawing.Transformation
import Lubeck.DV.Styling (Styled, Styling, zoom, zoomType, ZoomType(AutoScaleY))
import Lubeck.DV.Internal.Normalized
import Lubeck.DV.Internal.Table

import qualified Lubeck.Drawing
import qualified Lubeck.Drawing as D
import qualified Lubeck.DV.Styling
import qualified Lubeck.DV.Internal.Render

{-$data

Data is reprented in a tabular format.

A /table/ is a list of tuples or records. Each field in the tuple can be thought
of as a variable. A /table/ is commonly known as a data frame (in ggplot) or as
a varset (in Wilkinson).
-}

{-$statistics

Nothing to see here.

Future:

- These should arguably be handled outside 'Lubeck.DV'. Wilkinson makes the argument
  that statistical transformations has to be applied after scales, how relevant is
  that in this implementation?
-}

{-$coordinates

Nothing to see here.

Future:

- Most interesting ones are X/Y flip and map projections.

- Polar is mainly useful for pie charts.

-}

-- ALGEBRA

{-| No rows.

This is a synonym for 'mempty'.
-}
blendId :: [a]
blendId = mempty

{-| Concatenate rows.

This is a synonym for '<>'.
-}
blend :: [a] -> [a] -> [a]
blend = (<>)

-- TODO some type class thing here for merging tuples/records

{-| Concatenate columns left-to-right. If one column is shorter it is repeated.

>>> crossWith (,) [1..5] ["a", "b"]
[(1,"a"),(2,"b"),(3,"a"),(4,"b"),(5,"a")]

This is a variant of 'zipWith'.
-}
crossWith :: (a -> b -> c) -> [a] -> [b] -> [c]
crossWith _ [] _  = []
crossWith _ _  [] = []
crossWith f a  b  = take (length a `max` length b) $ zipWith f (cycle a) (cycle b)



-- AESTHETICS

{-|
Keys used in aesthetic mappings, i.e. @x@, @y@, @color@.
-}
newtype Key = Key { getKey :: Str }
  deriving (Eq, Ord, IsString)

instance Show Key where
  -- OK because of the IsString instance
  show = show . getKey


{-|
Non-numerical values that can be embedded in a visualization.
Just strings (for labels) and drawings (for embedded images) for now.
-}
data Special
  = SpecialStr Str
  | SpecialDrawing Drawing

instance Show Special where
  show (SpecialStr x) = show x
  show (SpecialDrawing _) = "<drawing>"

{-
The outer bounds of each aesthetic dimensions.

This acts as a linear transformation on data and guides that projects the
visualized data into the unit hypercube.
-}
type PlotBounds = Map Key (Double, Double)

{-|
An 'Aesthetic' maps a single tuple or record to a set of aesthetic attributes such
as position, color or shape.

Standard aesthetics such as 'x', 'y' and 'color' are provided, but you can also
add your own aesthetics using 'customAesthetic'.
-}
data Aesthetic a = Aesthetic
  { aestheticMapping       :: [a] -> a -> Map Key Double
      -- ^ Given dataset @vs@, map single value @v@ into the real domain.
      --
      --   See also 'scaleMapping'.
  , aestheticSpecialMapping  :: [a] -> a -> Map Key Special
      -- ^ Given dataset @vs@, map single value @v@ to a special value.
      --   Typically used for images and labels.
      --
      --   See also 'scaleMapping'.
  , aestheticPlotBounds        :: [a] -> Map Key (Double, Double)
      -- ^ Given a data set, return @(min, max)@ values to visualize (assuming
      --   the same mapping as 'aestheticMapping').
      --
      --   See also 'scalePlotBounds'.
  , aestheticGuides        :: [a] -> Map Key [(Double, Str)]
      -- ^ Given a data set, return a set of values in the real domain (assuming
      ---  the same mapping as 'scaleMapping') and their textual representation.
      --   Typically used for generating ticks and legends.
      --
      --   If you don't want any guides, just return 'mempty'.
      --
      --   See also 'scaleGuides'.

  , aestheticScaleBaseName :: [a] -> Map Key Str
      -- ^ Name of scale used to plot the given aesthetic.
      --
      --   See also 'scaleBaseName'.
  }


{-|
  - 'mempty' does not map anything.
  - 'mappend' interleaves bindings (left-biased).
-}
deriving instance Generic (Aesthetic a)
instance Monoid (Aesthetic a) where
   mempty  = memptydefault
   mappend = mappenddefault

{- |
Make a custom aesthetic attribute.
-}
customAesthetic :: HasScale a => Key -> Aesthetic a
customAesthetic = customAesthetic' scale (const Nothing) (const Nothing)

{- |
Make a custom aesthetic attribute.
-}
customAesthetic' :: (a -> Scale a) -> (a -> Maybe Str) -> (a -> Maybe Drawing) -> Key -> Aesthetic a
customAesthetic' scale toMStr toMDrawing n =
    Aesthetic mapping specialMapping genPlotBounds genGuides getScaleBaseName
  where
    mapping       = \vs v -> Map.singleton n $ scaleMapping (scale v) vs v

    specialMapping = \_  v -> case toMStr v of
      Just str -> Map.singleton n $ SpecialStr str
      Nothing -> case toMDrawing v of
        Just dr -> Map.singleton n $ SpecialDrawing dr
        Nothing -> mempty

    genPlotBounds = \vs -> Map.singleton n $ case vs of
      []    -> (0,0)
      (v:_) -> scalePlotBounds (scale v) vs

    genGuides  = \vs -> Map.singleton n $ case vs of
      []    -> []
      (v:_) -> scaleGuides (scale v) vs

    getScaleBaseName = \vs -> Map.singleton n $ case vs of
      []    -> ""
      (v:_) -> scaleBaseName (scale v)

{-
Contramapping an 'Aesthetic' provides an aesthetic for a (non-strictly) larger type.

>>> contramap fst :: Aesthetic a -> Aesthetic (a, b)
>>> contramap toInteger :: Integral a => Aesthetic Integer -> f a
-}
instance Contravariant Aesthetic where
  contramap f (Aesthetic g g2 h i j)
    = Aesthetic
      (\xs x -> g  (fmap f xs) (f x))
      (\xs x -> g2 (fmap f xs) (f x))
      (\xs   -> h  (fmap f xs))
      (\xs   -> i  (fmap f xs))
      (\xs   -> j  (fmap f xs))




x, y, color, strokeColor, fillColor, size, shape, thickness, lineType, crossLineX, crossLineY :: HasScale a => Aesthetic a

-- | Map values to the X axis of a plot.
x = customAesthetic "x"

-- | Map values to the Y axis of a plot.
y = customAesthetic "y"

-- | Map values to the Y axis of a plot.
-- This is used for the lower bound of the 'area' geometry.
yMin :: forall a. HasScale a => Aesthetic a
yMin = customAesthetic "yMin"

-- | Map values to the color of a plot element.
color = customAesthetic "color"

-- | Map values to the color of a plot element.
strokeColor = customAesthetic "strokeColor"

-- | Map values to the color of a plot element.
fillColor = customAesthetic "fillColor"

-- | Map values to the size of a plot element.
-- Used by 'pointG' and 'image'.
size = customAesthetic "size"

-- | Map values to the shape of a plot element.
shape = customAesthetic "shape"

-- | Map values to the thickness of a plot element.
thickness = customAesthetic "thickness"

-- | Map values to the line type of a plot element.
lineType = customAesthetic "lineType"

-- | Defines the bound of an interval (lower or upper). Works with their
-- standard 'Bool' scale.
--
-- Used by the 'area2' geometry.
bound :: forall a. HasScale a => Aesthetic a
bound = customAesthetic "bound"

-- | If present and non-zero, show X-intercepting cross-lines.
crossLineX = customAesthetic "crossLineX"

-- | If present and non-zero, show Y-intercepting cross-lines.
crossLineY = customAesthetic "crossLineY"

-- | Map string values.
label :: Aesthetic Str
label = customAesthetic' scale Just (const Nothing) "label"

-- | Map arbitrary drawings.
image :: Aesthetic Drawing
image = customAesthetic' dummyScale (const Nothing) Just "image"
  where
    dummyScale _ = Scale (\_ _ -> 0) (const (0,0)) (const []) "dummy"


-- SCALE

{-|
A 'Scale' provides a way of mapping values into the real domain, and scaling
them as necessary to fit into the visualization.

Scales also generate visual guides that helps the viewer understand the data
(conceptually allowing the user to /inverse/ the mapping and scaling).
-}
data Scale a = Scale
  { scaleMapping  :: [a] -> a -> Double
      -- ^ Given dataset @vs@, map single value @v@ into the real domain.
      --   You can construct a linear scale using @\_ x -> x@.
  , scalePlotBounds   :: [a] -> (Double, Double)
      -- ^ Given a data set, return @(min, max)@ values to visualize (assuming
      --   the same mapping as 'scaleMapping').
      --
      --   If you don't want automatic rescaling, use a constant value.
      --
      --   It is fine to provide larger or smaller values than the actual bounds
      --   of the dataset, but note that values outside the bounds given here may
      --   not be visible. On the other hand, if the given bounds are too large
      --   the visualized data may not be intelligeble.
  , scaleGuides    :: [a] -> [(Double, Str)]
      -- ^ Given a data set, return a set of values in the real domain (assuming
      ---  the same mapping as 'scaleMapping') and their textual representation.
      --   Typically used for generating ticks and legends.
      --
      --   If you don't want any guides, just return 'mempty'.
  , scaleBaseName :: Str
      -- ^ A basic name for the scaling (called "basic" as scales may be transformed
      --   combinatorically, so this name is really just a reminder).
      --
      --   For the built-in scales this is the same as the name of the exported
      --   API value, i.e. @"linear"@, @"categorical"@ etc.
  }

instance Contravariant Scale where
  contramap f (Scale r b t n) = Scale
    (\vs v -> r (fmap f vs) (f v))
    (b . fmap f)
    (t . fmap f)
    n

-- | Types with a default scale.
--
-- If you're writing an instance you can safely ignore the @a@ argument, however if
-- you are invoking 'scale' you need to pass an actual value.
--
class HasScale a where
  scale :: a -> Scale a

instance HasScale Double where
  scale = const linear

instance HasScale Integer where
  scale = const linear

instance HasScale Int where
  scale = const linear

instance HasScale Char where
  scale = const categorical

instance HasScale Bool where
  scale = const categoricalEnum

instance HasScale Ordering where
  scale = const categorical

-- Questionable, but gives us the ability to treat strings as categorical values.
instance (Ord a, Show a) => HasScale [a] where
  scale = const categorical

instance HasScale Str where
  scale x = contramap unpackStr $ scale (unpackStr x)

instance HasScale UTCTime where
  scale = const timeScale

-- | A utility for allowing users override the default scale type.
--
-- This works by wrapping up the value with a scale, and providing a 'HasScale'
-- instance that uses this scale, ignoring any other instances for 'a'.
--
-- See 'withScale' for details.
--
data Scaled a = Scaled
  { scaledValue :: a
  , scaledScale :: Scale a
  }

instance HasScale (Scaled a) where
  -- Here 'scaledScale' is used to extract the actual scale, which will be returned
  -- The 'contramap scaledValue' bit is to make sure the returned scale can handle
  -- its input by ignoring the passed scale (looking only at the value).
  scale = contramap scaledValue . scaledScale

{-|
A scale for unique values out of a set.

Only elements in the data set are included, which may not be what you want
if your set is small and bounded (i.e. weekdays). Use 'categoricalEnum' for this.
-}
categorical :: (Ord a, Show a) => Scale a
categorical = categoricalWithOptions Standard

{-|
How to generate bounds for a categorical plot
-}
data CategoricalPlotBounds
  = UseDataPlotBounds             -- ^ PlotBounds will be @[1..n]@ where n is number of elements
  | PadDataPlotBounds Int Int     -- ^ Using (AddToDataPlotBounds a b), bounds will be @[1-a..n+b]@ where n is number of elements
  | Standard                  -- ^ Same as @PadDataPlotBounds 1 1@

{-|
Like 'categorical', but with more custom options.
-}
categoricalWithOptions :: (Ord a, Show a)
  => CategoricalPlotBounds
  -> Scale a
categoricalWithOptions categoricalPlotBounds = Scale
  { scaleMapping  = \vs v -> realToFrac $ succ $ findPlaceIn (sortNub vs) v
  , scalePlotBounds   = bounds
  , scaleGuides   = \vs -> zipWith (\k v -> (realToFrac k, toStr v)) [(1::Int)..] (sortNub vs)
  , scaleBaseName = "categorical"
  }
  where
    -- >>> findPlaceIn "bce" 'b'
    -- 0
    -- >>> findPlaceIn "bce" 'c'
    -- 1
    -- >>> findPlaceIn "bce" 'd'
    -- 2
    -- >>> findPlaceIn "bce" 'e'
    -- 2
    findPlaceIn :: Ord a => [a] -> a -> Int
    findPlaceIn xs x = length $ takeWhile (< x) xs

    bounds' a b = \vs -> (realToFrac (1 - a), realToFrac (length (sortNub vs) + b))
    bounds = case categoricalPlotBounds of
      Standard      -> bounds' (1::Int) (1::Int)
      UseDataPlotBounds -> bounds' (0::Int) (0::Int)
      (PadDataPlotBounds a b) -> bounds' a b


    sortNub = Data.List.nub . Data.List.sort

{-|
A scale for small countable sets.

Guides are generated for all values in the domain, whether they appear in they
data set or not. Use 'categorical' if this is not desired.
-}
categoricalEnum :: (Enum a, Bounded a, Show a) => Scale a
categoricalEnum = Scale
  { scaleMapping  = \_ v -> realToFrac $ succ $ fromEnum v
  , scalePlotBounds   = \vs -> (0, realToFrac $ fromEnum (maxBound `asTypeOf` head vs) + 2)
  , scaleGuides   = \vs -> zipWith (\k v -> (k, toStr v)) [1..]
    [minBound..maxBound `asTypeOf` head vs]
  , scaleBaseName = "categoricalEnum"
  }

{-|
A linear scale.
-}
linear :: (Real a, Show a) => Scale a
linear = linearWithOptions False UseMin

{-|
A linear scale for integral values.

Accepts (and renders) non-integral values, but displays guides without decimals.
-}
linearIntegral :: (Real a, Show a) => Scale a
linearIntegral = linearWithOptions True UseMin

{-|
How to choose lower bound for a scale.

Used to be called 'IntegralPlotBounds'.
-}
data LinearPlotBounds
  = UseZero                     -- ^ Use zero.
  | InterpZeroAndMin Double     -- ^ Interpolate between zero and minimum value (0.5 for middle).
  | UseMin                      -- ^ Use minimum value.

{-|
A linear scale with options.
-}
linearWithOptions :: (Real a, Show a)
  => Bool
    -- ^ If true, use the integral version of show (i.e. no decimals).
    --   If false, a predefined number of decimal places is used
  -> LinearPlotBounds
    -- ^ How to deterine bounds .
  -> Scale a
linearWithOptions
  useIntegralShow
  lowerBoundChoice
  = Scale
  { scaleMapping  = \vs v -> realToFrac v
  -- TODO resize LB to 0?
  , scalePlotBounds   = bounds
  , scaleGuides   = guides
  -- , scaleGuides   = \vs -> [(0, "0"), (1, "1")]
  -- , scaleGuides   = \vs   -> fmap (\v -> (realToFrac v, toStr v)) $ sortNub vs
  , scaleBaseName = "linear"
  }
  where
    safeMin [] = 0
    safeMin xs = minimum xs
    safeMax [] = 0
    safeMax xs = maximum xs

    sortNub = Data.List.nub . Data.List.sort

    showN x = case useIntegralShow of
      True  -> toStr $ round x
      False -> toStr $ roundTo 5 x

    chooseLB minVal = case lowerBoundChoice of
      UseZero               -> 0
      InterpZeroAndMin x -> x * minVal
      UseMin                -> minVal
    chooseUB = id

    bounds vs = (chooseLB $ realToFrac $ safeMin vs, chooseUB $ realToFrac $ safeMax vs)
    guides vs = fmap (\x -> (x, showN x)) $ tickCalc 4 (bounds vs)

    -- number of ticks, interval, outpouts ticks
    tickCalc :: Int -> (Double, Double) -> [Double]
    tickCalc tickCount (lo, hi) =
      let range = hi - lo :: Double
          unroundedTickSize = range/(realToFrac $ tickCount-1)        --  :: Double
          x = realToFrac (ceiling (logBase 10 (unroundedTickSize)-1)) --  :: Double
          pow10x = 10**x -- Math.pow(10, x);
          stepSize = realToFrac ((ceiling (unroundedTickSize / pow10x))::Int) * pow10x
          lb = stepSize * realToFrac (floor (lo / stepSize))
          ub = stepSize * realToFrac (ceiling (hi / stepSize))

      in [lb, lb+stepSize..ub]
      where
        exrng = (2.1, 11.5)

    {-|
    >>> roundTo 5 pi
    3.14159
    >>> roundTo 6 pi
    3.141593
    >>> roundTo 0 pi
    3.0
    -}
    roundTo :: (Fractional a, RealFrac r) => Int -> r -> a
    roundTo n f =  (fromInteger $ round $ f * (10^n)) / (10.0^^n)


{-|
A scale for time values.
-}
timeScale :: Scale UTCTime
timeScale = timeScaleWithOptions StandardTR

data TimeRendering = StandardTR | MonthYearTR

{-|
Like 'timeScale', but with more custom options.
-}
timeScaleWithOptions :: TimeRendering -> Scale UTCTime
timeScaleWithOptions timeRendering = Scale
  { scaleMapping  = mapping
  , scalePlotBounds   = bounds . fmap toNDiffTime
  , scaleGuides   = guides . fmap toNDiffTime
  , scaleBaseName = "timeScale"
  }
  where
    safeMin [] = 0
    safeMin xs = minimum xs
    safeMax [] = 0
    safeMax xs = maximum xs

    sortNub = Data.List.nub . Data.List.sort

    showT t = case timeRendering of
      StandardTR  -> toStr t
      MonthYearTR -> packStr $ Data.Time.formatTime Data.Time.defaultTimeLocale "%m-%Y" t

    mapping _ v = realToFrac $ toNDiffTime v
    bounds vs = (realToFrac $ safeMin vs, realToFrac $ safeMax vs)
    -- Lots of different possibilities here
    guides vs = fmap (\x -> (x, showT $ toUTCTime $ realToFrac x)) $ tickCalc 4 (bounds vs)

    toNDiffTime = (`Data.Time.diffUTCTime` refTime)
    toUTCTime   = (`Data.Time.addUTCTime` refTime)

    -- number of ticks, interval, outpouts ticks
    tickCalc :: Int -> (Double, Double) -> [Double]
    tickCalc tickCount (lo, hi) =
      let range = hi - lo :: Double
          unroundedTickSize = range/(realToFrac $ tickCount-1)        --  :: Double
          x = realToFrac (ceiling (logBase 10 (unroundedTickSize)-1)) --  :: Double
          pow10x = 10**x -- Math.pow(10, x);
          stepSize = realToFrac ((ceiling (unroundedTickSize / pow10x))::Int) * pow10x
          lb = stepSize * realToFrac (floor (lo / stepSize))
          ub = stepSize * realToFrac (ceiling (hi / stepSize))

      in [lb, lb+stepSize..ub]
      where
        exrng = (2.1, 11.5)

    roundTo :: (Fractional a, RealFrac r) => Int -> r -> a
    roundTo n f =  (fromInteger $ round $ f * (10^n)) / (10.0^^n)

-- timeScale :: Scale UTCTime
-- timeScale = contramap (`Data.Time.diffUTCTime` refTime) linear

refTime :: UTCTime
refTime = case Data.Time.Format.parseTimeM True Data.Time.Format.defaultTimeLocale
  (Data.Time.Format.iso8601DateFormat Nothing) "2000-01-01" of
    Just t -> t
    _      -> error "Should not happen"

{-| Override the default scale instance.

@
[ x <~ name
, y <~ age \`withScale\` linear ]
@
-}
withScale :: Getter s a -> Scale a -> Getter s (Scaled a)
withScale g s = to $ \x -> flip Scaled s $ x^.g

infixl 4 `withScale`

-- Very similar to (>$$<)

{-|
Lifts an aesthetic through a 'Getter' from /lens/.

Idiomatically
@
x <~ name
@

To lift a standard function @s -> a@ instead of a @Getter s a@, you can use 'to',
or 'contramap'.

@
x '<~' 'to' getName
x '>$$<' getName
x 'contramap' getName
@

-}
(<~) :: Aesthetic a -> Getter s a -> Aesthetic s
(<~) a g = contramap (^.g) a

infixl 3 <~







































-- GEOMETRY


{-
A value in @[ x | 0 <= x <= 1]@.

Used to represent scaled data.
-}
type Coord = Normalized Double

{-
Each cell contains mapped data in scaled and unscaled form, as well as special data.
-}
data Cell = Cell
  { cUnscaled :: Double
  , cScaled   :: Coord
  , cSpecial  :: Maybe Special
  }

{-|
Provides a way to draw mapped and scaled data.

You can think of scaled and mapped data matrix of numbers and special values
(images, labels etc), where each rows correspond to a tuple in the original
dataset and each column to an aesthetic attribute such as size, position, color etc.
-}
data Geometry = Geometry
  { geomMapping        :: Table Key Cell -> Styled Drawing
  , geomBaseName       :: [String]
  }

deriving instance Generic Geometry
instance Monoid Geometry where
   mempty  = memptydefault
   mappend = mappenddefault

ifT :: Key -> Table Key Cell -> Table Key Cell
ifT key = filterRows key (\x -> cScaled x >= 0.5)

-- TODO separate tables by key combination
-- I.e. partition into subtables such that each subtable has a unique (linetype, color) combination

-- TODO how do we know what aesthetics a certain plot listens to?
-- Is this all dynamic or is types involved?

-- TODO stacking/dodging/jittering



scaledAttr :: Key -> Table Key Cell -> Column Double
scaledAttr k = fmap (getNormalized . cScaled) . getColumn k

unscaledAttr :: Key -> Table Key Cell -> Column Double
unscaledAttr k = fmap cUnscaled . getColumn k

specialAttr :: Key -> Table Key Cell -> Column (Maybe Special)
specialAttr k = fmap cSpecial . getColumn k


{-|
Point geometry.
Can be combined with line and fill.

Aesthetics:

@
x, y
@
-}
pointG :: Geometry
pointG = Geometry g []
  where
    g t = Lubeck.DV.Internal.Render.scatterData (Lubeck.DV.Internal.Render.ScatterData color) $ runColumnFinite $ do
      x <- scaledAttr "x" t
      y <- scaledAttr "y" t
      return $ P $ V2 x y
      where
        -- TODO color separation
        colors = runColumnFinite $ unscaledAttr "color" t
        color = case colors of
          [] -> 0
          xs -> head xs

{-|
Line geometry.
Can be combined with point and fill.

Aesthetics:

@
x, y
@
-}
line :: Geometry
line = Geometry g []
  where
    -- TODO extract color
    g t = Lubeck.DV.Internal.Render.lineData (Lubeck.DV.Internal.Render.LineData color 0) $ runColumnFinite $ do
      x <- scaledAttr "x" t
      y <- scaledAttr "y" t
      return $ P $ V2 x y
      where
        -- TODO color separation
        colors = runColumnFinite $ unscaledAttr "color" t
        color = case colors of
          [] -> 0
          xs -> head xs

{-|
Fill geometry. Similar to area, except that the lower bound is always the same as the X axis.
Can be combined with line and point.

See also area, area2.

Aesthetics:

@
x, y
@
-}
fill :: Geometry
-- TODO color separation
fill = Geometry g []
  where
    -- TODO extract color
    g t = Lubeck.DV.Internal.Render.fillData (Lubeck.DV.Internal.Render.AreaData color) $ runColumnFinite $ do
      x <- scaledAttr "x" t
      y <- scaledAttr "y" t
      return $ P $ V2 x y
      where
        -- TODO color separation
        colors = runColumnFinite $ unscaledAttr "color" t
        color = case colors of
          [] -> 0
          xs -> head xs

{-|
Fill geometry.

Similar to 'fill', but renders the area between 'y' and 'yMin' instead of between 'y' and 0.

Aesthetics:

@
x, yMin, y
@
-}
area :: Geometry
area = Geometry g []
  where
    -- TODO extract color
    g t = Lubeck.DV.Internal.Render.areaData (Lubeck.DV.Internal.Render.AreaData color) $ runColumnFinite $ do
      x    <- scaledAttr "x" t
      yMin <- scaledAttr "yMin" t
      y    <- scaledAttr "y" t
      return $ P $ V3 x yMin y
      where
        -- TODO color separation
        colors = runColumnFinite $ unscaledAttr "color" t
        color = case colors of
          [] -> 0
          xs -> head xs
{-# DEPRECATED area "Use fill or area2" #-}

{-|
An alternative version of 'area' that expects the lower and upper bounds to be distinct points
in the data sets, distinguished by the @bound@ aesthetic.

@
{x, y, bound:False}, {x, y, bound:True} ~ {x, y, y2}
@

Aesthetics:

@
x, y, bound
@
-}
area2 :: Geometry
area2 = Geometry g []
  where
    g t = Lubeck.DV.Internal.Render.areaData' (Lubeck.DV.Internal.Render.AreaData color) (ps1 <> reverse ps2)
      where
        ps1 = runColumnFinite $ do
          let low  = filterRows "bound" (\x -> cScaled x <  0.5) t
          x1 <- scaledAttr "x" low
          y1 <- scaledAttr "y" low
          let p1 = P (V2 x1 y1)
          return p1
        ps2 = runColumnFinite $ do
          let high = filterRows "bound" (\x -> cScaled x >= 0.5) t
          x2 <- scaledAttr "x" high
          y2 <- scaledAttr "y" high
          let p2 = P (V2 x2 y2)
          return p2

        -- TODO color separation
        colors = runColumnFinite $ unscaledAttr "color" t
        color = case colors of
          [] -> 0
          xs -> head xs

{-|
Bar geometry.

Aesthetics:

@
y
@
-}
bars :: Geometry
bars = Geometry g []
  where
    -- TODO color, stack, dodge
    g t = Lubeck.DV.Internal.Render.barData $ runColumnFinite $ do
      y <- scaledAttr "y" t
      return $ P $ V1 y

{-|
A line intercepting X values. Drawn if @crossLineX@ is present and non-zero.

Aesthetics:

@
x, y, crossLineX
@
-}
xIntercept :: Geometry
xIntercept = Geometry g []
  where
    -- TODO extract color
    g t2 = Lubeck.DV.Internal.Render.scatterDataX $ runColumnFinite $ do
      let t = ifT "crossLineX" t2
      x <- scaledAttr "x" t
      y <- scaledAttr "y" t
      return $ P $ V2 x y
{-|
A line intercepting Y values. Drawn if @crossLineY@ is present and non-zero.

Aesthetics:

@
x, y, crossLineY
@
-}
yIntercept :: Geometry
yIntercept = Geometry g []
  where
    -- TODO extract color
    g t2 = Lubeck.DV.Internal.Render.scatterDataY $ runColumnFinite $ do
      let t = ifT "crossLineY" t2
      x <- scaledAttr "x" t
      y <- scaledAttr "y" t
      return $ P $ V2 x y

{-|
A line intercepting X values. Drawn if @crossLineX@ is present and non-zero.

Aesthetics:

@
x, y, crossLineX
@
-}
xInterceptAlways :: Geometry
xInterceptAlways = Geometry g []
  where
    -- TODO extract color
    g t = Lubeck.DV.Internal.Render.scatterDataX $ runColumnFinite $ do
      -- let t = ifT "crossLineX" t2
      x <- scaledAttr "x" t
      y <- scaledAttr "y" t
      return $ P $ V2 x y
{-|
A line intercepting Y values. Drawn if @crossLineY@ is present and non-zero.

Aesthetics:

@
x, y, crossLineY
@
-}
yInterceptAlways:: Geometry
yInterceptAlways = Geometry g []
  where
    -- TODO extract color
    g t = Lubeck.DV.Internal.Render.scatterDataY $ runColumnFinite $ do
      -- let t = ifT "crossLineY" t2
      x <- scaledAttr "x" t
      y <- scaledAttr "y" t
      return $ P $ V2 x y

{-|
Draws a custom image specified by the 'image' aesthetic.

Aesthetics:

@
x, y, image
@
-}
imageG :: Geometry
imageG = Geometry g [""]
  where
    g t = mconcat $ runColumnFinite $ do
      x  <- scaledAttr "x" t
      y  <- scaledAttr "y" t
      sp <- specialAttr "image" t
      case sp of
        -- TODO get scaling in Maybe?
        -- Would require something like (Column a -> Column (Maybe a))
        Just (SpecialDrawing dr) -> return $ Lubeck.DV.Internal.Render.baseImage dr x y (Just 1)
        _                        -> return mempty

{-|
Draws custom text specified by the 'label' aesthetic.

Aesthetics:

@
x, y, labe
@
-}
labelG :: Geometry
labelG = Geometry g [""]
  where
    g t = mconcat $ runColumnFinite $ do
      x  <- scaledAttr "x" t
      y  <- scaledAttr "y" t
      sp <- specialAttr "label" t
      case sp of
        Just (SpecialStr "")  -> return $ mempty
        Just (SpecialStr str) -> return $ Lubeck.DV.Internal.Render.baseLabel x y str
        _                     -> return mempty































-- toNewGeom :: Geometry -> Geometry
-- toNewGeom = id

-- TOP-LEVEL



-- Table k a ~ [Map k a] ~ Map k [Maybe a]

-- Data/guides/labels is mapped but not scaled
data SinglePlot = SinglePlot
  -- Generated from running aesthetics over data
  { mappedData        :: Table Key Double
  , specialData       :: Table Key Special
  , guides            :: Map Key [(Double, Str)]
  , bounds            :: Map Key (Double, Double) -- aka PlotBounds

  -- Provided by user
  , geometry          :: Geometry
  , axesTitles        :: [Str]
  }
-- Not a Monoid!


{-
Can we write
  xScaledYBounds :: Plot -> T1 Double -> (Double, Double)
  yScaledXBounds :: Plot -> T1 Double -> (Double, Double)
Yes!
  Filter mapped data by assuring that x through the transformation is in UHQ]
  Extract Y values, return new bounds

Then create a linear transformation that fits this value into UHQ
This could be composed with the given T1 to create a new T2.

We could use this to override the zoom value in the Stylin using local.

XXX
OK so far for a single plot, but what about a layered plot?
-}

plotPlotBounds :: Plot -> PlotBounds
plotPlotBounds (Plot []) = mempty
plotPlotBounds (Plot ps) = foldr1 outerPlotBounds (fmap bounds ps)
  where
    outerPlotBounds :: (Ord k) => Map k (Double, Double) -> Map k (Double, Double) -> Map k (Double, Double)
    outerPlotBounds = Map.unionWith g
      where
        g (a1, b1) (a2, b2) = (a1 `min` a2, b1 `max` b2)

plotBoundsToTransf :: PlotBounds -> T2 Double
plotBoundsToTransf b = Lubeck.Drawing.rectToTransf $ Lubeck.Drawing.rect
  (fst $ b ? "x")
  (fst $ b ? "y")
  (snd $ b ? "x")
  (snd $ b ? "y")
  where
    m ? k = maybe (error "plotBoundsToTransf: Missing x/y") id $ Map.lookup k m

plotBoundsToTransfX :: PlotBounds -> T1 Double
plotBoundsToTransfX b = Lubeck.Drawing.lineSegToTransf $ Lubeck.Drawing.lineseg
  (fst $ b ? "x")
  (snd $ b ? "x")
  where
    m ? k = maybe (error "plotBoundsToTransf: Missing x/y") id $ Map.lookup k m


rectToLineSegX :: Rect a -> LineSeg a
rectToLineSegX r = D.lineseg (r ^. D._left) (r ^. D._right)

rectFromLineSegs :: LineSeg a -> LineSeg a -> Rect a
rectFromLineSegs (LineSeg (P (V1 x1)) (P (V1 x2))) (LineSeg (P (V1 y1)) (P (V1 y2))) = D.rect x1 y1 x2 y2

{-
Given a plot and a suggested zoom value for X, return a suitable zoom value for Y.
-}
autoscaleByX :: Plot -> LineSeg Double -> LineSeg Double
autoscaleByX pl@(Plot ps) xt =
    -- D.transfToLineSeg $ scaling1 0.5
  case newBounds of
    [] -> D.transfToLineSeg 1
    xs -> D.transfToLineSeg $ recip $ D.lineSegToTransf $ D.lineseg (getNormalized $ minimum xs) (getNormalized $ maximum xs)
  where
    newBounds :: [Coord]
    newBounds = mconcat $ fmap (runColumnFinite . getColumn "y"
      . filterRows "x" (\x -> withinNormRange (transformNormalized (D.lineSegToTransf xt) x))
      . scaledData
      ) ps

    scaledData :: SinglePlot -> Table Key Coord
    scaledData (SinglePlot mappedData specialData _ _ _ _)
        = normalizeData (plotPlotBounds pl) mappedData

    normalizeData :: PlotBounds -> Table Key Double -> Table Key Coord
    normalizeData b m = tableFromList $ fmap (normalizeData' b) $ tableToList m
      where
        normalizeData'   b = Map.mapWithKey (\aesK dsL ->              normalize (Map.lookup aesK b)  dsL)

    -- | Is a number within the normalized (UHQ) range?
    withinNormRange :: (Num a, Ord a) => a -> Bool
    withinNormRange x = 0 <= x && x <= 1

    transformNormalized t (Normalized x) = let (P (V1 x')) = transformPoint1 t (P (V1 x)) in Normalized x'

updateZoomToAutoScale :: Plot -> Styling -> Styling
updateZoomToAutoScale plot style
  | style^.zoomType == AutoScaleY = (zoom %~ g) style
  | otherwise                     = style
  where
    g z = D.rectToTransf $ rectFromLineSegs xx yy
      where
        yy = autoscaleByX plot xx
        xx = rectToLineSegX (D.transfToRect z)


-- TODO dummy, implement properly



newtype Plot = Plot [SinglePlot]
  deriving (Monoid)

{-|
Create a plot from a given data set, aesthetic mappings and geometry.

-}
createSinglePlot :: [Str] -> [a] -> [Aesthetic a] -> Geometry -> SinglePlot

{-|
Convert the given visualization to a 'Drawing'.

The 'Styled' monad can be used to customize the visual style of the plot without affecting semantics.
-}
drawPlot :: Plot -> Styled Drawing


-- createSinglePlot = undefined
-- drawPlot = undefined

createSinglePlot titles dat aess geometry =
  SinglePlot mappedData specialData guides bounds geometry titles
  where
    aes                 = mconcat aess
    bounds              = aestheticPlotBounds aes dat
    guides              = aestheticGuides aes dat
    specialData         = tableFromList $ fmap (aestheticSpecialMapping aes dat) dat
    mappedData          = tableFromList $ fmap (aestheticMapping aes dat) dat




drawPlot fullPlot@(Plot plots) = mconcat $ zipWith (drawPlot1 (plotPlotBounds (Plot plots))) (True : repeat False) plots
  where
    drawPlot1 :: PlotBounds -> Bool -> SinglePlot -> Styled Drawing
    drawPlot1 bounds includeGuides plot = mconcat
      [ dataD
      , if includeGuides
          then mconcat [axesD, guidesD]
          else mempty ]
      where

        axesD :: Styled Drawing
        axesD = Lubeck.DV.Internal.Render.labeledAxis (axesNames !! 0) (axesNames !! 1)
          where
            m ? k = maybe mempty id $ Map.lookup k m
            axesNames = axesTitles plot <> repeat mempty

        guidesD :: Styled Drawing
        guidesD = local (updateZoomToAutoScale fullPlot)
          $ drawGuides (scaledGuides (bounds) plot ? "x") (scaledGuides (bounds) plot ? "y")
          where
            m ? k = maybe mempty id $ Map.lookup k m
            drawGuides xs2 ys2 = Lubeck.DV.Internal.Render.ticks (fmap (first getNormalized) xs) (fmap (first getNormalized) ys)
              where
                xs = fmap (second Just) xs2
                ys = fmap (second Just) ys2

        dataD :: Styled Drawing
        dataD = local (updateZoomToAutoScale fullPlot) $ do
          (cells :: Table Key Cell) <- pure $ wrapTable (mappedData plot) (mappedAndScaledDataWithSpecial bounds plot)
          geomMapping (geometry plot) $ cells
          where
            wrapTable = overlayTablesShort (\a (b,c) -> Cell a b c)

        -- Scale/Normalize

        mappedAndScaledDataWithSpecial :: PlotBounds -> SinglePlot -> Table Key (Coord, Maybe Special)
        mappedAndScaledDataWithSpecial b (SinglePlot mappedData specialData _ _ _ _)
            = g b mappedData specialData
          where
            g b x y = conjoin2L (normalizeData b x) y

        scaledGuides :: PlotBounds -> SinglePlot -> Map Key [(Coord, Str)]
        scaledGuides b (SinglePlot _ _ guides _ _ _) = normalizeGuides b guides

        normalizeGuides :: PlotBounds -> Map Key [(Double, a)] -> Map Key [(Coord, a)]
        normalizeGuides b m = normalizeGuides' b m
          where
            normalizeGuides' b = Map.mapWithKey (\aesK dsL -> fmap (first (normalize (Map.lookup aesK b))) dsL)

        normalizeData :: PlotBounds -> Table Key Double -> Table Key Coord
        normalizeData b m = tableFromList $ fmap (normalizeData' b) $ tableToList m
          where
            normalizeData'   b = Map.mapWithKey (\aesK dsL ->              normalize (Map.lookup aesK b)  dsL)



{-| Print original data, mapped data and aesthetcis with their guides and bounds. -}
debugInfo :: Show a => [a] -> [Aesthetic a] -> B.Box
debugInfo dat aess = box
  where
    box = B.text "N/A"

  --   thePlot@(SinglePlot mappedData2 _ boundsM guidesM _ _) = createSinglePlot [] dat aess mempty
  --   aKeys       = Map.keys $ mconcat mappedData2
  --   scaleBaseNM = aestheticScaleBaseName (mconcat aess) dat :: Map Key Str
  --
  --   {-
  --     Create some basic text-based table views for he
  --     Based on the 'boxes' package.
  --   -}
  --   rawDataTable = B.vcat B.left $ map (toBox) dat
  --   mappedDataTable = makeTable (fmap (toBox) $ aKeys)
  --     (fmap (\aesMap -> fmap (\k -> maybe "" toBox $ Map.lookup k aesMap) aKeys) mappedData2)
  --   -- tab1a = makeTable (fmap (toBox) $ aKeys)
  --     -- (fmap (\aesMap -> fmap (\k -> maybe "" toBox $ Map.lookup k aesMap) aKeys) specialData)
  --   scaledDataTable = makeTable (fmap (toBox) $ aKeys)
  --     (fmap (\aesMap -> fmap (\k -> maybe "" toBox $ Map.lookup k aesMap) aKeys) (mappedAndScaledDataWithSpecial Nothing thePlot))
  --   aestheticTableTable = makeTable ["Aesthetic", "Scale base", "PlotBounds", "Guide"]
  --     (fmap (\k ->
  --       [ toBox k
  --       , B.text $ maybe "" show $ Map.lookup k scaleBaseNM
  --       , B.text $ maybe "" show $ Map.lookup k boundsM
  --       , B.text $ maybe "" show $ Map.lookup k guidesM
  --       ]) $ Map.keys guidesM)
  --
  --   box = B.vsep 1 B.left
  --     [ "Raw data    " B.<+> rawDataTable
  --     , "Mapped data " B.<+> mappedDataTable -- TODO show "special" data here too!
  --     , "Scaled data " B.<+> scaledDataTable
  --     , "Aesthetics  " B.<+> aestheticTableTable
  --     ]
  --
  --   toBox :: Show a => a -> B.Box
  --   toBox = B.text . show
  --
  --   -- | Make a box table (row-major order).
  --   makeTableNoHeader :: [[B.Box]] -> B.Box
  --   makeTableNoHeader x' = B.hsep 1 B.center1 $ fmap (B.vsep 0 B.top) $ Data.List.transpose x'
  --
  --   -- | Make a box table (row-major order), first argument is headers.
  --   makeTable :: [B.Box] -> [[B.Box]] -> B.Box
  --   makeTable headers rows = makeTableNoHeader $ headers : belowHeaderLines : rows
  --     where
  --       longestHeader = maximum $ fmap (length . B.render) headers
  --       belowHeaderLines = replicate (length headers) (B.text $ replicate longestHeader '-')




-- High level API, all wrappers around (Plot(..), createSinglePlot, drawPlot)

{-|
Create a visualization the given data set using the given aesthetics and geometries.
-}
plot :: [a] -> [Aesthetic a] -> Geometry -> Plot
plot dat aess geom = Plot [createSinglePlot [] dat aess geom]

{-|
Create a visualization the given data set using the given aesthetics and geometries.
-}
plotWithTitles :: [Str] -> [a] -> [Aesthetic a] -> Geometry -> Plot
plotWithTitles titles dat aess geom = Plot [createSinglePlot titles dat aess geom]

plotLabel :: Str -> [a] -> [Aesthetic a] -> Plot
plotLabel text dat baseAes =
  plot dat (baseAes <> [text >$ label]) labelG

plotImage :: Drawing -> [a] -> [Aesthetic a] -> Plot
plotImage drawing dat baseAes =
  plot dat (baseAes <> [drawing >$ image]) imageG

plotXIntercept :: [a] -> [Aesthetic a] -> Plot
plotXIntercept dat baseAes =
  plot dat (baseAes <> [True >$ crossLineX]) xIntercept


{-| Convenient wrapper for 'visualize' using 'mempty' style. -}
visualize :: Show s => [Str] -> [s] -> Geometry -> [Aesthetic s] -> Drawing
visualize axesNames d g a = Lubeck.DV.Styling.withDefaultStyle $ visualizeWithStyle axesNames d g a

{-|
The main entry-point of the library.
-}
visualizeWithStyle :: Show s => [Str] -> [s] -> Geometry -> [Aesthetic s] -> Styled Drawing
visualizeWithStyle axesNames dat geom aess = drawPlot $ plotWithTitles axesNames dat aess geom

visualizeTest :: Show s => [s] -> Geometry -> [Aesthetic s] -> IO ()
visualizeTest dat geom aess = do
  -- printDebugInfo dat aess
  putStrLn $ B.render $ debugInfo dat aess
  let finalD = visualizeWithStyle ["FIRST AXIS", "SECOND AXIS"] dat geom aess
  exportTestDrawing mempty mempty finalD

exportTestDrawing :: RenderingOptions -> Styling -> Styled Drawing -> IO ()
exportTestDrawing drawOpts style finalD = do
  let svgS = Lubeck.Drawing.toSvgStr drawOpts $ ($ style) $ Lubeck.DV.Styling.getStyled finalD
  return ()
  -- withSystemTempDirectory "lubeck-dv-test" $ \dir -> do
  let dir = "/tmp/lubeck-dv-test"
  do
    createDirectoryIfMissing True dir
    writeFile (dir ++ "/test.svg") $ unpackStr svgS
    return ()
