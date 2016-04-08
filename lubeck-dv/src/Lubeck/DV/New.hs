
{-# LANGUAGE
    TemplateHaskell
  , RankNTypes
  , NoMonomorphismRestriction
  , MultiParamTypeClasses
  , FunctionalDependencies
  , TypeFamilies
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  , TupleSections
  , FlexibleContexts
  , NoImplicitPrelude
  , QuasiQuotes
  , CPP
  , TypeSynonymInstances
  #-}

{-|
A high-level type safe Data Visualization library.

TODO basic tutorial

Example
---

@
newtype Day = Day Int
  deriving (Eq, Ord, Show, Num, Real, HasScale)

data LikeType = ProjLike | RealLike
  deriving (Eq, Ord, Show)

instance HasScale LikeType where scale _ = categorical

data LikeCount = LikeCount { _time :: Day, _count :: Int, _likeType :: LikeType }
  deriving (Eq, Ord, Show)

$(makeLenses ''LikeCount)

likeCounts :: [LikeCount]
likeCounts =
  [ LikeCount 0 112000 RealLike
  , LikeCount 1 115000 RealLike
  , LikeCount 1 118000 RealLike
  , LikeCount 0 112000 ProjLike
  , LikeCount 1 113000 ProjLike
  , LikeCount 1 114000 ProjLike
  ]

test = visualizeTest likeCounts fill
  [ x     <~ time
  , y     <~ count
  , color <~ likeType
  ]
@
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
  , IntegralBounds(..)
  , linearWithOptions
  , categorical
  , CategoricalBounds(..)
  , categoricalWithOptions
  , categoricalEnum
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
  , ifG
  , pointG
  , line
  , fill
  , bars
  , area
  , area2
  , xIntercept
  , yIntercept
  , labelG
  , imageG
  -- ** Legacy
  , scatter

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
  -- * Intervals/Bounds
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



  -- * Top-level (old new)
  , visualize
  , visualizeWithStyle
  -- * Top-level (new new)
  , Plot
  , visualizePlot
  , createPlot
  -- * Debug
  , visualizeTest
  , exportTestDrawing
  )
where

import BasePrelude
import Control.Lens(Getter, to)
import Control.Lens(_1, _2, _3, _4) -- TODO debug
import Control.Lens.Operators hiding ((<~))
import Control.Lens.TH (makeLenses, makeFields)
import Control.Monad.Reader (ask)
import Data.Functor.Contravariant (Contravariant(..))
import Data.Map(Map)
import Data.Time(UTCTime)
import Linear.Affine (Point(..))
import Linear.V1 (V1(..), _x)
import Linear.V2 (V2(..), _y)
import Linear.V3 (V3(..))
import NeatInterpolation(string)

import qualified Data.Char
import qualified Data.List
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Time
import qualified Data.Time.Format
import qualified Text.PrettyPrint.Boxes as B
import qualified Data.Colour.Names as Colors

import Lubeck.Drawing (Drawing, Str, toStr, packStr, unpackStr)
import Lubeck.Drawing (RenderingOptions(..), OriginPlacement(..)  )
import Lubeck.DV.Styling (StyledT, Styled, Styling, renderingRectangle)

import qualified Lubeck.Drawing
import qualified Lubeck.DV.Drawing
import qualified Lubeck.DV.Styling

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
crossWith f a b = take (length a `max` length b) $ zipWith f (cycle a) (cycle b)



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
Special values that can be embedded in a visualizations.
Just strings (for labels) and drawings (for embedded images) for now.
-}
data Special
  = SpecialStr Str
  | SpecialDrawing Drawing

instance Show Special where
  show (SpecialStr x) = show x
  show (SpecialDrawing _) = "<drawing>"

{-|
An 'Aesthetic' maps a single tuple or record to a set of aesthetic attributes such
as position, color or shape.
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
  , aestheticBounds        :: [a] -> Map Key (Double, Double)
      -- ^ Given a data set, return @(min, max)@ values to visualize (assuming
      --   the same mapping as 'aestheticMapping').
      --
      --   See also 'scaleBounds'.
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
instance Monoid (Aesthetic a) where
  mempty = Aesthetic mempty mempty mempty mempty mempty
  mappend (Aesthetic a1 a2 a3 a4 a5) (Aesthetic b1 b2 b3 b4 b5)
    = Aesthetic (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)

-- | Make a custom aesthetic attribute.
customAesthetic :: HasScale a => Key -> Aesthetic a
customAesthetic = customAesthetic' scale (const Nothing) (const Nothing)

customAesthetic' :: (a -> Scale a) -> (a -> Maybe Str) -> (a -> Maybe Drawing) -> Key -> Aesthetic a
customAesthetic' scale toMStr toMDrawing n =
    Aesthetic mapping specialMapping genBounds genGuides getScaleBaseName
  where
    mapping       = \vs v -> Data.Map.singleton n $ scaleMapping (scale v) vs v

    specialMapping = \_  v -> case toMStr v of
      Just str -> Data.Map.singleton n $ SpecialStr str
      Nothing -> case toMDrawing v of
        Just dr -> Data.Map.singleton n $ SpecialDrawing dr
        Nothing -> mempty

    genBounds = \vs -> Data.Map.singleton n $ case vs of
      []    -> (0,0)
      (v:_) -> scaleBounds (scale v) vs

    genGuides  = \vs -> Data.Map.singleton n $ case vs of
      []    -> []
      (v:_) -> scaleGuides (scale v) vs

    getScaleBaseName = \vs -> Data.Map.singleton n $ case vs of
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




x, y, color, strokeColor, fillColor, size, shape, thickness, crossLineX, crossLineY :: HasScale a => Aesthetic a

-- | Map values to the X axis of a plot.
x = customAesthetic "x"

-- | Map values to the Y axis of a plot.
y = customAesthetic "y"

-- | Map values to the Y axis of a plot.
-- This is used for the lower bound of the 'area' geometry.
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

-- | Defines the bound of an interval (lower or upper). Works with their
-- standard 'Bool' scale.
--
-- Used by the 'area2' geometry.
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

data Scale a = Scale
  { scaleMapping  :: [a] -> a -> Double
      -- ^ Given dataset @vs@, map single value @v@ into the real domain.
      --   You can construct a linear scale using @\_ x -> x@.
  , scaleBounds   :: [a] -> (Double, Double)
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
data CategoricalBounds
  = UseDataBounds             -- ^ Bounds will be @[1..n]@ where n is number of elements
  | PadDataBounds Int Int     -- ^ Using (AddToDataBounds a b), bounds will be @[1-a..n+b]@ where n is number of elements
  | Standard                  -- ^ Same as @PadDataBounds 1 1@
categoricalWithOptions :: (Ord a, Show a)
  => CategoricalBounds
  -> Scale a
categoricalWithOptions categoricalBounds = Scale
  { scaleMapping  = \vs v -> realToFrac $ succ $ findPlaceIn (sortNub vs) v
  , scaleBounds   = bounds
  , scaleGuides   = \vs -> zipWith (\k v -> (realToFrac k, toStr v)) [1..] (sortNub vs)
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
    bounds = case categoricalBounds of
      Standard      -> bounds' 1 1
      UseDataBounds -> bounds' 0 0
      (PadDataBounds a b) -> bounds' a b


    sortNub = Data.List.nub . Data.List.sort

{-|
A scale for small countable sets.

Guides are generated for all values in the domain, whether they appear in they
data set or not. Use 'categorical' if this is not desired.
-}
categoricalEnum :: (Enum a, Bounded a, Show a) => Scale a
categoricalEnum = Scale
  { scaleMapping  = \vs v -> realToFrac $ succ $ fromEnum v
  , scaleBounds   = \vs -> (0, realToFrac $ fromEnum (maxBound `asTypeOf` head vs) + 2)
  , scaleGuides   = \vs -> zipWith (\k v -> (k, toStr v)) [1..]
    [minBound..maxBound `asTypeOf` head vs]
  , scaleBaseName = "categoricalEnum"
  }
-- TODO could be written without the asTypeOf using ScopedTypeVariables

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

TODO should really be called 'LinearBounds'.
-}
data IntegralBounds
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
  -> IntegralBounds
    -- ^ How to deterine bounds .
  -> Scale a
linearWithOptions
  useIntegralShow
  lowerBoundChoice
  = Scale
  { scaleMapping  = \vs v -> realToFrac v
  -- TODO resize LB to 0?
  , scaleBounds   = bounds
  -- TODO more alternatives
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

timeScaleWithOptions :: TimeRendering -> Scale UTCTime
timeScaleWithOptions timeRendering = Scale
  { scaleMapping  = mapping
  , scaleBounds   = bounds . fmap toNDiffTime
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

-- TOP-LEVEL

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
Assuming (Ord k) =>

[a]     ~ (Int, Int -> a)
Map k v ~ ([k], k -> Maybe v)
Map k v ~ (Int, Int -> k, Int -> v)  ~  (Int, Int -> (k, v))  ~ [(k, v)]


[Map k v]
  ~ (Int, Int -> Map k v)
  ~ (Int, Int -> ([k], k -> Maybe v))


INTERESTINGLY
  [Map k v]
    ~  Map k [Maybe v]
    ~  ([k], Int, k -> Int -> Maybe v)

-}

{-
A value in @[ x | 0 <= x <= 1]@.

Used to represent scaled data.
-}
type Coord = Normalized Double

data Geometry = Geometry
  { geomMapping         :: [Map Key (Coord, Maybe Special)] -> Styled Drawing
  , geomBaseName        :: [String]
  }

instance Monoid Geometry where
  mempty = Geometry mempty mempty
  mappend (Geometry a1 a2) (Geometry b1 b2) = Geometry (a1 <> b1) (a2 <> b2)

geom_blank = mempty


-- TODO use GG/ggplot terminology vs Wilkinson
-- Wilkinson: Point, Line, Area, Interval, Path, Schema ("box"), Contour

-- TODO conditional plots (i.e. only show cross line if some "aesthetic" is set)

-- TODO interval/area

-- TODO stacking/dodging/jittering

-- TODO how do we know what aesthetics a certain plot listens to?
-- Is this all dynamic or is types involved?

-- TODO ablines, i.e. intercepting lines
-- http://docs.ggplot2.org/current/geom_abline.html


{-
TODO
GGplot geoms with their Wilkinson equivalents

geom_blank
  mempty
  Wilkinson: Point
geom_point
  Wilkinson: Line, Path
geom_segment/geom_curve
  Wilkinson: Line, Path
geom_path/geom_line/geom_step
  Wilkinson: Path
geom_bar
  Wilkinson: Point, Interval
  geom_boxplot(stat_boxplot)
  Wilkinson: Schema


geom_abline(geom_hline, geom_vline)
geom_bin2d(stat_bin2d, stat_bin_2d)
geom_contour(stat_contour)
geom_count(stat_sum)
geom_crossbar(geom_errorbar, geom_linerange, geom_pointrange)
geom_density(stat_density)
geom_density_2d(geom_density2d, stat_density2d, stat_density_2d)
geom_dotplot
geom_errorbarh
geom_freqpoly(geom_histogram, stat_bin)
geom_hex(stat_bin_hex, stat_binhex)
geom_jitter
geom_label(geom_text)
geom_map
geom_polygon
geom_quantile(stat_quantile)
geom_raster(geom_rect, geom_tile)
geom_ribbon(geom_area)
geom_rug
geom_smooth(stat_smooth)
geom_violin(stat_ydensity)
-}


{-| Render a geometry iff a key is present and has a (scaled) value @> 0.5@.

This is convenient to use with standard 'Bool' or 'Integer' scales.
-}
ifG :: Key -> Geometry -> Geometry
ifG k (Geometry f n) = Geometry (f . filterCoords id k) n

filterCoords :: (Bool -> Bool) -> Key -> [Map Key (Coord, a)] -> [Map Key (Coord, a)]
filterCoords boolF k = filter (\m -> boolF $ truish $ m ?! k)
  where
    truish Nothing                  = False
    truish (Just (Normalized n, _)) = n > 0.5


{-# DEPRECATED scatter "Use 'pointG" #-}
scatter :: Geometry
scatter = pointG

-- TODO change fillColor/strokeColor/strokeWith/strokeType/shape

pointG :: Geometry
pointG = Geometry tot [""]
  where
    tot ms = case colors ms of
      Nothing -> baseL 0 ms
      Just xs -> mconcat $ fmap (\color -> baseL color $ atColor color ms) xs

    baseL :: Coord -> [Map Key (Coord, a)] -> Styled Drawing
    baseL _ ms = Lubeck.DV.Drawing.scatterData $ fmap (\m -> P $
      V2 (getNormalized $ m ! "x") (getNormalized $ m ! "y")) ms

line :: Geometry
line = Geometry tot [""]
  where
    tot ms = case colors ms of
      Nothing -> baseL 0 ms
      Just xs -> mconcat $ fmap (\color -> baseL color $ atColor color ms) xs

    baseL :: Coord -> [Map Key (Coord, a)] -> Styled Drawing
    baseL _ ms = Lubeck.DV.Drawing.lineData $ fmap (\m -> P $
      V2 (getNormalized $ m ! "x") (getNormalized $ m ! "y")) ms

fill :: Geometry
fill = Geometry tot [""]
  where
    tot ms = case colors ms of
      Nothing -> baseL 0 ms
      Just xs -> mconcat $ fmap (\color -> baseL color $ atColor color ms) xs

    baseL :: Coord -> [Map Key (Coord, a)] -> Styled Drawing
    baseL _ ms = Lubeck.DV.Drawing.fillData $ fmap (\m -> P $ V2 (getNormalized $ m ! "x") (getNormalized $ m ! "y")) ms

{-|
Like 'point', but renders the set of values between 'y' and 'yMin' instead of 'y'.

Dims            x,y   x,0,y x,yMin,y
Not connected   point bar   interval
Connected       line  fill  area

-- Wilkinson: Point, Line, Area, Interval, Path, Schema ("box"), Contour

-}
bars :: Geometry
bars = Geometry tot [""]
  where
    tot ms = case colors ms of
      Nothing -> baseL 0 ms
      Just xs -> mconcat $ fmap (\color -> baseL color $ atColor color ms) xs

    baseL :: Coord -> [Map Key (Coord, a)] -> Styled Drawing
    baseL _ ms = Lubeck.DV.Drawing.barData $ fmap (\m -> P $
      V1 (getNormalized $ m ! "y")
        -- (getNormalized $ m ! "y")
        )
        ms


{-|
Like 'fill', but renders the area between 'y' and 'yMin' instead of between 'y' and 0.
-}
area :: Geometry
area = Geometry tot [""]
  where
    tot ms = case colors ms of
      Nothing -> baseL 0 ms
      Just xs -> mconcat $ fmap (\color -> baseL color $ atColor color ms) xs

    baseL :: Coord -> [Map Key (Coord, a)] -> Styled Drawing
    baseL _ ms = Lubeck.DV.Drawing.areaData $ fmap (\m -> P $
      V3 (getNormalized $ m ! "x") (getNormalized $ m ! "yMin") (getNormalized $ m ! "y")) ms

{-|
Like 'fill', but renders the area between {x, y, bound:False} and {x, y, bound:True}
-}
area2 :: Geometry
area2 = Geometry tot [""]
  where
    tot ms = case colors ms of
      Nothing -> baseL 0 ms
      Just xs -> mconcat $ fmap (\color -> baseL color $ atColor color ms) xs

    baseL :: Coord -> [Map Key (Coord, a)] -> Styled Drawing
    baseL _ ms = Lubeck.DV.Drawing.areaData' (ps1 <> reverse ps2)
      where
        k = "bound"
        lowMappings  = filterCoords not k ms
        highMappings = filterCoords id  k ms

        xs1 = fmap (getNormalized . (! "x")) lowMappings
        ys1 = fmap (getNormalized . (! "y")) lowMappings
        ps1 = zipWith (\x y -> P (V2 x y)) xs1 ys1

        xs2 = fmap (getNormalized . (! "x")) highMappings
        ys2 = fmap (getNormalized . (! "y")) highMappings
        ps2 = zipWith (\x y -> P (V2 x y)) xs2 ys2

        -- xs  = fmap (getNormalized . (! "x")) lowMappings -- assume xs in lowMappings are the same as in highMappings
        -- ys1 = fmap (getNormalized . (! "y")) lowMappings
        -- ys2 = fmap (getNormalized . (! "y")) highMappings
        -- ps = zipWith3 (\x y1 y2 -> P (V3 x y1 y2)) xs ys1 ys2

    -- baseL _ ms = Lubeck.DV.Drawing.areaData $ fmap (\m -> P $ V3
      -- (getNormalized $ m ! "x") (getNormalized $ m ! "yMin") (getNormalized $ m ! "y")) ms


-- \ Draw a line intercepting X values, iff crossLineY is present and non-zero.
xIntercept :: Geometry
xIntercept = ifG "crossLineX" (Geometry g [""])
  where
   g ms = Lubeck.DV.Drawing.scatterDataX $ fmap (\m -> P $
    V2 (getNormalized $ m ! "x") (getNormalized $ m ! "y")) ms

-- \ Draw a line intercepting X values, iff crossLineY is present and non-zero.
yIntercept :: Geometry
yIntercept = ifG "crossLineY" (Geometry g [""])
  where
   g ms = Lubeck.DV.Drawing.scatterDataY $ fmap (\m -> P $
    V2 (getNormalized $ m ! "x") (getNormalized $ m ! "y")) ms

imageG :: Geometry
imageG = Geometry g [""]
  where
    g ms = mconcat $ fmap singleImage ms

    singleImage :: Map Key (Coord, Maybe Special) -> Styled Drawing
    singleImage m = let
      size = case (m ?! "size") of
        Nothing               -> 1
        Just (Normalized x,_) -> x
      in case (m ?! "x", m ?! "y", m ?! "image") of
        -- TODO listen to width etc
        (Just (Normalized x,_), Just (Normalized y,_), Just (_,Just (SpecialDrawing dr))) -> do
          style <- ask
          return $ Lubeck.Drawing.translateX (x * style^.Lubeck.DV.Styling.renderingRectangle._x)
            $ Lubeck.Drawing.translateY (y * style^.Lubeck.DV.Styling.renderingRectangle._y)
            $ Lubeck.Drawing.scale size
            $ dr
        _ -> mempty

labelG :: Geometry
labelG = Geometry g [""]
  where
    g ms = mconcat $ fmap singleLabel ms

    singleLabel :: Map Key (Coord, Maybe Special) -> Styled Drawing
    singleLabel m = case (m ?! "x", m ?! "y", m ?! "label") of
    -- TODO listen to width etc
      (Just (Normalized x,_), Just (Normalized y,_), Just (_,Just (SpecialStr ""))) -> mempty
      (Just (Normalized x,_), Just (Normalized y,_), Just (_,Just (SpecialStr str))) -> do
        style <- ask
        return $ Lubeck.Drawing.translateX (x * style^.Lubeck.DV.Styling.renderingRectangle._x)
          $ Lubeck.Drawing.translateY (y * style^.Lubeck.DV.Styling.renderingRectangle._y)
          -- TODO font
          $ text_ style str
      _ -> mempty

    text_ style = fmap (Lubeck.Drawing.translate absOffset) $ Lubeck.Drawing.textWithOptions $ mempty
      {
      Lubeck.Drawing.textAnchor = style^.Lubeck.DV.Styling.labelTextAnchor
      -- TODO read family from style
      , Lubeck.Drawing.fontFamily = style^.Lubeck.DV.Styling.labelTextFontFamily
      , Lubeck.Drawing.fontStyle  = style^.Lubeck.DV.Styling.labelTextFontStyle
      , Lubeck.Drawing.fontSize   = First $ Just $ (toStr $ style^.Lubeck.DV.Styling.labelTextFontSizePx) <> "px"
      , Lubeck.Drawing.fontWeight = style^.Lubeck.DV.Styling.labelTextFontWeight
      }
      where
        absOffset = style^.Lubeck.DV.Styling.labelTextAbsOffset

atColor :: (Eq b, Ord k, IsString k) => b -> [Map k (b, a)] -> [Map k (b, a)]
atColor c = filter (\m -> fmap fst (m ?! "color") == Just c)

-- All color values in the dataset or Nothing if there are none
colors :: [Map Key (Coord, a)] -> Maybe [Coord]
colors ms = case Data.Maybe.catMaybes $ fmap (fmap fst . (?! "color")) ms of
  [] -> Nothing
  xs -> Just $ sortNub xs
  where
    sortNub = Data.List.nub . Data.List.sort





{-| Tag a value to keep track of the fact that it is /normalized/, i.e. in the unit hypercube. -}
newtype Normalized a = Normalized { getNormalized :: a }
  deriving (Eq, Ord, Num, Fractional, Real, RealFrac, Floating)

instance Show a => Show (Normalized a) where
  -- OK because of overloaded literal
  show (Normalized x) = show x

normalize :: Maybe (Double, Double) -> Double -> Coord
normalize Nothing        x = Normalized x
normalize (Just (lb,ub)) x
  -- FIXME div by zero
  | isNaN ((x - lb) / (ub - lb)) = Normalized x
  | otherwise                    = Normalized $ (x - lb) / (ub - lb)
{-
(x - lb) / (ub - lb)


-}




(!) :: (Num b, Ord k) => Map k (b, a) -> k -> b
m ! k =  maybe 0 id $ fmap fst $ Data.Map.lookup k m

(?) :: (Monoid b, Ord k) => Map k b -> k -> b
m ? k = maybe mempty id $ Data.Map.lookup k m

(?!) :: (Ord k) => Map k b -> k -> Maybe b
m ?! k = Data.Map.lookup k m








-- TOP-LEVEL

visualizeTest :: Show s => [s] -> Geometry -> [Aesthetic s] -> IO ()
visualizeTest dat geom aess = do
  -- printDebugInfo dat aess
  putStrLn $ B.render $ debugInfo dat aess
  let finalD = visualize ["FIRST AXIS", "SECOND AXIS"] dat geom aess
  let svgS = Lubeck.Drawing.toSvgStr mempty $ finalD
  writeFile "static/tmp/test2.svg" $ unpackStr svgS
  return ()

exportTestDrawing :: RenderingOptions -> Styling -> Styled Drawing -> IO ()
exportTestDrawing drawOpts style finalD = do
  let svgS = Lubeck.Drawing.toSvgStr drawOpts $ ($ style) $ Lubeck.DV.Styling.getStyled finalD
  writeFile "static/tmp/test2.svg" $ unpackStr svgS
  return ()

{-| Convenient wrapper for 'visualize' using 'mempty' style. -}
visualize :: Show s => [Str] -> [s] -> Geometry -> [Aesthetic s] -> Drawing
visualize axesNames d g a = Lubeck.DV.Styling.withDefaultStyle $ visualizeWithStyle axesNames d g a

-- {-| Print original data, mapped data and aesthetcis with their guides and bounds. -}
-- printDebugInfo :: Show s => [s] -> [Aesthetic s] -> IO ()
-- printDebugInfo dat aess = putStrLn $ debugInfo dat aess


{-| Print original data, mapped data and aesthetcis with their guides and bounds. -}
debugInfo :: Show a => [a] -> [Aesthetic a] -> B.Box
debugInfo dat aess = box
  where
    plot@(SinglePlot mappedData _ boundsM guidesM _) = createSinglePlot dat aess mempty
    aKeys       = Data.Map.keys $ mconcat mappedData
    scaleBaseNM = aestheticScaleBaseName (mconcat aess) dat :: Map Key Str

    tab0 = B.vcat B.left $ map (toBox) dat
    tab1 = makeTable (fmap (toBox) $ aKeys)
      (fmap (\aesMap -> fmap (\k -> maybe "" toBox $ Data.Map.lookup k aesMap) aKeys) mappedData)
    -- tab1a = makeTable (fmap (toBox) $ aKeys)
      -- (fmap (\aesMap -> fmap (\k -> maybe "" toBox $ Data.Map.lookup k aesMap) aKeys) specialData)
    tab2 = makeTable (fmap (toBox) $ aKeys)
      (fmap (\aesMap -> fmap (\k -> maybe "" toBox $ Data.Map.lookup k aesMap) aKeys) (mappedAndScaledDataWithSpecial Nothing plot))
    tab = makeTable ["Aesthetic", "Scale base", "Bounds", "Guide"]
      (fmap (\k ->
        [ toBox k
        , B.text $ maybe "" show $ Data.Map.lookup k scaleBaseNM
        , B.text $ maybe "" show $ Data.Map.lookup k boundsM
        , B.text $ maybe "" show $ Data.Map.lookup k guidesM
        ]) $ Data.Map.keys guidesM)

    box = B.vsep 1 B.left
      [ "Raw data    " B.<+> tab0
      , "Mapped data " B.<+> tab1 -- TODO show "special" data here too!
      , "Scaled data " B.<+> tab2
      , "Aesthetics  " B.<+> tab
      ]
    toBox = B.text . show

    -- | Make a box table (row-major order).
    makeTableNoHeader :: [[B.Box]] -> B.Box
    makeTableNoHeader x = B.hsep 1 B.center1 $ fmap (B.vsep 0 B.top) $ Data.List.transpose x

    -- | Make a box table (row-major order), first argument is headers.
    makeTable :: [B.Box] -> [[B.Box]] -> B.Box
    makeTable headers rows = makeTableNoHeader $ headers : belowHeaderLines : rows
      where
        longestHeader = maximum $ fmap (length . B.render) headers
        belowHeaderLines = replicate (length headers) (B.text $ replicate longestHeader '-')


{-|
The main entry-point of the library.
-}
visualizeWithStyle :: Show s => [Str] -> [s] -> Geometry -> [Aesthetic s] -> Styled Drawing
visualizeWithStyle axesNames1 dat (Geometry drawData _) aess =
  let dataD     = drawData (mappedAndScaledDataWithSpecial Nothing plot)  :: Styled Drawing
      guidesD   = drawGuides (scaledGuides Nothing plot ? "x") (scaledGuides Nothing plot ? "y")    :: Styled Drawing
      axesD     = Lubeck.DV.Drawing.labeledAxis (axesNames !! 0) (axesNames !! 1) :: Styled Drawing
  in mconcat [dataD, axesD, guidesD]
  where
    axesNames = axesNames1 ++ repeat ""                              :: [Str]
    drawGuides xs2 ys2 = Lubeck.DV.Drawing.ticks (fmap (first getNormalized) xs) (fmap (first getNormalized) ys)
      where
        xs = fmap (second Just) xs2
        ys = fmap (second Just) ys2
    plot = createSinglePlot dat aess mempty

createSinglePlot :: [a] -> [Aesthetic a] -> Geometry -> SinglePlot
createSinglePlot dat aess geometry =
  SinglePlot mappedData specialData guides bounds geometry
  where
    aes                 = mconcat aess
    bounds              = aestheticBounds aes dat                       :: Map Key (Double, Double)
    guides              = aestheticGuides aes dat                       :: Map Key [(Double, Str)]
    scaledGuides        = normalizeGuides bounds guides                 :: Map Key [(Coord, Str)]
    specialData         = fmap (aestheticSpecialMapping aes dat) dat    :: [Map Key Special]
    mappedData          = fmap (aestheticMapping aes dat) dat           :: [Map Key Double]


scaledGuides :: Maybe Bounds -> SinglePlot -> Map Key [(Coord, Str)]
scaledGuides (Just bounds) (SinglePlot _ _ guides _ _)      = normalizeGuides bounds guides
scaledGuides Nothing       (SinglePlot _ _ guides bounds _) = normalizeGuides bounds guides

mappedAndScaledDataWithSpecial :: Maybe Bounds -> SinglePlot -> [Map Key (Coord, Maybe Special)]
mappedAndScaledDataWithSpecial
  Nothing       (SinglePlot mappedData specialData _ bounds _) = mappedAndScaledDataWithSpecial2 bounds mappedData specialData
mappedAndScaledDataWithSpecial
  (Just bounds) (SinglePlot mappedData specialData _ _ _)      = mappedAndScaledDataWithSpecial2 bounds mappedData specialData

mappedAndScaledDataWithSpecial2 bounds mappedData specialData
  = zipWith mergeMapsL mappedAndScaledData specialData
      :: [Map Key (Coord, Maybe Special)]
      where
        mappedAndScaledData :: [Map Key Coord]
        mappedAndScaledData = normalizeData bounds mappedData

        mergeMapsL :: Ord k => Map k a -> Map k b -> Map k (a, Maybe b)
        mergeMapsL x y = mconcat $ fmap g (Data.Map.keys x)
          where
            g k = case (Data.Map.lookup k x, Data.Map.lookup k y) of
              (Nothing, _)       -> error "mergeMapsL: Impossible as key set is drawn from first map"
              (Just xv, Nothing) -> Data.Map.singleton k (xv, Nothing)
              (Just xv, Just yv) -> Data.Map.singleton k (xv, Just yv)


type Bounds = Map Key (Double, Double)



plotBounds :: Plot -> Bounds
plotBounds (OnePlot p)    = bounds p
plotBounds (ManyPlots []) = mempty
plotBounds (ManyPlots ps) = foldr1 outerBounds (fmap bounds ps)
  where
    outerBounds :: (Ord k) => Map k (Double, Double) -> Map k (Double, Double) -> Map k (Double, Double)
    outerBounds = Data.Map.unionWith g
      where
        g (a1, b1) (a2, b2) = (a1 `min` a2, b1 `max` b2)

-- Data/guides/labels is mapped but not scaled
data SinglePlot = SinglePlot
  { mappedData        :: [Map Key Double]
  , specialData       :: [Map Key Special]
  , guides            :: Map Key [(Double, Str)]
  , bounds            :: Map Key (Double, Double)
  , geometry          :: Geometry
  }

data Plot = OnePlot SinglePlot | ManyPlots [SinglePlot]
instance Monoid Plot where
  mempty = ManyPlots []
  mappend
    (OnePlot a) (OnePlot b) = ManyPlots [a, b]
  mappend
    (OnePlot a) (ManyPlots bs) = ManyPlots (a : bs)
  mappend
    (ManyPlots as) (OnePlot b) = ManyPlots (as ++ [b])
  mappend
    (ManyPlots as) (ManyPlots bs) = ManyPlots (as ++ bs)

createPlot :: [a] -> [Aesthetic a] -> Geometry -> Plot
createPlot dat aess geom = OnePlot $ createSinglePlot dat aess geom

visualizePlot :: Plot -> Styled Drawing
visualizePlot
  (OnePlot plot) = visualizeSinglePlot (plotBounds (OnePlot plot)) True plot
visualizePlot
  (ManyPlots plots) = mconcat $ zipWith (visualizeSinglePlot (plotBounds (ManyPlots plots))) (True : repeat False) plots

visualizeSinglePlot :: Bounds -> Bool -> SinglePlot -> Styled Drawing
visualizeSinglePlot bounds includeGuides plot = mconcat [dataD, axesD, guidesD]
  where
    drawData  = geomMapping $ geometry plot
    dataD     = drawData (mappedAndScaledDataWithSpecial (Just bounds) plot)  :: Styled Drawing
    guidesD   = if not includeGuides then mempty else drawGuides (scaledGuides (Just bounds) plot ? "x") (scaledGuides (Just bounds) plot ? "y")    :: Styled Drawing
    axesD     = if not includeGuides then mempty else Lubeck.DV.Drawing.labeledAxis (axesNames !! 0) (axesNames !! 1) :: Styled Drawing

    axesNames = {-axesNames1 ++-} repeat ""                              :: [Str]
    drawGuides xs2 ys2 = Lubeck.DV.Drawing.ticks (fmap (first getNormalized) xs) (fmap (first getNormalized) ys)
      where
        xs = fmap (second Just) xs2
        ys = fmap (second Just) ys2


{-
visualizePlot plots =
  mconcat [dataD, axesD, guidesD]
  where
    dataD     = drawData (mappedAndScaledDataWithSpecial plot)  :: Styled Drawing
    guidesD   = drawGuides (scaledGuides plot ? "x") (scaledGuides plot ? "y")    :: Styled Drawing
    axesD     = Lubeck.DV.Drawing.labeledAxis (axesNames !! 0) (axesNames !! 1) :: Styled Drawing
-}

-- When drawing plots:
-- Calculate bounds using foldr maxBounds



{-
TODO embed Geom in plot such that
  - Plots are monoidal
  - Monoidal composition of plots
    - Retains association of geoms to subplots (i.e. geoms are NOT unified)
    - Does not retain association of bounds to subplots (i.e. bounds ARE unified)

-}

-- isEmptyPlot :: Plot -> Bool
-- isEmptyPlot (Plot [] [] a b _) = Data.Map.null a && Data.Map.null b
-- isEmptyPlot _ = False


-- instance Monoid Plot where
--   mempty = Plot mempty mempty mempty mempty
--   mappend a@(Plot a1 a2 a3 a4) b@(Plot b1 b2 b3 b4)
--     | isEmptyPlot a = b
--     | otherwise = Plot  (a1 <> b1)
--                         (a2 <> b2)
--                         a3
--                         (a4 `maxBounds` b4)



{-
instance Monoid Plot where
  mempty = Plot mempty mempty mempty mempty mempty mempty
  mappend (Plot a1 a2 a3 a4 a5 a6) (Plot b1 b2 b3 b4 b5 b6) =
    Plot (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6)
-}



normalizeGuides :: Map Key (Double, Double) -> Map Key [(Double, a)] -> Map Key [(Coord, a)]
normalizeGuides b m = normalizeGuides' b m

normalizeData :: Map Key (Double, Double) -> [Map Key Double] -> [Map Key (Coord)]
normalizeData b m = fmap (normalizeData' b) m

-- TODO nice Identity vs Compose pattern!
normalizeGuides' b = Data.Map.mapWithKey (\aesK dsL -> fmap (first (normalize (Data.Map.lookup aesK b))) dsL)
normalizeData'   b = Data.Map.mapWithKey (\aesK dsL ->              normalize (Data.Map.lookup aesK b)  dsL)



-- TEST

newtype Day = Day Int
  deriving (Eq, Ord, Show, Num, Real, HasScale)

data LikeType = ProjLike | RealLike
  deriving (Eq, Ord, Show)

instance HasScale LikeType where scale _ = categorical

data LikeCount = LikeCount { _time :: Day, _count :: Int, _likeType :: LikeType }
  deriving (Eq, Ord, Show)

$(makeLenses ''LikeCount)

likeCounts :: [LikeCount]
likeCounts =
  [ LikeCount 0 112000 RealLike
  , LikeCount 1 115000 RealLike
  , LikeCount 1 118000 RealLike
  , LikeCount 0 112000 ProjLike
  , LikeCount 1 113000 ProjLike
  , LikeCount 1 114000 ProjLike
  ]

test0 = visualizeTest likeCounts fill
  [ x     <~ time
  , y     <~ count
  , color <~ likeType
  ]





newtype Name = Name String deriving (Eq, Ord, Show, IsString)
data Gender = Female | Male deriving (Eq, Ord, Show)

instance HasScale Name where
  scale = const categorical

instance HasScale Gender where
  scale = const categorical

data Person = P1
  { personName :: Name
  , personAge :: Int
  , personHeight :: Double
  }
  deriving (Eq, Ord, Show)

data Person2 = P2
  { person2Name   :: Name
  , person2Age    :: Int
  , person2Height :: Double
  , person2Gender :: Gender
  }
  deriving (Eq, Ord, Show)

$(makeFields ''Person)
$(makeFields ''Person2)

males, females :: [Person]
males =
  [ P1 "Hans" 28 1.15
  , P1 "Sven" 25 1.15
  , P1 "Sven" 25 1.15
  , P1 "Sven" 25 1.15
  , P1 "Sven" 25 1.15
  , P1 "Sven" 25 1.15
  ]
females =
  [ P1 "Elin" 21 1.15
  , P1 "Alva" 19 1.15
  ]

people :: [Person2]
people = (males `cr` [Male]) <> (females `cr` [Female])
  where
    cr = crossWith (\p gender -> P2 (p^.name) (p^.age) (p^.height) gender)

test = visualizeTest people (mconcat [scatter, line, fill])
  [ mempty
  , color <~ gender
  -- , shape <~ gender
  , x     <~ name
  , y     <~ age `withScale` linear
  ]
test2 = visualizeTest ([(1,2), (3,4)] :: [(Int, Int)]) line
  [ mempty
  , x <~ _1
  , y <~ to snd
  ]
test3 = visualizeTest ( [ ] :: [(UTCTime, Int)]) line
  [ mempty
  , x <~ to fst
  , y <~ to snd
  ]

test4 = visualizeTest ("hello world" :: String) scatter [x <~ id, y <~ id]



data WD = Mon | Tues | Wed | Thurs | Fri | Sat | Sun deriving (Eq, Ord, Enum, Bounded) -- Show,
instance Show WD where
  show x = case x of
    Mon   -> "m"
    Tues  -> "t"
    Wed   -> "w"
    Thurs -> "tr"
    Fri   -> "f"
    Sat   -> "sa"
    Sun   -> "su"

instance HasScale WD where
  scale = const categoricalEnum

test5 = visualizeTest [(Mon, 100 :: Int), (Sun, 400)] line [x <~ to fst, y <~ to snd]


test6 = do
  visualizeTest dat geom aes
 where
  dat =
    [ (Mon,   10)
    , (Tues,  30)
    , (Wed,   3)
    , (Thurs, 3)
    , (Fri,   12)
    , (Sat,   3)
    , (Sun,   10 :: Int)]
  aes =
    [ x <~ to fst
    , y <~ to snd
    ]
  geom = mconcat [scatter, line, fill]

test7 = visualizeTest dat (mconcat [scatter, line, fill])
  [ mempty
  , x     <~ to (\(x,_,_) -> x)
  , y     <~ to (\(_,x,_) -> x)
  , color <~ to (\(_,_,x) -> x)
  ]
  where
    dat =
      [ (0::Int, 1::Int, True)
      , (1, 3, True)
      , (2, 0, True)
      , (3, 2, True)
      , (5, 9, True)

      , (0, 3, False)
      , (1, 2, False)
      , (2, 1, False)
      , (3, 0, False)
      , (5, 0, False)
      ]


-- test8
-- The same data plotted in 3 different ways:

-- Version I: Cross with True/False and plot 2 overlapping lines/areas
test8a = visualizeTest dat2 (mconcat [line, fill])
  [ x     <~ _1
  , y     <~ _2
  , color <~ _3
  ]
  where
    dat2 = fmap (\a -> (a^._1, a^._2, False)) dat <> fmap (\a -> (a^._1, a^._3, True)) dat
    dat =
     [ (0, 3, 12)
     , (1, 1, 12)
     , (2, 1, 16)
     , (3, 5, 16)
     , (4, 16, 1) :: (Int, Int, Int)
     ]
-- Version II: Cross with True/False use area plot with bound aesthetic (lower/upper)
test8b = visualizeTest dat2 (mconcat [area2])
  [ x     <~ _1
  , y     <~ _2
  , bound <~ _3
  ]
  where
    dat2 :: [(Int,Int,Bool)]
    dat2 = fmap (\a -> (a^._1, a^._2, False)) dat <> fmap (\a -> (a^._1, a^._3, True)) dat
    dat =
     [ (0, 3, 12)
     , (1, 1, 12)
     , (2, 1, 16)
     , (3, 5, 16)
     , (4, 16, 1) :: (Int, Int, Int)
     ]
-- Version III: Use are a plot with "two y values" (questionable).
-- Note that y and yMin needs to have the same bounds for this to work (here [1..16])
test8c = visualizeTest dat (mconcat [area])
  [ x    <~ _1
  , yMin <~ _2
  , y    <~ _3
  ]
  where
    dat =
     [ (0, 3, 12)
     , (1, 1, 12)
     , (2, 1, 16)
     , (3, 5, 16)
     , (4, 16, 1) :: (Int, Int, Int)
     ]

-- Cross-lines
test9 = visualizeTest dat (mconcat [scatter, xIntercept, yIntercept])
  [ x <~ _1 `withScale` categorical
  , y <~ _2 `withScale` linearIntegral
  , crossLineX <~ _3
  , crossLineY <~ _4
  ]
  where
    dat :: [(Int,Int,Bool,Bool)]
    dat = zip4
      [1..4] [1..4]
      [True,False,False,True] [False,False,True,True]

-- Labels and custom images
test10 = visualizeTest dat (mconcat [labelG, scatter, imageG])
  [ x <~ _1 `withScale` categorical
  , y <~ _2 `withScale` linearIntegral
  , contramap (("value is "<>). toStr) label <~ _1
  , contramap (const customDr) image <~ _2
  ]
  where
    customDr :: Drawing
    customDr = Lubeck.Drawing.fillColor Colors.whitesmoke
      $ Lubeck.Drawing.scale 50 $ Lubeck.Drawing.square

    dat :: [(Int,Int)]
    dat = zip
      [1..4] [1..4]

-- Custom image.
test11 = visualizeTest dat (mconcat [labelG, scatter, imageG])
  [ x <~ _1 `withScale` categorical
  , y <~ _2 `withScale` linearIntegral
  , contramap (const customDr) image <~ _2
  ]
  where
    customDr :: Drawing
    customDr = Lubeck.Drawing.fillColor Colors.turquoise
      $ Lubeck.Drawing.scale 50 $ Lubeck.Drawing.triangle

    dat :: [(Int,Int)]
    dat = zip
      [1..4] [1..4]

-- Custom image with size.
test12 = visualizeTest dat (mconcat [labelG, scatter, imageG])
  [ x <~ _1 `withScale` categorical
  , y <~ _2 `withScale` linearIntegral
  , size <~ _1
  , contramap (const customDr) image <~ _2
  ]
  where
    customDr :: Drawing
    customDr = Lubeck.Drawing.fillColor Colors.turquoise
      $ Lubeck.Drawing.scale 50 $ Lubeck.Drawing.triangle

    dat :: [(Int,Int)]
    dat = zip
      [1..4] [1..4]

-- Custom image with size (linearly transformed).
test13 = visualizeTest dat (mconcat [labelG, scatter, imageG])
  [ x <~ _1 `withScale` categorical
  , y <~ _2 `withScale` linearIntegral
  , contramap (\x -> -1*x + 1) size <~ _1
  , contramap (const customDr) image <~ _2
  ]
  where
    customDr :: Drawing
    customDr = Lubeck.Drawing.fillColor Colors.turquoise
      $ Lubeck.Drawing.scale 50 $ Lubeck.Drawing.triangle

    dat :: [(Int,Int)]
    dat = zip
      [1..4] [1..4]

-- Custom image with size (linearly transformed).
test14 = visualizeTest dat (mconcat [labelG, scatter, imageG])
  [ x <~ _1 `withScale` categorical
  , y <~ _2 `withScale` linearIntegral
  , contramap (\x -> -1*x + 1) size <~ _1
  , contramap (const customDr) image <~ _2
  ]
  where
    customDr :: Drawing
    customDr = Lubeck.Drawing.fillColor Colors.turquoise
      $ Lubeck.Drawing.scale 50 $ dr

    dat :: [(Int,Int)]
    dat = zip
      [1..4] [1..4]

    Just dr =
      Lubeck.Drawing.addEmbeddedSVGFromStr $ packStr $ [string|
              <rect x="-0.5" y="-0.5" width="1" height="1" style="fill:blue">
                        <animateTransform attributeName="transform"
                              attributeType="XML"
                              type="rotate"
                              from="0 0 0"
                              to="360 0 0"
                              dur="10s"
                              repeatCount="indefinite"/>
              </rect>

      |]

-- Multiple plots composed

test20 = exportTestDrawing mempty mempty $ visualizePlot $
  createPlot (zip "hans" "sven" :: [(Char,Char)]) [x<~_1,y<~_2] scatter
    <>
  createPlot (zip "hans" "svfn" :: [(Char,Char)]) [x<~_1,y<~_2] line


{-
  Same type/scale.
-}
test21 = exportTestDrawing mempty mempty $ visualizePlot $
  createPlot (zip
    ([1..10] :: [Int])
    ([2,5,1,2,5,-6,7,2,3,9] :: [Int])
    ) [x<~_1,y<~_2]
    line
    <>
  createPlot (zip
    ([1..10] :: [Int])
    ([12,5,1,2,5,-6,7,2,13,15] :: [Int])
    )
    [x<~_1,y<~_2]
    area

{-
  Different Y scales
-}
test22 = exportTestDrawing mempty mempty $ visualizePlot $
  createPlot (zip
    ([1..10] :: [Int])
    ([0,1,5,5,4,3,4,4,4,3] :: [Int])
    ) [x<~_1,y<~_2]
    line
    <>
  createPlot (zip
    ([1..10] :: [Int])
    ("AAABBBZQqz" :: [Char])
    )
    [x<~_1,y<~_2]
    area


{-
  Different X scales
-}
test23 = exportTestDrawing mempty mempty $ visualizePlot $
  createPlot (zip
    ([1..10] :: [Int])
    ([0,1,5,5,4,3,4,4,4,3] :: [Int])
    ) [x<~_1,y<~_2]
    line
    <>
  createPlot (zip
    ([11..20] :: [Int])
    ("AAABBBZQqz" :: [Char])
    )
    [x<~_1,y<~_2]
    area

{-
  Bar plot.
-}
test25 = exportTestDrawing mempty mempty $ visualizePlot $
  createPlot (zip chars freq) [x <~ _1, y <~ _2] bars
  where
    chars :: [Char]
    freq :: [Int]
    chars = sortNub text
    freq = fmap (\c -> length $ filter (== c) text) chars
    text = filter Data.Char.isAlpha $ fmap Data.Char.toUpper $ [string|
      Statistics is the study of the collection, analysis, interpretation,
      presentation, and organization of data.[1] In applying statistics
      to, e.g., a scientific, industrial, or societal problem, it is
      conventional to begin with a statistical population or a statistical
      model process to be studied. Populations can be diverse topics such
      as "all people living in a country" or "every atom composing a
      crystal". Statistics deals with all aspects of data including the
      planning of data collection in terms of the design of surveys and
      experiments.
      |]
    sortNub = Data.List.nub . Data.List.sort


{-
  Same type/scale.
-}
test24 = exportTestDrawing mempty mempty $ visualizePlot $ mconcat $ zipWith putTogether geoms dat
  where
    putTogether = \geom dat -> createPlot (zip [1..10::Int] dat) [x<~_1,y<~_2] geom
    geoms = [pointG, line, area, pointG <> line]
    dat =
      [ [2,5,1,2,5,-6,7,2,3,9] :: [Int]
      , [1,1,1,1,2,2,2,2,3,3]
      , [-1,-2,-3,-3,-3,-3,-3,-3,-3,-3]
      ]


testRad = exportTestDrawing
  (mempty { dimensions = P (V2 800 500), originPlacement = BottomLeft })
  (renderingRectangle .~ V2 800 500 $ mempty) $ visualizePlot $ mconcat
    [ createPlot dat [x<~to (!! 0), y<~to (!! 1)] line
    , createPlot dat [x<~to (!! 0), y<~to (!! 2)] line
    ]
  where
    dat = dataset1


testRad2 = exportTestDrawing
  -- (mempty { dimensions = P (V2 800 500), originPlacement = BottomLeft })
  -- (renderingRectangle .~ V2 800 500 $ mempty)
  mempty
  mempty
  $ visualizePlot $ mconcat
    [ createPlot dat [x<~to (!! 0), y<~to (!! 1)] line
    , createPlot dat [x<~to (!! 0), y<~to (!! 2)] line
    ]
  where
    dat = [ [x,cos x,sin x :: Double] | x <- [0,0.1..pi*2] ]



dataset1 :: [[Double]]
dataset1 =
      [
          [2,5.5,3.4  ],
          [3,7.9,9.4  ],
          [4,8.5,13  ],
          [5,4.9,3.2  ],
          [6,2.7,1.4  ],
          [7,5.7,1.6  ],
          [8,6.5,0.6  ],
          [9,6.8,0.4  ],
          [10,2.2,0  ],
          [11,1.4,0  ],
          [12,1.6,0  ],
          [13,-0.1,0  ],
          [14,1.7,3.4  ],
          [15,-1.1,0  ],
          [16,-1.2,0  ],
          [17,-1.4,0  ],
          [18,-3.8,0  ],
          [19,-2.6,0  ],
          [20,2.6,15.4  ],
          [21,5.5,3.2  ],
          [22,5.1,2.2  ],
          [23,5.5,2.7  ],
          [24,7.5,13  ],
          [25,6.8,12.8  ],
          [26,4,3.4  ],
          [27,2.5,0  ],
          [28,2.7,0.5  ],
          [29,5.4,11  ],
          [30,7.6,0.8  ],
          [31,6.8,1.8  ],
          [32,4.9,6.6  ],
          [33,4.9,0  ],
          [34,4.7,0  ],
          [35,4.1,0  ],
          [36,3.2,0  ],
          [37,4.8,0  ],
          [38,5.1,0  ],
          [39,6.4,5.6  ],
          [40,5.6,12.2  ],
          [41,8.1,3.6  ],
          [42,7.5,0.2  ],
          [43,6.4,2.8  ],
          [44,5.9,1  ],
          [45,4.1,0.4  ],
          [46,3.2,0  ],
          [47,4.2,5  ],
          [48,6.8,4.8  ],
          [49,4.3,20.6  ],
          [50,3.7,0  ],
          [51,4.4,2.7  ],
          [52,8.3,1.2  ],
          [53,7.1,0  ],
          [54,4.2,0.2  ],
          [55,5.2,28  ],
          [56,6,0.8  ],
          [57,0.8,0.2  ],
          [58,0.2,0  ],
          [59,2.4,0.2  ],
          [60,2.2,6.4  ],
          [61,3.9,0.4  ],
          [62,3.6,2.6  ],
          [63,9.3,0.2  ],
          [64,8.5,0.6  ],
          [65,4.4,8.7  ],
          [66,3,0  ],
          [67,2.1,0  ],
          [68,4.6,0  ],
          [69,5.1,7.6  ],
          [70,6.9,7.4  ],
          [71,6.2,0.2  ],
          [72,4.6,6.6  ],
          [73,3,0.2  ],
          [74,4.4,2.8  ],
          [75,8.4,14.2  ],
          [76,6.6,2  ],
          [77,7.2,0.4  ],
          [78,3.9,0.6  ],
          [79,2.4,0  ],
          [80,4.2,6.7  ],
          [81,4,0.2  ],
          [82,3.1,0  ],
          [83,5.1,0  ],
          [84,5.7,0  ],
          [85,8,0  ],
          [86,6.7,2.4  ],
          [87,9.3,0.4  ],
          [88,8.9,2.6  ],
          [89,7.4,8.6  ],
          [90,5.8,11.2  ],
          [91,3.7,2.6  ],
          [92,6.8,4.5  ],
          [93,9.9,0  ],
          [94,6.7,1.8  ],
          [95,4.3,0  ],
          [96,6.2,0  ],
          [97,4.8,0  ],
          [98,6.2,0  ],
          [99,8.7,0  ],
          [100,11.5,0  ],
          [101,11.6,0  ],
          [102,8.9,4.2  ],
          [103,8.4,1.2  ],
          [104,8.2,0  ],
          [105,7.9,0  ],
          [106,10.1,0  ],
          [107,9.5,8.4  ],
          [108,5.7,1  ],
          [109,10.2,1  ],
          [110,7.5,8.2  ],
          [111,9.3,0.6  ],
          [112,8.8,0  ],
          [113,13,0  ],
          [114,13.1,0  ],
          [115,11,3.6  ],
          [116,11.9,4  ],
          [117,11.1,0.2  ],
          [118,8,1.6  ],
          [119,10.3,0.2  ],
          [120,11.6,0.9  ],
          [121,10.7,5.1  ],
          [122,9,0  ],
          [123,7.4,0  ],
          [124,9.1,7.8  ],
          [125,10.5,0  ],
          [126,8.4,0  ],
          [127,9.7,0  ],
          [128,12.1,0  ],
          [129,13.7,0  ],
          [130,9.8,0  ],
          [131,8.6,0  ],
          [132,9.3,0  ],
          [133,12.8,0  ],
          [134,14.6,0  ],
          [135,15.9,0  ],
          [136,12.9,0  ],
          [137,12.4,0  ],
          [138,10.5,0  ],
          [139,9.3,0  ],
          [140,10.6,0  ],
          [141,12.4,0  ],
          [142,13.8,7  ],
          [143,11.1,0.3  ],
          [144,11.4,0.3  ],
          [145,10.9,0.2  ],
          [146,15,0  ],
          [147,16.6,0  ],
          [148,12.8,0  ],
          [149,13,0  ],
          [150,11.8,0  ],
          [151,12.1,8  ],
          [152,12.4,1.2  ],
          [153,15.8,4.4  ],
          [154,13.5,1  ],
          [155,11.5,0  ],
          [156,9.9,0.5  ],
          [157,13.3,0  ],
          [158,10.2,0.2  ],
          [159,11.6,12.2  ],
          [160,11.1,2.2  ],
          [161,12.2,0  ],
          [162,12.7,0  ],
          [163,13.8,0  ],
          [164,14.5,0  ],
          [165,14.2,0  ],
          [166,11.8,0  ],
          [167,12.1,0  ],
          [168,15.3,0.6  ],
          [169,15,0.4  ],
          [170,13.9,1.5  ],
          [171,14.4,0  ],
          [172,14.8,0  ],
          [173,16.3,0  ],
          [174,13.1,10.8  ],
          [175,13.8,5  ],
          [176,12,0  ],
          [177,14,0  ],
          [178,16.5,0  ],
          [179,13,0  ],
          [180,13.3,1.8  ],
          [181,16.7,0.2  ],
          [182,15.5,3.2  ],
          [183,16.2,0  ],
          [184,13,4.3  ],
          [185,14.3,8.9  ],
          [186,12.9,0  ],
          [187,15,0  ],
          [188,16.3,0  ],
          [189,17.5,0  ],
          [190,18.3,0  ],
          [191,17.3,0  ],
          [192,15.4,0  ],
          [193,16.8,0  ],
          [194,17.8,0  ],
          [195,18,3  ],
          [196,18.3,0  ],
          [197,15,1.8  ],
          [198,18.1,0  ],
          [199,18.5,0  ],
          [200,15.6,0  ],
          [201,17.8,0  ],
          [202,16.4,2.4  ],
          [203,17,0  ],
          [204,16.8,0  ],
          [205,17.8,0.2  ],
          [206,17.4,0  ],
          [207,17.4,0  ],
          [208,17.4,0  ],
          [209,15.8,0  ],
          [210,15.2,0  ],
          [211,16.4,0  ],
          [212,17,0  ],
          [213,17.1,0  ],
          [214,17,0  ],
          [215,17.2,0  ],
          [216,18.8,0  ],
          [217,21.6,0  ],
          [218,22.3,0  ],
          [219,17.2,0  ],
          [220,16.7,1.4  ],
          [221,15.7,0  ],
          [222,17.9,0  ],
          [223,17.2,0  ],
          [224,18.3,0  ],
          [225,20.6,0  ],
          [226,17.6,0  ],
          [227,18.4,0  ],
          [228,22.6,0  ],
          [229,22.1,0  ],
          [230,21.2,0  ],
          [231,16.2,0  ],
          [232,16.3,0  ],
          [233,17.7,0  ],
          [234,17.1,0  ],
          [235,17,0  ],
          [236,16.2,0  ],
          [237,15,0  ],
          [238,16.5,0  ],
          [239,14.2,0.2  ],
          [240,14.9,0  ],
          [241,16.5,0  ],
          [242,16,0  ],
          [243,15.1,0  ],
          [244,14.7,0  ],
          [245,14.9,0  ],
          [246,14.5,0  ],
          [247,15.2,0  ],
          [248,15.6,0  ],
          [249,15.6,0  ],
          [250,19.4,0  ],
          [251,18.6,0  ],
          [252,18.5,0  ],
          [253,14.2,0  ],
          [254,13.3,0.2  ],
          [255,10.9,0  ],
          [256,12.8,0  ],
          [257,14.9,0  ],
          [258,15.2,0  ],
          [259,14.9,0  ],
          [260,16.2,0  ],
          [261,17.3,0  ],
          [262,16.7,0  ],
          [263,14.6,0  ],
          [264,14.6,0  ],
          [265,11.5,0  ],
          [266,13.9,0  ],
          [267,13.3,0  ],
          [268,13.1,0  ],
          [269,12.9,0  ],
          [270,12.3,0  ],
          [271,14,0  ],
          [272,14.1,2  ],
          [273,12.2,0  ],
          [274,12.2,0  ],
          [275,15.3,0  ],
          [276,10.5,0  ],
          [277,11.1,0  ],
          [278,10.7,0  ],
          [279,11.5,0  ],
          [280,12.6,0  ],
          [281,13.7,0  ],
          [282,12.4,0  ],
          [283,9.3,0  ],
          [284,8.8,0  ],
          [285,10.4,0  ],
          [286,10.7,3.6  ],
          [287,13.6,12.8  ],
          [288,13.6,17.2  ],
          [289,10.2,10.1  ],
          [290,10.7,0  ],
          [291,8,0  ],
          [292,9.3,13.8  ],
          [293,10.7,0.4  ],
          [294,6.1,0  ],
          [295,6.3,0  ],
          [296,7.5,3.8  ],
          [297,7.7,4.4  ],
          [298,7.1,3.8  ],
          [299,7.9,0  ],
          [300,7.6,6.2  ],
          [301,8.7,16.2  ],
          [302,11.2,0.8  ],
          [303,12.3,15.5  ],
          [304,11.2,29.8  ],
          [305,10.5,14.8  ],
          [306,12.1,1.4  ],
          [307,11.6,3  ],
          [308,12.1,9  ],
          [309,12.8,7.4  ],
          [310,10.3,0  ],
          [311,9.1,5.3  ],
          [312,7.8,0.6  ],
          [313,6.9,0  ],
          [314,3.8,0  ],
          [315,2.1,0  ],
          [316,2.4,23.8  ],
          [317,7.3,13.6  ],
          [318,7.8,0.2  ],
          [319,5.9,0  ],
          [320,5.2,0  ],
          [321,6.2,0.2  ],
          [322,8.5,2.2  ],
          [323,7.5,8.2  ],
          [324,7,25.8  ],
          [325,7.9,7.8  ],
          [326,4.3,15  ],
          [327,4.3,0.4  ],
          [328,7.5,10.4  ],
          [329,4.7,0  ],
          [330,3.9,0  ],
          [331,3.1,0  ],
          [332,3.5,0  ],
          [333,3.4,6.4  ],
          [334,9.1,0.8  ],
          [335,9.5,14.6  ],
          [336,8.2,21  ],
          [337,8.6,10.4  ],
          [338,7.6,12.3  ],
          [339,9.6,9.6  ],
          [340,5.6,5.8  ],
          [341,4.1,2.2  ],
          [342,3.6,6.2  ],
          [343,3.5,0  ],
          [344,3.4,5.4  ],
          [345,3.6,0  ],
          [346,4.9,2.8  ],
          [347,5.3,0  ],
          [348,5.4,5.2  ],
          [349,3.4,3.4  ],
          [350,3.6,0.8  ],
          [351,4.1,22.2  ],
          [352,3.3,10.5  ],
          [353,2.5,0.4  ],
          [354,4.5,26.8  ],
          [355,3.7,7.4  ],
          [356,5.2,0.4  ],
          [357,4.4,3.8  ],
          [358,5,0.4  ],
          [359,3.9,4.5  ],
          [360,4.3,27.1  ],
          [361,5,7.8  ],
          [362,4.3,0.4  ],
          [363,5,2.2  ],
          [364,4.8,2.8  ],
          [365,2.7,0  ],
          [366,2.9,4.3  ]
          ]
