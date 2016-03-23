
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
  , x
  , y
  , color
  , strokeColor
  , fillColor
  , size
  , shape
  , thickness
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
  , visualize
  , visualizeWithStyle
  , visualizeTest
  )
where

import BasePrelude
import Debug.Trace(trace) -- TODO debug
import Control.Lens(Getter, to)
import Control.Lens(_1, _2, _3, _4) -- TODO debug
import Control.Lens.Operators hiding ((<~))
import Control.Lens.TH
import Data.Functor.Contravariant
import Data.Functor.Identity
import Data.Map(Map)
import Data.Proxy
import Data.Time(UTCTime)
import Linear.Affine
import Linear.V0
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import qualified Data.List
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Time
import qualified Data.Time.Format
import qualified Text.PrettyPrint.Boxes as B
import Control.Monad.Reader (ask)
import qualified Data.Colour.Names as Colors

import Lubeck.Drawing (Drawing, Str, toStr, packStr, unpackStr)
import Lubeck.DV.Styling (StyledT, Styled)
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
      -- ^ Given dataset @vs@, map single value @v@ into a special value.
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

  , aestheticLabels        :: [a] -> Map Key [(Double, Double, Str)]
      -- ^ Similar to guides, except map into the X-Y coordinates of the underlying plot.
      --   Mainly used for textural labels.

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
  mempty = Aesthetic mempty mempty mempty mempty mempty mempty
  mappend (Aesthetic a1 a2 a3 a4 a5 a6) (Aesthetic b1 b2 b3 b4 b5 b6) = Aesthetic (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)  (a6 <> b6)

-- | Make a custom aesthetic attribute.
customAesthetic :: HasScale a => Key -> Aesthetic a
customAesthetic = customAesthetic' scale (const Nothing) (const Nothing)

customAesthetic' :: (a -> Scale a) -> (a -> Maybe Str) -> (a -> Maybe Drawing) -> Key -> Aesthetic a
customAesthetic' scale toMStr toMDrawing n =
    Aesthetic mapping specialMapping genBounds genGuides genLabels getScaleBaseName
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

    genLabels = (const mempty)

    getScaleBaseName = \vs -> Data.Map.singleton n $ case vs of
      []    -> ""
      (v:_) -> scaleBaseName (scale v)

{-
Contramapping an 'Aesthetic' provides an aesthetic for a (non-strictly) larger type.

>>> contramap fst :: Aesthetic a -> Aesthetic (a, b)
>>> contramap toInteger :: Integral a => Aesthetic Integer -> f a
-}
instance Contravariant Aesthetic where
  contramap f (Aesthetic g g2 h i j k)
    = Aesthetic
      (\xs x -> g  (fmap f xs) (f x))
      (\xs x -> g2 (fmap f xs) (f x))
      (\xs   -> h  (fmap f xs))
      (\xs   -> i  (fmap f xs))
      (\xs   -> j  (fmap f xs))
      (\xs   -> k  (fmap f xs))

x, y, color, strokeColor, fillColor, size, shape, thickness, crossLineX, crossLineY :: HasScale a => Aesthetic a

-- | Map values to the X axis of a plot.
x = customAesthetic "x"

-- | Map values to the Y axis of a plot.
y = customAesthetic "y"

-- | Map values to the Y axis of a plot.
yMin = customAesthetic "yMin"

-- | Map values to the color of a plot element.
color = customAesthetic "color"

-- | Map values to the color of a plot element.
strokeColor = customAesthetic "strokeColor"

-- | Map values to the color of a plot element.
fillColor = customAesthetic "fillColor"

-- | Map values to the size of a plot element.
size = customAesthetic "size"

-- | Map values to the shape of a plot element.
shape = customAesthetic "shape"

-- | Map values to the thickness of a plot element.
thickness = customAesthetic "thickness"

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
  contramap f (Scale r b t n) = Scale (\vs v -> r (fmap f vs) (f v)) (b . fmap f) (t . fmap f) n

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

#ifdef __GHCJS__
instance HasScale Str where
  scale x = contramap unpackStr $ scale (unpackStr x)
#endif

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
  , scaleGuides   = \vs -> zipWith (\k v -> (k, toStr v)) [1..] [minBound..maxBound `asTypeOf` head vs]
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
-}
linearIntegral :: (Integral a, Show a) => Scale a
linearIntegral = linearWithOptions True UseMin

{-|
How to choose lower bound for a scale.
-}
data IntegralBounds
  = UseZero                     -- ^ Use zero.
  | InterpZeroAndMin Double     -- ^ Interpolate between zero and minimum value (0.5 for middle).
  | UseMin                      -- ^ Use minimum value.


linearWithOptions :: (Real a, Show a)
  => Bool
  -> IntegralBounds
  -> Scale a
linearWithOptions
  useIntegralShow
    -- Display numbers as integers (necessary to avoid integers being displayed as "1.0")
    -- If false, a hard-coded number of decimal places is used
  -- integralDiv
  --   -- If using integral show, divide (using 'div') before showing
  --   -- Useful for million/thousand scales etc
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

    bounds vs = (chooseLB $ realToFrac $ safeMin vs, realToFrac $ safeMax vs)
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
Map k v ~ ([Int], Int -> k, k -> v)  ~  ([Int], Int -> (k, v))  ~ [(k, v)]


[Map k v]
  ~ ([Int], Int -> Map k v)
  ~ ([Int], Int -> ([k], k -> Maybe v))


INTERESTINGLY
  [Map k v]
    ~  Map k [Maybe v]
    ~  ([k], Int, k -> Int -> Maybe v)

-}

type Coord = Normalized Double

data Geometry = Geometry
  { geomMapping         :: [Map Key (Coord, Maybe Special)] -> Styled Drawing
  -- , getSpecialGeometry  :: [Map Key Special] -> Styled Drawing
  , geomDummy :: ()
  , geomBaseName        :: [String]
  }

instance Monoid Geometry where
  mempty = Geometry mempty mempty mempty
  mappend (Geometry a1 a2 a3) (Geometry b1 b2 b3) = Geometry (a1 <> b1) (a2 <> b2) (a3 <> b3)

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


{-| Render a geometry iff a key is present and its scaled value > 0.5.

This is convenient to use with standard 'Bool' or 'Integer' scales.
-}
ifG :: Key -> Geometry -> Geometry
ifG k (Geometry f g n) = Geometry (f . filterCoords id k) g n

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
pointG = Geometry tot mempty [""]
  where
    tot ms = case colors ms of
      Nothing -> baseL 0 ms
      Just xs -> mconcat $ fmap (\color -> baseL color $ atColor color ms) xs

    baseL :: Coord -> [Map Key (Coord, a)] -> Styled Drawing
    baseL _ ms = Lubeck.DV.Drawing.scatterData $ fmap (\m -> P $ V2 (getNormalized $ m ! "x") (getNormalized $ m ! "y")) ms

line :: Geometry
line = Geometry tot mempty [""]
  where
    tot ms = case colors ms of
      Nothing -> baseL 0 ms
      Just xs -> mconcat $ fmap (\color -> baseL color $ atColor color ms) xs

    baseL :: Coord -> [Map Key (Coord, a)] -> Styled Drawing
    baseL _ ms = Lubeck.DV.Drawing.lineData $ fmap (\m -> P $ V2 (getNormalized $ m ! "x") (getNormalized $ m ! "y")) ms

fill :: Geometry
fill = Geometry tot mempty [""]
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
bars = pointG


{-|
Like 'fill', but renders the area between 'y' and 'yMin' instead of between 'y' and 0.
-}
area :: Geometry
area = Geometry tot mempty [""]
  where
    tot ms = case colors ms of
      Nothing -> baseL 0 ms
      Just xs -> mconcat $ fmap (\color -> baseL color $ atColor color ms) xs

    baseL :: Coord -> [Map Key (Coord, a)] -> Styled Drawing
    baseL _ ms = Lubeck.DV.Drawing.areaData $ fmap (\m -> P $ V3 (getNormalized $ m ! "x") (getNormalized $ m ! "yMin") (getNormalized $ m ! "y")) ms

{-|
Like 'fill', but renders the area between {x, y, bound:False} and {x, y, bound:True}
-}
area2 :: Geometry
area2 = Geometry tot mempty [""]
  where
    tot ms = case colors ms of
      Nothing -> baseL 0 ms
      Just xs -> mconcat $ fmap (\color -> baseL color $ atColor color ms) xs

    baseL :: Coord -> [Map Key (Coord, a)] -> Styled Drawing
    baseL _ ms = Lubeck.DV.Drawing.areaData ps
      where
        k = "bound"
        lowMappings  = filterCoords not k ms
        highMappings = filterCoords id  k ms
        xs  = fmap (getNormalized . (! "x")) lowMappings -- assume xs in lowMappings are the same as in highMappings
        ys1 = fmap (getNormalized . (! "y")) lowMappings
        ys2 = fmap (getNormalized . (! "y")) highMappings
        ps = zipWith3 (\x y1 y2 -> P (V3 x y1 y2)) xs ys1 ys2

    -- baseL _ ms = Lubeck.DV.Drawing.areaData $ fmap (\m -> P $ V3 (getNormalized $ m ! "x") (getNormalized $ m ! "yMin") (getNormalized $ m ! "y")) ms


-- \ Draw a line intercepting X values, iff crossLineY is present and non-zero.
xIntercept :: Geometry
xIntercept = ifG "crossLineX" (Geometry g mempty [""])
  where
   g ms = Lubeck.DV.Drawing.scatterDataX $ fmap (\m -> P $ V2 (getNormalized $ m ! "x") (getNormalized $ m ! "y")) ms

-- \ Draw a line intercepting X values, iff crossLineY is present and non-zero.
yIntercept :: Geometry
yIntercept = ifG "crossLineY" (Geometry g mempty [""])
  where
   g ms = Lubeck.DV.Drawing.scatterDataY $ fmap (\m -> P $ V2 (getNormalized $ m ! "x") (getNormalized $ m ! "y")) ms

imageG :: Geometry
imageG = Geometry g () [""]
  where
    g ms = mconcat $ fmap singleImage ms

    singleImage :: Map Key (Coord, Maybe Special) -> Styled Drawing
    singleImage m = case (m ?! "x", m ?! "y", m ?! "image") of
    -- TODO listen to width etc
      (Just (Normalized x,_), Just (Normalized y,_), Just (_,Just (SpecialDrawing dr))) -> do
        style <- ask
        return $ Lubeck.Drawing.translateX (x * style^.Lubeck.DV.Styling.renderingRectangle._x)
          $ Lubeck.Drawing.translateY (y * style^.Lubeck.DV.Styling.renderingRectangle._y)
          $ dr
      _ -> mempty

labelG :: Geometry
labelG = Geometry g () [""]
  where
    g ms = mconcat $ fmap singleLabel ms

    singleLabel :: Map Key (Coord, Maybe Special) -> Styled Drawing
    singleLabel m = case (m ?! "x", m ?! "y", m ?! "label") of
    -- TODO listen to width etc
      (Just (Normalized x,_), Just (Normalized y,_), Just (_,Just (SpecialStr str))) -> do
        style <- ask
        return $ Lubeck.Drawing.translateX (x * style^.Lubeck.DV.Styling.renderingRectangle._x)
          $ Lubeck.Drawing.translateY (y * style^.Lubeck.DV.Styling.renderingRectangle._y)
          -- TODO font
          $ text_ style str
      _ -> mempty

    text_ style = Lubeck.Drawing.textWithOptions $ mempty
      {
      Lubeck.Drawing.textAnchor = style^.Lubeck.DV.Styling.labelTextAnchor
      -- TODO read family from style
      , Lubeck.Drawing.fontFamily = style^.Lubeck.DV.Styling.labelTextFontFamily
      , Lubeck.Drawing.fontStyle  = style^.Lubeck.DV.Styling.labelTextFontStyle
      , Lubeck.Drawing.fontSize   = First $ Just $ (toStr $ style^.Lubeck.DV.Styling.labelTextFontSizePx) <> "px"
      , Lubeck.Drawing.fontWeight = style^.Lubeck.DV.Styling.labelTextFontWeight
      }

atColor :: (Eq b, Ord k, IsString k) => b -> [Map k (b, a)] -> [Map k (b, a)]
atColor c = filter (\m -> fmap fst (m ?! "color") == Just c)

-- All color values in the dataset or Nothing if there are none
colors :: [Map Key (Coord, a)] -> Maybe [Coord]
colors ms = case Data.Maybe.catMaybes $ fmap (fmap fst . (?! "color")) ms of
  [] -> Nothing
  xs -> Just $ sortNub xs
  where
    sortNub = Data.List.nub . Data.List.sort



normalizeGuides :: Map Key (Double, Double) -> Map Key [(Double, a)] -> Map Key [(Coord, a)]
normalizeGuides b m = normalizeGuides' b m

normalizeData :: Map Key (Double, Double) -> [Map Key Double] -> [Map Key (Coord)]
normalizeData b m = fmap (normalizeData' b) m

-- TODO nice Identity vs Compose pattern!
normalizeGuides' b = Data.Map.mapWithKey (\aesK dsL -> fmap (first (normalize (Data.Map.lookup aesK b))) dsL)
normalizeData'   b = Data.Map.mapWithKey (\aesK dsL ->              normalize (Data.Map.lookup aesK b)  dsL)

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
  printDebugInfo dat aess
  let finalD = visualize ["FIRST AXIS", "SECOND AXIS"] dat geom aess
  let svgS = Lubeck.Drawing.toSvgStr mempty $ finalD
  writeFile "static/tmp/test2.svg" $ unpackStr svgS
  return ()

{-| Print original data, mapped data and aesthetcis with their guides and bounds. -}
printDebugInfo :: Show s => [s] -> [Aesthetic s] -> IO ()
printDebugInfo dat aess = putStrLn $ B.render $ box
  where
    aes = mconcat aess

    guidesM     = aestheticGuides aes dat :: Map Key [(Double, Str)]
    boundsM     = aestheticBounds aes dat :: Map Key (Double, Double)
    scaleBaseNM = aestheticScaleBaseName aes dat :: Map Key Str
    mappedData          = fmap (aestheticMapping aes dat) dat :: [Map Key Double]
    specialData         = fmap (aestheticSpecialMapping aes dat) dat :: [Map Key Special]
    mappedDataWSpecial  = zipWith mergeMapsL mappedData specialData
      :: [Map Key (Double, Maybe Special)]

    mappedAndScaledData = normalizeData boundsM mappedData :: [Map Key (Coord)]
    mappedAndScaledDataWSpecial = zipWith mergeMapsL mappedAndScaledData specialData
      :: [Map Key (Coord, Maybe Special)]

    aKeys       = Data.Map.keys $ mconcat mappedData

    tab0 = B.vcat B.left $ map (toBox) dat
    tab1 = makeTable (fmap (toBox) $ aKeys)
      (fmap (\aesMap -> fmap (\k -> maybe "" toBox $ Data.Map.lookup k aesMap) aKeys) mappedData)
    tab1a = makeTable (fmap (toBox) $ aKeys)
      (fmap (\aesMap -> fmap (\k -> maybe "" toBox $ Data.Map.lookup k aesMap) aKeys) specialData)
    tab2 = makeTable (fmap (toBox) $ aKeys)
      (fmap (\aesMap -> fmap (\k -> maybe "" toBox $ Data.Map.lookup k aesMap) aKeys) mappedAndScaledData)
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
      , "Mapped (special) data " B.<+> tab1a-- TODO show "special" data here too!
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

    mergeMapsL :: Ord k => Map k a -> Map k b -> Map k (a, Maybe b)
    mergeMapsL x y = mconcat $ fmap g (Data.Map.keys x)
      where
        g k = case (Data.Map.lookup k x, Data.Map.lookup k y) of
          (Nothing, _)       -> error "mergeMapsL: Impossible as key set is drawn from first map"
          (Just xv, Nothing) -> Data.Map.singleton k (xv, Nothing)
          (Just xv, Just yv) -> Data.Map.singleton k (xv, Just yv)

{-| Convenient wrapper for 'visualize' using 'mempty' style. -}
visualize :: Show s => [Str] -> [s] -> Geometry -> [Aesthetic s] -> Drawing
visualize axesNames d g a = Lubeck.DV.Styling.withDefaultStyle $ visualizeWithStyle axesNames d g a


{-| The main entry-point for the library. -}
-- data/trans (our [s]),
-- scale (implied by the aesthetic),
-- coord (implied by the geometry)
-- guide/elem (implied by data, geometry and aesthetic)

visualizeWithStyle :: Show s => [Str] -> [s] -> Geometry -> [Aesthetic s] -> Styled Drawing
visualizeWithStyle axesNames1 dat (Geometry geom geomSpecial _) aess =
  let dataD = geom mappedAndScaledDataWSpecial --  :: StyledT M Drawing
      ticksD = drawTicks (guidesM ? "x") (guidesM ? "y") --  :: StyledT M Drawing
      axesNames = axesNames1 ++ repeat ""
      axesD  = Lubeck.DV.Drawing.labeledAxis (axesNames !! 0) (axesNames !! 1)
  in mconcat [dataD, axesD, ticksD]
  where
    aes                 = mconcat aess
    boundsM             = aestheticBounds aes dat --  :: Map Key (Double, Double)
    guidesM2            = aestheticGuides aes dat --  :: Map Key [(Double, Str)]
    guidesM             = normalizeGuides boundsM guidesM2 :: Map Key [(Coord, Str)]

    mappedData2         = fmap (aestheticMapping aes dat) dat        :: [Map Key Double]
    specialData         = fmap (aestheticSpecialMapping aes dat) dat :: [Map Key Special]
    mappedAndScaledData = normalizeData boundsM mappedData2          :: [Map Key Coord]
    mappedAndScaledDataWSpecial = zipWith mergeMapsL mappedAndScaledData specialData
      :: [Map Key (Coord, Maybe Special)]

    drawTicks xs ys = Lubeck.DV.Drawing.ticks (fmap (first getNormalized) xs) (fmap (first getNormalized) ys)

    mergeMapsL :: Ord k => Map k a -> Map k b -> Map k (a, Maybe b)
    mergeMapsL x y = mconcat $ fmap g (Data.Map.keys x)
      where
        g k = case (Data.Map.lookup k x, Data.Map.lookup k y) of
          (Nothing, _)       -> error "mergeMapsL: Impossible as key set is drawn from first map"
          (Just xv, Nothing) -> Data.Map.singleton k (xv, Nothing)
          (Just xv, Just yv) -> Data.Map.singleton k (xv, Just yv)


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

data Person = P1 { personName :: Name, personAge :: Int, personHeight :: Double }
  deriving (Eq, Ord, Show)

data Person2 = P2 { person2Name :: Name, person2Age :: Int, person2Height :: Double, person2Gender :: Gender }
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
  , x <~ to fst
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

test8b = visualizeTest dat2 (mconcat [line, fill])
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
test8c = visualizeTest dat2 (mconcat [area2])
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
test8 = visualizeTest dat (mconcat [area])
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
    customDr = Lubeck.Drawing.fillColor Colors.whitesmoke $ Lubeck.Drawing.scale 50 $ Lubeck.Drawing.square

    dat :: [(Int,Int)]
    dat = zip
      [1..4] [1..4]
