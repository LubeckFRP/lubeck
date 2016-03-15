
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
  #-}

module Lubeck.DV.New
  (
  -- * Algebra
    blendId
  , blend
  , crossWith
  -- * Scales
  , categorical
  , categoricalEnum
  , linear
  , timeScale
  , Scale
  , HasScale(..)
  , Scaled
  , withScale

  -- * Geometry
  , Geometry
  , line
  , fill
  , scatter

  -- * Aesthetics
  , x
  , y
  , color
  , strokeColor
  , fillColor
  , size
  , shape
  , thickness

  , Key
  , Aesthetic
  , customAesthetic
  -- ** Mapping aesthetics
  , (<~)
  , (~>)
  -- * Top-level
  , visualize
  , visualizeWithStyle
  , visualizeTest
  )
where

import BasePrelude
import Control.Lens(Getter, to)
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

import Lubeck.Drawing (Drawing, Str, toStr, packStr, unpackStr)
import Lubeck.DV.Styling (StyledT, Styled)
import qualified Lubeck.Drawing
import qualified Lubeck.DV.Drawing
import qualified Lubeck.DV.Styling



-- ALGEBRA

-- | A "table" with no columns.
blendId :: [a]
blendId = mempty

-- | Concatenate columns.
blend :: [a] -> [a] -> [a]
blend = (<>)

-- TODO some type class thing here for merging tuples/records
-- so i.e.
--  cross (1,2) (3,4) => (1,2,3,4)
--  cross {foo=1, bar=""} {baz=()} => {foo=1, bar="", baz=()}

-- | Concatenate rows left-to-right. If one row is shorter it is repeated.
crossWith :: (a -> b -> c) -> [a] -> [b] -> [c]
crossWith f a b = take (length a `max` length b) $ zipWith f (cycle a) (cycle b)



-- AESTHETICS

newtype Key = Key { getKey :: Str }
  deriving (Eq, Ord, IsString)

instance Show Key where
  -- OK because of the IsString instance
  show = show . getKey

{-|
An 'Aesthetic' maps a /row/ (also known as a /tuple/ or /record/) to a set of aesthetic
attribute such as position, color or shape.
-}
data Aesthetic a = Aesthetic
  { aestheticMapping       :: [a] -> a -> Map Key Double
      -- ^ Given dataset @vs@, map single value @v@ into the real domain.
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
  mempty = Aesthetic mempty mempty mempty mempty
  mappend (Aesthetic a1 a2 a3 a4) (Aesthetic b1 b2 b3 b4) = Aesthetic (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)

-- | Make a custom aesthetic attribute.
customAesthetic :: HasScale a => Key -> Aesthetic a
customAesthetic n = Aesthetic convert genBounds genGuides getScaleBaseName
  where
    convert   = \vs v -> Data.Map.singleton n $ scaleMapping (scale v) vs v
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
  contramap f (Aesthetic g h i j)
    = Aesthetic
      (\xs x -> g (fmap f xs) (f x))
      (\xs   -> h (fmap f xs))
      (\xs   -> i (fmap f xs))
      (\xs   -> j (fmap f xs))

x, y, color, strokeColor, fillColor, size, shape, thickness, crossLineX :: HasScale a => Aesthetic a

-- | Map values to the X axis of a plot.
x = customAesthetic "x"

-- | Map values to the Y axis of a plot.
y = customAesthetic "y"

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

crossLineX = customAesthetic "crossLineX"


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
  scale = const categorical

instance HasScale Ordering where
  scale = const categorical

instance HasScale Name where
  scale = const categorical

instance HasScale Gender where
  scale = const categorical

-- Questionable, but gives us the ability to treat strings as categorical values.
instance (Ord a, Show a) => HasScale [a] where
  scale = const categorical

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
categorical = Scale
  { scaleMapping  = \vs v -> realToFrac $ succ $ findPlaceIn (sortNub vs) v
  , scaleBounds   = \vs -> (0, realToFrac $ length (sortNub vs) + 1)
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
linear = Scale
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

    bounds vs = (realToFrac $ safeMin vs, realToFrac $ safeMax vs)
    guides vs = fmap (\x -> (x, toStr $ roundTo 5 x)) $ tickCalc 4 (bounds vs)

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
timeScale = contramap (`Data.Time.diffUTCTime` refTime) linear

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


-- TOP-LEVEL

-- Very similar to (>$$<)
(<~) :: Aesthetic a -> Getter s a -> Aesthetic s
(<~) a g = contramap (^.g) a

-- Very similar to (>$<)
(~>) :: Getter s a -> Aesthetic a -> Aesthetic s
(~>) g a = contramap (^.g) a

printDebugInfo :: Show s => [s] -> [Aesthetic s] -> IO () -- TODO result type
printDebugInfo dat aess = putStrLn $ B.render $ box
  where
    aes = mconcat aess

    guidesM     = aestheticGuides aes dat :: Map Key [(Double, Str)]
    boundsM     = aestheticBounds aes dat :: Map Key (Double, Double)
    scaleBaseNM = aestheticScaleBaseName aes dat :: Map Key Str
    mappedData  = fmap (aestheticMapping aes dat) dat :: [Map Key Double]
    aKeys       = Data.Map.keys $ mconcat mappedData

    tab0 = B.vcat B.left $ map (toBox) dat
    tab1 = makeTable (fmap (toBox) $ aKeys)
      (fmap (\aesMap -> fmap (\k -> maybe "" toBox $ Data.Map.lookup k aesMap) aKeys) mappedData)
    tab = makeTable ["Aesthetic", "Scale base", "Bounds", "Guide"]
      (fmap (\k ->
        [ toBox k
        , B.text $ maybe "" show $ Data.Map.lookup k scaleBaseNM
        , B.text $ maybe "" show $ Data.Map.lookup k boundsM
        , B.text $ maybe "" show $ Data.Map.lookup k guidesM
        ]) $ Data.Map.keys guidesM)

    box = B.vsep 1 B.left
      [ "Raw data    " B.<+> tab0
      , "Mapped data " B.<+> tab1
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

visualizeTest :: Show s => [s] -> Geometry -> [Aesthetic s] -> IO ()
visualizeTest dat geom aess = do
  printDebugInfo dat aess
  visualizeTest2 dat geom aess
  return ()

visualizeTest2 :: Show s => [s] -> Geometry -> [Aesthetic s] -> IO ()
visualizeTest2 dat geom aess = do
  let finalD = visualizeWithStyle ["FIRST AXIS", "SECOND AXIS"] dat geom aess
  let svgS = Lubeck.Drawing.toSvgStr mempty $ Lubeck.DV.Styling.withDefaultStyle $ finalD
  writeFile "/root/lubeck/static/tmp/test2.svg" $ unpackStr svgS
  return ()
  where
    -- aes = mconcat aess
    -- boundsM     = aestheticBounds aes dat :: Map Key (Double, Double)
    -- guidesM2    = aestheticGuides aes dat :: Map Key [(Double, Str)]
    -- guidesM = applyScalingToGuides boundsM guidesM2
    -- scaleBaseNM = aestheticScaleBaseName aes dat :: Map Key Str
    -- mappedData2  = fmap (aestheticMapping aes dat) dat :: [Map Key Double]
    -- mappedAndScaledData = applyScalingToValues boundsM mappedData2

-- TODO return drawing, not styled drawing
visualize :: Show s => [Str] -> [s] -> Geometry -> [Aesthetic s] -> Drawing
visualize axesNames d g a = Lubeck.DV.Styling.withDefaultStyle $ visualizeWithStyle axesNames d g a

visualizeWithStyle :: Show s => [Str] -> [s] -> Geometry -> [Aesthetic s] -> Styled Drawing
visualizeWithStyle axesNames1 dat (Geometry geom) aess =
  let dataD = geom mappedAndScaledData --  :: StyledT M Drawing
      ticksD = Lubeck.DV.Drawing.ticks (guidesM ? "x") (guidesM ? "y") --  :: StyledT M Drawing
      axesNames = axesNames1 ++ repeat ""
      axesD  = Lubeck.DV.Drawing.labeledAxis (axesNames !! 0) (axesNames !! 1)
  in mconcat [dataD, axesD, ticksD]
  where
    aes = mconcat aess
    boundsM     = aestheticBounds aes dat :: Map Key (Double, Double)
    guidesM2    = aestheticGuides aes dat :: Map Key [(Double, Str)]
    guidesM = applyScalingToGuides boundsM guidesM2
    -- scaleBaseNM = aestheticScaleBaseName aes dat :: Map Key Str
    mappedData2  = fmap (aestheticMapping aes dat) dat :: [Map Key Double]
    mappedAndScaledData = applyScalingToValues boundsM mappedData2

-- TODO
applyScalingToGuides :: Map Key (Double,Double)
  -> Map Key [(Double, str)]
  -> Map Key [(Double, str)]
applyScalingToGuides b m = Data.Map.mapWithKey (\aesK dsL -> fmap (\(d,s) -> (scale (fromMaybe idS $ Data.Map.lookup aesK b) d,s)) dsL) m
  where
    idS = (0,1)
    scale :: (Double, Double) -> Double -> Double
    scale (lb,ub) x = (x - lb) / (ub - lb)

applyScalingToValues :: Map Key (Double,Double) -> [Map Key Double] -> [Map Key Double]
applyScalingToValues b m = fmap (Data.Map.mapWithKey (\aesK d -> scale (fromMaybe idS $ Data.Map.lookup aesK b) d)) m
  where
    idS = (0,1)
    scale :: (Double, Double) -> Double -> Double
    scale (lb,ub) x = (x - lb) / (ub - lb)

newtype Geometry = Geometry { getGeometry :: [Map Key Double] -> Styled Drawing }
  deriving (Monoid)

(!) :: (Num b, Ord k) => Map k b -> k -> b
m ! k = maybe 0 id $ Data.Map.lookup k m

(?) :: (Monoid b, Ord k) => Map k b -> k -> b
m ? k = maybe mempty id $ Data.Map.lookup k m

(?!) :: (Ord k) => Map k b -> k -> Maybe b
m ?! k = Data.Map.lookup k m

-- TODO remove (~ Identity) restrictions
line :: Geometry
line = Geometry tot
  where
    -- All color values in the dataset or Nothing if there are none
    colors :: [Map Key Double] -> Maybe [Double]
    colors ms = case Data.Maybe.catMaybes $ fmap (?! "color") ms of
      [] -> Nothing
      xs -> Just $ sortNub xs

    atColor :: Double -> [Map Key Double] -> [Map Key Double]
    atColor c = filter (\m -> m ?! "color" == Just c)

    tot ms = case colors ms of
      Nothing -> baseL 0 ms
      Just xs -> mconcat $ fmap (\color -> baseL color $ atColor color ms) xs

    baseL :: Double -> [Map Key Double] -> Styled Drawing
    baseL _ ms = Lubeck.DV.Drawing.lineData $ fmap (\m -> P $ V2 (m ! "x") (m ! "y")) ms

fill :: Geometry
fill = Geometry tot
  where
    -- All color values in the dataset or Nothing if there are none
    colors :: [Map Key Double] -> Maybe [Double]
    colors ms = case Data.Maybe.catMaybes $ fmap (?! "color") ms of
      [] -> Nothing
      xs -> Just $ sortNub xs

    atColor :: Double -> [Map Key Double] -> [Map Key Double]
    atColor c = filter (\m -> m ?! "color" == Just c)

    tot ms = case colors ms of
      Nothing -> baseL 0 ms
      Just xs -> mconcat $ fmap (\color -> baseL color $ atColor color ms) xs

    baseL :: Double -> [Map Key Double] -> Styled Drawing
    baseL _ ms = Lubeck.DV.Drawing.fillData $ fmap (\m -> P $ V2 (m ! "x") (m ! "y")) ms

scatter :: Geometry
scatter = Geometry tot
  where
    -- All color values in the dataset or Nothing if there are none
    colors :: [Map Key Double] -> Maybe [Double]
    colors ms = case Data.Maybe.catMaybes $ fmap (?! "color") ms of
      [] -> Nothing
      xs -> Just $ sortNub xs

    atColor :: Double -> [Map Key Double] -> [Map Key Double]
    atColor c = filter (\m -> m ?! "color" == Just c)

    tot ms = case colors ms of
      Nothing -> baseL 0 ms
      Just xs -> mconcat $ fmap (\color -> baseL color $ atColor color ms) xs

    baseL :: Double -> [Map Key Double] -> Styled Drawing
    baseL _ ms = Lubeck.DV.Drawing.scatterData $ fmap (\m -> P $ V2 (m ! "x") (m ! "y")) ms

-- TODO move
sortNub = Data.List.nub . Data.List.sort


infixl 4 `withScale`
infixl 3 <~
infixl 3 ~>







-- TEST


data Gender = Female | Male deriving (Eq, Ord, Show)
newtype Name = Name String deriving (Eq, Ord, Show, IsString)

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
