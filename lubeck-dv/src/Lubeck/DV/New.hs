
{-# LANGUAGE TemplateHaskell, RankNTypes, NoMonomorphismRestriction, MultiParamTypeClasses
  , FunctionalDependencies, TypeFamilies, GeneralizedNewtypeDeriving, OverloadedStrings
  , NoImplicitPrelude, TupleSections
  , FlexibleContexts
  #-}

module Lubeck.DV.New
  (
  -- * Algebra
    blendId
  , blend
  , crossWith
  -- * Scales
  , categorical, categoricalEnum, linear, logarithmic, timeScale
  , Scale(..)
  , HasScale(..)
  , Scaled(..)
  , withScale

  -- * Aesthetics
  , x, y, color, size, shape, thickness
  , Aes
  , defaultAes
  -- ** Mapping aesthetics
  , (<~)
  , (~>)
  -- * Top-level
  , visualizeTest
  )
where

import BasePrelude
import Data.Functor.Contravariant
import Control.Monad.Zip(mzipWith)
import Data.Map(Map)
import qualified Data.Map
import Control.Lens(Getter, to)
import Control.Lens.Operators hiding ((<~))
import Control.Lens.TH
import qualified Data.List
import Data.Time(UTCTime)
import Data.Proxy




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
-- TODO generic long zip! (Traversable, ZipList Applicative?)

-- | Concatenate rows left-to-right. If one row is shorter it is repeated.
crossWith :: (a -> b -> c) -> [a] -> [b] -> [c]
crossWith f a b = take (length a `max` length b) $ mzipWith f (cyc a) (cyc b)
  where
    cyc = cycle
    -- cyc xs = xs <> cyc xs



-- AESTHETICS

newtype Aes a = Aes
  { getAes ::
    ( [a] -> a -> Map String Double
    , [a] -> Map String (Double, Double)
    , [a] -> Map String [(Double, String)]
    , [a] -> Map String String -- name of scale used to plot the given aesthetic
    )
  }
  deriving (Monoid)

  -- TODO naming
runAes :: Aes a -> [a] -> a -> Map String Double
runAes (Aes (convert, _, _, _)) = convert

runAesBounds :: Aes t -> [t] -> Map String (Double, Double)
runAesBounds (Aes (_, bounds, _, _)) = bounds

runAesGuides :: Aes t -> [t] -> Map String [(Double, String)]
runAesGuides (Aes (_, _, ticks, _)) = ticks

runAesScaleBaseName :: Aes t -> [t] -> Map String String
runAesScaleBaseName (Aes (_, _, _, sbn)) = sbn

-- Anything that is scaled can be maed into an aesthetic.
defaultAes :: HasScale a => String -> Aes a
defaultAes n = Aes (convert, genBounds, genTicks, getScaleBaseName)
  where
    convert   = \vs v -> Data.Map.singleton n $ scaleMapping (scale v) vs v
    genBounds = \vs -> case vs of
      []    -> Data.Map.singleton n $ (0,0)
      (v:_) -> Data.Map.singleton n $ scaleBounds (scale v) vs
    genTicks  = \vs -> case vs of
      []    -> Data.Map.singleton n $ []
      (v:_) -> Data.Map.singleton n $ scaleTicks (scale v) vs
    getScaleBaseName = \vs -> case vs of
      []    -> Data.Map.singleton n $ []
      (v:_) -> Data.Map.singleton n $ scaleBaseName (scale v)


instance Contravariant Aes where
  contramap f (Aes (g, h, i, j))
    = Aes
      ( \xs x -> g (fmap f xs) (f x)
      , \xs   -> h (fmap f xs)
      , \xs   -> i (fmap f xs)
      , \xs   -> j (fmap f xs)
      )

x, y, color, size, shape, thickness :: HasScale a => Aes a
x         = defaultAes "x"
y         = defaultAes "y"
color     = defaultAes "color"
size      = defaultAes "size"
shape     = defaultAes "shape"
thickness = defaultAes "thickness"



-- SCALE

data Scale a = Scale
  { scaleMapping  :: [a] -> a -> Double
      -- ^ Given dataset @vs@, map single value @v@ into the real domain.
      --   You can construct a linear scale using @\_ x -> x@.
  , scaleBounds   :: [a] -> (Double, Double)
      -- ^ Given a data set, return @(min, max)@ values to visualize (assuming.
      --   the same mapping as 'scaleMapping').
      --
      --   If you don't want automatic rescaling, use a constant value.
      --
      --   It is fine to provide larger or smaller values than the actual bounds
      --   of the dataset, but note that values outside the bounds given here may
      --   not be visible. On the other hand, if the given bounds are too large
      --   the visualized data may not be intelligeble.
  , scaleTicks    :: [a] -> [(Double, String)]
      -- ^ Given a data set, return guide labels and positions (assuming.
      --   the same mapping as 'scaleMapping').
      --
      --   If you don't want any guides, just return 'mempty'.
  , scaleBaseName :: String
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
-- you are invoking 'scale' you need to pass an actual value. The reason for this
-- is that the 'Scaled' instance will use the scale defined in the value, so this
-- must be defined even though it is not otherwise used in the construction of
-- the scale.
--
-- >>> scaleTicks (scale (undefined :: Double)) []
-- []
-- >>> scaleTicks (scale (undefined :: Scaled Double)) []
-- *** Exception: Prelude.undefined
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
instance HasScale UTCTime where
  scale = const timeScale
-- instance Ord a => HasScale [a] where
  -- type S UTCTime = UTCTime
  -- scale = const categorical

-- | A utility for allowing users override the default scale type.
--
-- This works by wrapping up the value with a scale, and providing a 'HasScale'
-- instance that uses this scale, ignoring any other instances for 'a'.
--
-- Typically used with 'withScale' as follows:
--
-- @
-- [ x <~ name
-- , y <~ age `withScale` linear
-- ]
-- @
data Scaled a = Scaled
  { scaledValue :: a
  , scaledScale :: Scale a
  }
instance HasScale (Scaled a) where
  -- Here 'scaledScale' is used to extract the actual scale, which will be returned
  -- The 'contramap scaledValue' bit is to make sure the returned scale can handle
  -- its input by ignoring the passed scale (looking only at the value).
  scale = contramap scaledValue . scaledScale


-- TODO assure no duplicates
categorical :: (Ord a, Show a) => Scale a
categorical = Scale
  { scaleMapping = \vs v -> realToFrac $ succ $ findPlaceIn (sortNub vs) v
  , scaleBounds   = \vs -> (0, realToFrac $ length (sortNub vs) + 1)
  , scaleTicks    = \vs -> zipWith (\k v -> (realToFrac k, show v)) [1..] (sortNub vs)
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

-- TODO could be written without the asTypeOf using ScopedTypeVariables
categoricalEnum :: (Enum a, Bounded a, Show a) => Scale a
categoricalEnum = Scale
  { scaleMapping = \vs v -> realToFrac $ succ $ fromEnum v
  , scaleBounds   = \vs -> (0, realToFrac $ fromEnum (maxBound `asTypeOf` head vs) + 2)
  , scaleTicks    = \vs -> zipWith (\k v -> (k, show v)) [1..] [minBound..maxBound `asTypeOf` head vs]
  , scaleBaseName = "categoricalEnum"
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

linear :: (Real a, Show a) => Scale a
linear = Scale
  { scaleMapping = \vs v -> realToFrac v
  -- TODO resize LB to 0?
  , scaleBounds   = \vs   -> (realToFrac $ safeMin vs, realToFrac $ safeMax vs)
  -- TODO something nicer
  , scaleTicks    = \vs   -> fmap (\v -> (realToFrac v, show v)) vs
  , scaleBaseName = "linear"
  }
  where
    safeMin [] = 0
    safeMin xs = minimum xs
    safeMax [] = 0
    safeMax xs = maximum xs

logarithmic :: Floating a => Scale a
timeScale :: Scale UTCTime
[logarithmic, timeScale ] = undefined

withScale :: Getter s a -> Scale a -> Getter s (Scaled a)
withScale g s = to $ \x -> flip Scaled s $ x^.g


-- TOP-LEVEL

-- Very similar to (>$$<)
(<~) :: Aes a -> Getter s a -> Aes s
(<~) a g = contramap (^.g) a

-- Very similar to (>$<)
(~>) :: Getter s a -> Aes a -> Aes s
(~>) g a = contramap (^.g) a

visualizeTest' :: [s] -> [Aes s] -> [Map String Double] -- TODO result type
visualizeTest' dat aes = fmap ((runAes $ mconcat aes) dat) dat

visualizeTest :: [s] -> [Aes s] -> IO () -- TODO result type
visualizeTest dat aes = do
  putStrLn $ replicate 20 '-'

  putStrLn "Data to plot (column-order):"
  mapM_ print $ fmap ((runAes $ mconcat aes) dat) dat

  putStrLn "Scale base types (row-order):"
  mapM_ print $ ((runAesScaleBaseName $ mconcat aes) dat)

  putStrLn "Data bounds (row-order):"
  mapM_ print $ ((runAesBounds $ mconcat aes) dat)

  putStrLn "Guides to plot (row-order):"
  mapM_ print $ ((runAesGuides $ mconcat aes) dat)
  putStrLn $ replicate 20 '-'

infixl 4 `withScale`
infixl 3 <~
infixl 3 ~>

-- instance Monoid (Aes a) where
--   mempty = Aes2 mempty mempty mempty mempty
--   mappend (Aes2 a1 a2 a3 a4) (Aes2 b1 b2 b3 b4) = Aes2 (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)






-- TEST


data Gender = Female | Male deriving (Eq, Ord, Show)
newtype Name = Name String deriving (Eq, Ord, Show, IsString)

data Person = P { personName :: Name, personAge :: Int, personHeight :: Double }
  deriving (Eq, Ord, Show)

data Person2 = P2 { person2Name :: Name, person2Age :: Int, person2Height :: Double, person2Gender :: Gender }
  deriving (Eq, Ord, Show)

$(makeFields ''Person)
$(makeFields ''Person2)

males, females :: [Person]
males =
  [ P (Name "Hans") 28 1.15
  , P (Name "Sven") 25 1.15
  , P (Name "Sven") 25 1.15
  , P (Name "Sven") 25 1.15
  , P (Name "Sven") 25 1.15
  , P (Name "Sven") 25 1.15
  ]
females =
  [ P "Elin" 21 1.15
  , P "Alva" 19 1.15
  ]

people :: [Person2]
people = (males `cr` [Male]) <> (females `cr` [Female])
  where
    cr = crossWith (\p gender -> P2 (p^.name) (p^.age) (p^.height) gender)

test = mapM_ print people >> visualizeTest people
  [ mempty
  , color <~ height
  , shape <~ gender
  , x     <~ name
  , y     <~ age `withScale` linear
  ]
test2 = visualizeTest ([(1,2), (3,4)] :: [(Int, Int)])
  [ mempty
  , x <~ to fst
  , y <~ to snd
  ]
test3 = visualizeTest ( [ ] :: [(UTCTime, Int)])
  [ mempty
  , x <~ to fst
  , y <~ to snd
  ]

test4 = visualizeTest ("hello world" :: String) [x <~ id]

data WD = Mon | Tues | Wed | Thurs | Fri | Sat | Sun deriving (Eq, Ord, Show, Enum, Bounded)

instance HasScale WD where
  scale = const categoricalEnum

test5 = visualizeTest [(Mon, 100 :: Int), (Sun, 400)] [x <~ to fst, y <~ to snd]
