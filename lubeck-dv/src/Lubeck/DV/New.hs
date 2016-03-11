
{-# LANGUAGE TemplateHaskell, RankNTypes, NoMonomorphismRestriction, MultiParamTypeClasses
  , FunctionalDependencies, TypeFamilies, GeneralizedNewtypeDeriving, OverloadedStrings
  , NoImplicitPrelude, TupleSections
  , FlexibleContexts
  #-}

module Lubeck.DV.New
-- ()
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

test = mapM_ print people >> visualize people
  [
    x     <~ name
  , y     <~ age `withScale` linear
  , color <~ height
  , shape <~ gender

  ]
test2 = visualize ([(1,2), (3,4)] :: [(Int, Int)])
  [
    x <~ to fst
  , y <~ to snd
  ]
test3 = visualize ( [ ] :: [(UTCTime, Int)])
  [
    x <~ to fst
  , y <~ to snd
  ]

test4 = visualize ("hello world" :: String) [x <~ id]


--
-- logarithmic :: Real a => Scale a
-- (y := k)               :: (HasStyle a, Has k s a) => Aesthetic s
-- (y := k `withStyle` s) :: Has k s a => Aesthetic s
-- visualize' :: s -> [Aesthetic s] -> Drawing




-- Algebra:

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



-- Scale/Aesthetics:

-- TODO pass along ticks/bounds in aesthetic

-- data Aes a
newtype Aes a = Aes
  { getAes ::
    ( [a] -> a -> Map String Double
    , [a] -> Map String (Double, Double)
    , [a] -> Map String [(Double, String)]
    )
  }
  deriving (Monoid)

runAes :: Aes a -> [a] -> a -> Map String Double
runAes (Aes (convert, _, _)) = convert
-- TODO naming
runAesT (Aes (_, ticks, _)) = ticks
runAesB (Aes (_, _, bounds)) = bounds

-- Anything that is scaled can be maed into an aesthetic.
defaultAes :: HasScale a => String -> Aes a
defaultAes n = Aes (convert, genBounds, genTicks)
  where
    convert   = \vs v -> Data.Map.singleton n $ runScale (scale v) vs v
    genBounds = \vs -> case vs of
      []    -> Data.Map.singleton n $ (0,0)
      (v:_) -> Data.Map.singleton n $ bounds (scale v) vs
    genTicks  = \vs -> case vs of
      []    -> Data.Map.singleton n $ []
      (v:_) -> Data.Map.singleton n $ ticks (scale v) vs

instance Contravariant Aes where
  contramap f (Aes (g, h, i))
    = Aes
      ( \xs x -> g (fmap f xs) (f x)
      , \xs   -> h (fmap f xs)
      , \xs   -> i (fmap f xs)
      )

data Scale a = Scale
  { runScale :: [a] -> a -> Double
  , bounds   :: [a] -> (Double, Double)
  , ticks    :: [a] -> [(Double, String)]
  }

instance Contravariant Scale where
  contramap f (Scale r b t) = Scale (\vs v -> r (fmap f vs) (f v)) (b . fmap f) (t . fmap f)

-- | Types with a default scale.
--
-- If you're writing an instance you can safely ignore the @a@ argument, however if
-- you are invoking 'scale' you need to pass an actual value. The reason for this
-- is that the 'Scaled' instance will use the scale defined in the value, so this
-- must be defined even though it is not otherwise used in the construction of
-- the scale.
--
-- >>> ticks (scale (undefined :: Double)) []
-- []
-- >>> ticks (scale (undefined :: Scaled Double)) []
-- *** Exception: Prelude.undefined
--
class HasScale a where
  scale :: a -> Scale a
instance HasScale Double where
  scale = const linear
instance HasScale Int where
  scale = const linear
instance HasScale Char where
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



x, y, color, size, shape, thickness :: HasScale a => Aes a
x = defaultAes "x"
y = defaultAes "y"
color = defaultAes "color"
size = defaultAes "size"
shape = defaultAes "shape"
thickness = defaultAes "thickness"

-- TODO assure no duplicates
categorical :: (Ord a, Show a) => Scale a
categorical = Scale
  { runScale = \vs v -> realToFrac $ succ $ findPlaceIn (sortNub vs) v
  , bounds   = \vs -> (0, realToFrac $ length (sortNub vs) + 1)
  , ticks    = \vs -> zipWith (\k v -> (realToFrac k, show v)) [1..] (sortNub vs)
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
  { runScale = \vs v -> realToFrac v
  -- TODO resize LB to 0?
  , bounds   = \vs   -> (realToFrac $ safeMin vs, realToFrac $ safeMax vs)
  -- TODO something nicer
  , ticks    = \vs   -> fmap (\v -> (realToFrac v, show v)) vs
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

-- Very similar to (>$$<)
(<~) :: Aes a -> Getter s a -> Aes s
(<~) a g = contramap (^.g) a

-- Very similar to (>$<)
(~>) :: Getter s a -> Aes a -> Aes s
(~>) g a = contramap (^.g) a

-- >>> visualize "hello world" [x <~ id]
visualize' :: [s] -> [Aes s] -> [Map String Double] -- TODO result type
visualize' dat aes = fmap ((runAes $ mconcat aes) dat) dat

visualize :: [s] -> [Aes s] -> IO () -- TODO result type
visualize dat aes = do
  mapM_ print $ fmap ((runAes  $ mconcat aes) dat) dat
  mapM_ print $ ((runAesT $ mconcat aes) dat)
  mapM_ print $ ((runAesB $ mconcat aes) dat)

infixl 4 `withScale`
infixl 3 <~
infixl 3 ~>
