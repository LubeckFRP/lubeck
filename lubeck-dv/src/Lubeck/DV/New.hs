
{-# LANGUAGE TemplateHaskell, RankNTypes, NoMonomorphismRestriction, MultiParamTypeClasses
  , FunctionalDependencies, TypeFamilies, GeneralizedNewtypeDeriving, OverloadedStrings
  , NoImplicitPrelude, TupleSections
  , FlexibleContexts
  #-}

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
  , color <~ gender
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


--
-- logarithmic :: Real a => Scale a
-- (y := k)               :: (HasStyle a, Has k s a) => Aesthetic s
-- (y := k `withStyle` s) :: Has k s a => Aesthetic s
-- visualize' :: s -> [Aesthetic s] -> Drawing




-- Algebra:

idBlend   :: [a]
blend     :: [a] -> [a] -> [a]
-- TODO some type class thing here for merging tuples/records
-- so i.e.
--  cross (1,2) (3,4) => (1,2,3,4)
--  cross {foo=1, bar=""} {baz=()} => {foo=1, bar="", baz=()}
crossWith :: (a -> b -> c) -> [a] -> [b] -> [c]
idBlend   = []
blend     = (<>)
-- TODO generic long zip! (Traversable, ZipList Applicative?)
crossWith f a b = take (length a `max` length b) $ mzipWith f (cyc a) (cyc b)
  where
    cyc = cycle
    -- cyc xs = xs <> cyc xs

-- Scale/Aesthetics:

-- data Aes a
newtype Aes a = Aes
  { getAes :: [a] -> a -> Map String Double
  }
  deriving (Monoid)

instance Contravariant Aes where
  contramap f (Aes g) = Aes (\xs x -> g (fmap f xs) (f x))

data Scale a = Scale
  { runScale :: [a] -> a -> Double
  , bounds   :: [a] -> (Double, Double)
  , ticks    :: [a] -> [(Double, String)]
  }

instance Contravariant Scale where
  contramap f (Scale r b t) = Scale (\vs v -> r (fmap f vs) (f v)) (b . fmap f) (t . fmap f)

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
data Scaled a = Scaled
  { scaledValue :: a
  , scaledScale :: Scale a
  }
instance HasScale (Scaled a) where -- ignore eventual a instance
  scale = contramap scaledValue . scaledScale


x, y, color, size, shape, thickness :: HasScale a => Aes a
x = Aes $ \vs v -> Data.Map.singleton "x" $ runScale (scale v) vs v
y = Aes $ \vs v -> Data.Map.singleton "y" $ runScale (scale v) vs v
color = Aes $ \vs v -> Data.Map.singleton "color" $ runScale (scale v) vs v
[size, shape, thickness] = undefined

-- TODO assure no duplicates
categorical :: (Ord a, Show a) => Scale a
categorical = Scale
  { runScale = \vs v -> realToFrac $ succ $ findPlaceIn vs v
  , bounds   = \vs -> (0, realToFrac $ length vs + 1)
  , ticks    = \vs -> zipWith (\k v -> (realToFrac k, show v)) [1..] vs
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
    findPlaceIn xs x = length $ takeWhile (< x) $ Data.List.nub $ Data.List.sort xs

linear :: (Real a, Show a) => Scale a
linear = Scale
  { runScale = \vs v -> realToFrac v
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

visualize' :: [s] -> [Aes s] -> [Map String Double] -- TODO result type
visualize' dat aes = fmap ((getAes $ mconcat aes) dat) dat

visualize :: [s] -> [Aes s] -> IO () -- TODO result type
visualize dat aes = mapM_ print $ visualize' dat aes

infixl 4 `withScale`
infixl 3 <~
infixl 3 ~>
