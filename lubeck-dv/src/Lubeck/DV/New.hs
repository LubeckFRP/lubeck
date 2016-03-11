
{-# LANGUAGE TemplateHaskell, RankNTypes, NoMonomorphismRestriction, MultiParamTypeClasses
  , FunctionalDependencies, TypeFamilies, GeneralizedNewtypeDeriving, OverloadedStrings
  , NoImplicitPrelude
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

test = visualize people
  [
    x     <~ name
  , y     <~ age `withScale` categorical
  -- , color <~ gender
  , color <~ height
  ]
test2 = visualize ([(1,2), (3,4)] :: [(Int, Double)])
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
  { getAes :: a -> Map String Double
  }
  deriving (Monoid)
instance Contravariant Aes where
  contramap f (Aes g) = Aes (g . f)
data Scale a = Scale
  { runScale :: a -> Double
  , bounds :: [a] -> (Double, Double)
  , ticks :: [a] -> (Double, String)
  }

class HasScale a where
  scale :: a -> Scale a
instance HasScale Double where
  scale = const linear
instance HasScale Int where
  scale = const categorical
-- instance HasScale Char where
--   scale = const categorical
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
  , scaledScale :: Scale (Scaled a)
  }
instance HasScale (Scaled a) where -- ignore eventual a instance
  scale = scaledScale


x, y, color, size, shape, thickness :: (HasScale a) => Aes a
x = Aes $ \v -> Data.Map.singleton "x" $ runScale (scale v) v
y = Aes $ \v -> Data.Map.singleton "y" $ runScale (scale v) v
color = Aes $ \v -> Data.Map.singleton "color" $ runScale (scale v) v

categorical :: (Ord a) => Scale a
-- categorical = Scale
  -- { runScale
  --
  -- }

linear :: Real a => Scale a
logarithmic :: Floating a => Scale a
timeScale :: Scale UTCTime

withScale :: Getter s a -> Scale a -> Getter s (Scaled a)
withScale g s = to $ \x -> aToScaled s $ x^.g
  where
    aToScaled s x = Scaled x (unScaled s)
    unScaled (Scale f g h) = Scale (f . scaledValue) (g . fmap scaledValue) (h . fmap scaledValue)


(~>) :: (HasScale a) => Getter s a -> Aes a -> Aes s
(<~) = flip (~>)
(~>) g = contramap (^.g)

visualize :: [s] -> [Aes s] -> [Map String Double] -- TODO result type
visualize dat aes = fmap (getAes $ mconcat aes) dat

infixl 4 `withScale`
infixl 3 <~
infixl 3 ~>

[size, shape, thickness,
  categorical, linear, logarithmic, timeScale ] = undefined
