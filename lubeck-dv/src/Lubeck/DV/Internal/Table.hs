
{-# LANGUAGE
    RankNTypes
  , NoMonomorphismRestriction
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  , TupleSections
  , FlexibleContexts
  , DeriveFoldable
  , DeriveTraversable
  , StandaloneDeriving
  , NoImplicitPrelude
  , MultiParamTypeClasses
  , DeriveFunctor
  , CPP
  #-}

module Lubeck.DV.Internal.Table
  (
  -- ** Table type
    Table
  , tableNull
  , tableSingleton
  , tableFromList
  , tableToMap
  , tableToList
  , tableSize
  , overlayTablesLong
  , overlayTablesShort
  , crossTablesLong
  , crossTablesShort
  , conjoin2With
  , conjoin2L
  , filterRows
  , getColumn
  -- ** Column type
  , Column
  , columnFromList
  , runColumn
  , runColumnZ
  , runColumnFinite
  , runColumnFiniteZ
  -- ** Utility
  , prettyTable
  , printTable
  )
where

import BasePrelude
import Control.Lens(Getter, to, toListOf)
import Control.Lens.Operators hiding ((<~))
import Control.Monad.Reader (ask)
import Data.Functor.Contravariant (Contravariant(..))
import Data.Map(Map)
import Data.Set(Set)
import Data.IntMap(IntMap)
import Data.IntSet(IntSet)

import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Functor.Compose
import qualified Data.Char
import qualified Data.List
import qualified Data.Map
import qualified Data.Map as Map
import qualified Data.Set
import qualified Data.IntMap
import qualified Data.IntSet
import qualified Data.Maybe
import qualified Data.Time
import qualified Data.Time.Format
import qualified Text.PrettyPrint.Boxes as B
import qualified Data.Colour.Names as Colors

import Data.Functor.Classes(Eq1, Ord1, Show1)
import Lubeck.DV.Internal.ZipListMonad

{-
Assuming (Ord k) =>

[a]     ~ (Int, Int -> a)
Map k v ~ ([k], k -> Maybe v)
Map k v ~ (Int, Int -> k, Int -> v)  ~  (Int, Int -> (k, v))  ~ [(k, v)]


[Map k v]
  ~ (Int, Int -> Map k v)
  ~ (Int, Int -> ([k], k -> Maybe v))


INTERESTINGLY
  [Map k a]
    ~  Map k [Maybe a]
    ~  ([k], Int, k -> Int -> Maybe v)

-}

-- TODO orphans

#if MIN_VERSION_linear(1,20,0)
#else
deriving instance Foldable ZipList
deriving instance Traversable ZipList
#endif
deriving instance Eq1 ZipList
deriving instance Ord1 ZipList
deriving instance Show1 ZipList


{-|
Single column of a 'Table'. Isomorphic to [Maybe a].
-}
newtype Column a = Column { getColumn_ :: MaybeT ZipList a }
  deriving (Functor, Applicative, Monad,
   Foldable, Traversable,
   Alternative, Eq, Ord, Show
   )


-- instance Alternative Column where
--   empty = Column (MaybeT (ZipList empty))
--   (Column (MaybeT (ZipList a))) <|> (Column (MaybeT (ZipList b))) = Column (MaybeT (ZipList $ a <|> b))
--
-- instance MonadPlus Column where
--   mzero = empty
--   mplus = (<|>)

-- TODO good Alternative/MonadPlus?


columnFromList :: [Maybe a] -> Column a
columnFromList xs = Column $ MaybeT $ ZipList xs

runColumn :: Column a -> [Maybe a]
runColumn = getZipList . runMaybeT . getColumn_

runColumnZ :: Column a -> ZipList (Maybe a)
runColumnZ = runMaybeT . getColumn_


{-|
The top-most non-Nothing values of a column.
-}
runColumnFinite :: Column a -> [a]
runColumnFinite = fmap fromJust . takeWhile isJust . runColumn
  where
    isJust (Just _) = True
    isJust _        = False
    fromJust (Just x) = x

runColumnFiniteZ :: Column a -> ZipList a
runColumnFiniteZ = ZipList . runColumnFinite

{-
Table type used to represent data internally in Lubeck DV.

You can think of it as a spreadsheet where columns have names, rows have
numbers indexed from 0, and cells might be empty.

See tableToMap, tableToList.
-}
newtype Table k a = Table [Map k a]
  deriving (Functor, Monoid, Show)

instance (Ord k, Eq a) => Eq (Table k a) where
  a == b  =  tableToMap a == tableToMap b


tableNull :: Table k a -> Bool
tableNull (Table []) = True
tableNull _          = False

{-
Create a table with a single row and column.
-}
tableSingleton :: k -> a -> Table k a
tableSingleton k v = Table $ pure $ Data.Map.singleton k v

{-|
Create a table from a list.

@
tableToList . tableFromList = id
tableFromList . tableToList = id
@
-}
tableFromList :: Ord k => [Map k a] -> Table k a
tableFromList = Table

{-|
Returns the names of each column.
-}
tableHeaders :: Ord k => Table k a -> [k]
tableHeaders (Table t) = ks
  where
    ks = Data.List.nub $ mconcat $ fmap Data.Map.keys t

{-|
Returns values in column-major order.
-}
tableValues :: Ord k => Table k a -> [[Maybe a]]
tableValues = toList . tableToMap
  where
    toList = toListOf traverse

{-|
The number of rows.
-}
tableSize :: Ord k => Table k a -> Int
tableSize (Table t) = length t

{-|
View the table as a list of key-value mappings.
-}
tableToList :: Ord k => Table k a -> [Map k a]
tableToList (Table t) = t

{-|
View the table as a map from column name to values.
-}
tableToMap :: Ord k => Table k a -> Map k [Maybe a]
tableToMap (Table t) = Data.Map.fromList $ fmap (\k -> (k, fmap (Data.Map.lookup k) t)) ks
  where
    -- ks :: [[k]]
    ks = Data.List.nub $ mconcat $ fmap Data.Map.keys t

tableToMap' :: Ord k => Table k a -> Map k (Column a)
tableToMap' = fmap columnFromList . tableToMap

getColumn :: Ord k => k -> Table k a -> Column a
getColumn k t = case Data.Map.lookup k (tableToMap' t) of
  Nothing -> empty
  Just c -> c


{-
Filter a row by looking at a single key.
-}
filterRows :: Ord k => k -> (a -> Bool) -> Table k a -> Table k a
filterRows k p (Table t) = Table $ filter (\m -> justJust p $ Data.Map.lookup k m) t
  where
    justJust p (Just x) = p x
    justJust p Nothing  = False

{-
  short/long zips
  cross retains data present in only one tableToMap
  overlay throws a way data not present in both

Laws
  tableSize (crossTablesShort f a b)   === min (tableSize a) (tableSize b)
  tableSize (overlayTablesShort f a b) === min (tableSize a) (tableSize b)
  tableSize (crossTablesLong f a b)   === max (tableSize a) (tableSize b)
    a /= mempty, b /= mempty
  tableSize (overlayTablesLong f a b) === max (tableSize a) (tableSize b)
    a /= mempty, b /= mempty
-}

crossTablesShort :: Ord k => (a -> a -> a) -> Table k a -> Table k a -> Table k a
crossTablesShort f (Table a) (Table b) = Table $ zipWith (Data.Map.unionWith f) a b

crossTablesLong :: Ord k => (a -> a -> a) -> Table k a -> Table k a -> Table k a
crossTablesLong f (Table a) (Table b) = Table $ crossWith (Data.Map.unionWith f) a b
  where
    -- TODO consolidate w New module
    crossWith f [] b  = []
    crossWith f a  [] = []
    crossWith f a  b  = take (length a `max` length b) $ zipWith f (cycle a) (cycle b)

-- Note: This would form an applicative with a suitable pure

overlayTablesShort :: Ord k => (a -> b -> c) -> Table k a -> Table k b -> Table k c
overlayTablesShort f (Table a) (Table b) = Table $ zipWith (combine2With f) a b

overlayTablesLong :: Ord k => (a -> b -> c) -> Table k a -> Table k b -> Table k c
overlayTablesLong f (Table a) (Table b) = Table $ crossWith (combine2With f) a b
  where
    -- TODO consolidate w New module
    crossWith f [] b  = []
    crossWith f a  [] = []
    crossWith f a  b  = take (length a `max` length b) $ zipWith f (cycle a) (cycle b)

combine2 :: Ord k => Map k a -> Map k b -> Map k (a, b)
combine2 = combine2With (,)

combine2With :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
combine2With f a b = Data.Map.fromList $ Data.Maybe.catMaybes $ fmap (\k -> safeLookUpWithKey a b k) ks
  where
    -- safeLookUpWithKey :: Ord k => Map k a -> Map k b -> k -> Maybe (k, (a, b))
    safeLookUpWithKey as bs k = case (Data.Map.lookup k as, Data.Map.lookup k bs) of
      (Just a, Just b) -> Just (k, f a b)
      _ -> Nothing
    ks = sortNub $ Map.keys a <> Map.keys b
    sortNub = Data.List.nub . Data.List.sort




conjoin2With :: Ord k => (a -> c) -> (b -> c) -> (a -> b -> c) -> Map k a -> Map k b -> Map k c
conjoin2With f g h a b = Data.Map.fromList $ Data.Maybe.catMaybes $ fmap (\k -> safeLookUpWithKey a b k) ks
  where
    -- safeLookUpWithKey :: Ord k => Map k a -> Map k b -> k -> Maybe (k, (a, b))
    safeLookUpWithKey as bs k = case (Data.Map.lookup k as, Data.Map.lookup k bs) of
      (Just a, Just b) -> Just (k, h a b)
      (Just a, _)      -> Just (k, f a)
      (_,      Just b) -> Just (k, g b)
      _                -> Nothing
    ks = sortNub $ Map.keys a <> Map.keys b
    sortNub = Data.List.nub . Data.List.sort





{-
TODO this is used in DV.New
Needs a more general form of combine
-}

conjoin2L :: Ord k => Table k a -> Table k b -> Table k (a, Maybe b)
conjoin2L (Table x) (Table y) = tableFromList $ zipWith mergeMapsL x y
  where
    mergeMapsL x y = mconcat $ fmap g (Map.keys x)
      where
        g k = case (Map.lookup k x, Map.lookup k y) of
          (Nothing, _)       -> error "mergeMapsL: Impossible as key set is drawn from first map"
          (Just xv, Nothing) -> Map.singleton k (xv, Nothing)
          (Just xv, Just yv) -> Map.singleton k (xv, Just yv)


{-|
Pretty-print a table.
-}
prettyTable :: (Ord k, Show k, Show a) => Table k a -> B.Box
prettyTable t = makeTable (fmap toBox $ tableHeaders t) (fmap (fmap toBox) $ transpose $ tableValues t)
  where
    toBox :: Show a => a -> B.Box
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

printTable :: (Ord k, Show k, Show a) => Table k a -> IO ()
printTable x = putStrLn  $ B.render $ prettyTable x
