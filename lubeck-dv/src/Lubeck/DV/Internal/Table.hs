
{-# LANGUAGE
    RankNTypes
  , NoMonomorphismRestriction
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  , TupleSections
  , FlexibleContexts
  , NoImplicitPrelude
  , MultiParamTypeClasses
  , DeriveFunctor
  #-}

module Lubeck.DV.Internal.Table
  -- ( Table
  -- , tableFromList
  -- , tableToMap
  -- , tableToList
  -- , overlayTablesLong
  -- , overlayTablesShort
  -- )
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

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Functor.Compose
import qualified Data.Char
import qualified Data.List
import qualified Data.Map
import qualified Data.Set
import qualified Data.IntMap
import qualified Data.IntSet
import qualified Data.Maybe
import qualified Data.Time
import qualified Data.Time.Format
import qualified Text.PrettyPrint.Boxes as B
import qualified Data.Colour.Names as Colors


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


newtype Table k a = Table [Map k a]
  deriving (Functor, Monoid, Show)

instance (Ord k, Eq a) => Eq (Table k a) where
  a == b  =  tableToMap a == tableToMap b

tableFromList :: Ord k => [Map k a] -> Table k a
tableFromList = Table

tableSingleton :: k -> a -> Table k a
tableSingleton k v = Table $ pure $ Data.Map.singleton k v

tableToList :: Ord k => Table k a -> [Map k a]
tableToList (Table t) = t

{-
Returns the header of each column.
-}
tableHeaders :: Ord k => Table k a -> [k]
tableHeaders (Table t) = ks
  where
    ks = Data.List.nub $ mconcat $ fmap Data.Map.keys t

{-
Returns values in column-major order.
-}
tableValues :: Ord k => Table k a -> [[Maybe a]]
tableValues = toList . tableToMap
  where
    toList = toListOf traverse

tableSize :: Ord k => Table k a -> Int
tableSize (Table t) = length t

tableToMap :: Ord k => Table k a -> Map k [Maybe a]
tableToMap (Table t) = Data.Map.fromList $ fmap (\k -> (k, fmap (Data.Map.lookup k) t)) ks
  where
    -- ks :: [[k]]
    ks = Data.List.nub $ mconcat $ fmap Data.Map.keys t

{-
  short/long zips
  cross retains data present in only one tableToMap
  overlay throws a way data not present in both

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

overlayTablesShort :: Ord k => (a -> b -> c) -> Table k a -> Table k b -> Table k c
overlayTablesShort f (Table a) (Table b) = Table $ zipWith (combine2With f) a b

overlayTablesLong :: Ord k => (a -> b -> c) -> Table k a -> Table k b -> Table k c
overlayTablesLong f (Table a) (Table b) = Table $ crossWith (combine2With f) a b
  where
    -- TODO consolidate w New module
    crossWith f [] b  = []
    crossWith f a  [] = []
    crossWith f a  b  = take (length a `max` length b) $ zipWith f (cycle a) (cycle b)


{-
Filter a row by looking at a single key.
-}
filterRows :: Ord k => k -> (a -> Bool) -> Table k a -> Table k a
filterRows k p (Table t) = Table $ filter (\m -> justJust p $ Data.Map.lookup k m) t
  where
    justJust p (Just x) = p x
    justJust p Nothing  = False



-- Internal

combine2 :: Ord k => Map k a -> Map k b -> Map k (a, b)
combine2 a b = Data.Map.fromList $ Data.Maybe.catMaybes $ fmap (\k -> safeLookUpWithKey a b k) ks
  where
    -- safeLookUpWithKey :: Ord k => Map k a -> Map k b -> k -> Maybe (k, (a, b))
    safeLookUpWithKey as bs k = case (Data.Map.lookup k as, Data.Map.lookup k bs) of
      (Just a, Just b) -> Just (k, (a, b))
      _ -> Nothing
    ks = sortNub $ ksA <> ksB
    (ksA, ksB) = (Data.Map.keys a, Data.Map.keys b)
    sortNub = Data.List.nub . Data.List.sort

combine2With :: Ord k => (a -> b1 -> b) -> Map k a -> Map k b1 -> Map k b
combine2With f a b = fmap (uncurry f) $ combine2 a b



-- Show/Print

printTable :: (Ord k, Show k, Show a) => Table k a -> IO ()
printTable x = putStrLn  $ B.render $ showTable x

showTable :: (Ord k, Show k, Show a) => Table k a -> B.Box
showTable t = makeTable (fmap toBox $ tableHeaders t) (fmap (fmap toBox) $ transpose $ tableValues t)
  where
    -- plot@(SinglePlot mappedData _ boundsM guidesM _ _) = createSinglePlot [] dat aess mempty
    -- aKeys       = Data.Map.keys $ mconcat mappedData
    -- scaleBaseNM = aestheticScaleBaseName (mconcat aess) dat :: Map Key Str
    --
    -- {-
    --   Create some basic text-based table views for he
    --   Based on the 'boxes' package.
    -- -}
    -- rawDataTable = B.vcat B.left $ map (toBox) dat
    -- mappedDataTable = makeTable (fmap (toBox) $ aKeys)
    --   (fmap (\aesMap -> fmap (\k -> maybe "" toBox $ Data.Map.lookup k aesMap) aKeys) mappedData)
    -- -- tab1a = makeTable (fmap (toBox) $ aKeys)
    --   -- (fmap (\aesMap -> fmap (\k -> maybe "" toBox $ Data.Map.lookup k aesMap) aKeys) specialData)
    -- scaledDataTable = makeTable (fmap (toBox) $ aKeys)
    --   (fmap (\aesMap -> fmap (\k -> maybe "" toBox $ Data.Map.lookup k aesMap) aKeys) (mappedAndScaledDataWithSpecial Nothing plot))
    -- aestheticTableTable = makeTable ["Aesthetic", "Scale base", "PlotBounds", "Guide"]
    --   (fmap (\k ->
    --     [ toBox k
    --     , B.text $ maybe "" show $ Data.Map.lookup k scaleBaseNM
    --     , B.text $ maybe "" show $ Data.Map.lookup k boundsM
    --     , B.text $ maybe "" show $ Data.Map.lookup k guidesM
    --     ]) $ Data.Map.keys guidesM)
    --
    -- box = B.vsep 1 B.left
    --   [ "Raw data    " B.<+> rawDataTable
    --   , "Mapped data " B.<+> mappedDataTable -- TODO show "special" data here too!
    --   , "Scaled data " B.<+> scaledDataTable
    --   , "Aesthetics  " B.<+> aestheticTableTable
    --   ]

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
