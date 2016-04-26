
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
  , TemplateHaskell
  #-}

import Lubeck.DV.Internal.Table

import BasePrelude
import Test.QuickCheck
import qualified Data.Map


instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Table k a) where
  arbitrary = fmap (tableFromList . fmap Data.Map.fromList) arbitrary

prop_tableEmptyL a1  =  (a <> mempty) === a
  where
    a = a1 :: Table Int Int
prop_tableEmptyR a1  =  (mempty <> a) === a
  where
    a = a1 :: Table Int Int

prop_tableAssoc a1 b1 c1  = ((a <> b) <> c) === (a <> (b <> c))
  where
    a = a1 :: Table Int Int
    b = b1
    c = c1



prop_tableCrossLenghS a b = tableSize (crossTablesShort const a b) === min (tableSize a) (tableSize b)
  where
    _ = a :: Table Int Int

prop_tableCrossLenghL a b = a === mempty  .||. b === mempty .||. tableSize (crossTablesLong const a b) === max (tableSize a) (tableSize b)
  where
    _ = a :: Table Int Int

prop_tableOverlayLenghS a b = tableSize (overlayTablesShort const a b) === min (tableSize a) (tableSize b)
  where
    _ = a :: Table Int Int

prop_tableOverlayLenghL a b = a === mempty  .||. b === mempty .||. tableSize (overlayTablesLong const a b) === max (tableSize a) (tableSize b)
  where
    _ = a :: Table Int Int

prop_fromListToListIso a = tableFromList (tableToList a) === a
  where
    _ = a :: Table Int Int

prop_fooBar =
  ( ("foo" -: 1 <> "foo" -: 2) `crLL` ("bar" -: 3) ) === ( ("bar" -: 3) `crLL` ("foo" -: 1 <> "foo" -: 2) )




(-:) = tableSingleton
infixl 7 -:

crLL = crossTablesLong const
crSL = crossTablesShort const


test :: Table String Int
test = overlayTablesShort (+) foobar (mconcat $ replicate 10 (("foo" -: 0) `crLL` ("bar" -: 0)))
  where
    foobar = mconcat $ take 4 $ cycle [
      (   ("foo" -: 1)
       <> ("foo" -: 2)) `crLL` ("bar" -: 3)
      ,                        "bar" -: 4
      ]



-- For the strange "return []" line, see http://stackoverflow.com/questions/28358575/quickcheckall-always-return-true
return []
main = $quickCheckAll
