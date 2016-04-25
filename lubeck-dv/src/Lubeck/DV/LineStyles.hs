
{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
  , QuasiQuotes
  , OverloadedStrings
  , TupleSections
  , TemplateHaskell
  , ConstraintKinds
  #-}

{-|
Specifies color palettes.

TODO
  Use the palette package to generate colors (this type is for keeping track of them).
-}
module Lubeck.DV.LineStyles
  ( LineStyles
  , defaultLineStyles
  , lineStylesFromList
  , extractLineStyle
  , lineStyleFromLineStyles
  ) where

import Prelude hiding (div)
import qualified Prelude

import Control.Applicative
import Control.Lens (to)
import Control.Lens.Operators
import Control.Lens.TH (makeLenses)
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Colour (Colour, AlphaColour, withOpacity, blend, alphaChannel)
import Data.Monoid
import Data.Map(Map)
import qualified Data.Colour.Names as Colors

import Linear.Vector
import Linear.Affine
import Linear.V0
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4


{-|
A mapping from real numbers to line styles.
-}
newtype LineStyles = LineStyles { getLineStyles :: Double -> [Double] }
  deriving (Monoid)

instance Show LineStyles where
  -- TODO nicer
  show _ = "<<line styles>>"

{-|
Create a alette from a list (finite or infinite).
-}
lineStylesFromList :: [[Double]] -> LineStyles
lineStylesFromList xs = LineStyles $ \n -> cycle xs !! floor n

defaultLineStyles :: LineStyles
defaultLineStyles = lineStylesFromList
  [ mempty -- solid
  -- , [2]
  -- , [1, 2]
  , [4]
  , [2, 2, 4]
  , [8]
  , [1, 8]
  , [4, 2]
  , [4, 8]
  ]

{-|
Extract line style from the given line style set.
-}
extractLineStyle :: LineStyles -> Double -> [Double]
extractLineStyle = getLineStyles

{-|
Default line styles for a given line style set.
-}
lineStyleFromLineStyles :: LineStyles -> [Double]
lineStyleFromLineStyles p = extractLineStyle p 1
