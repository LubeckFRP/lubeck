
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
module Lubeck.DV.ColorPalette
  ( Palette
  , singleColor
  , singleColour
  , paletteFromList
  , getColorFromPalette
  , paletteToColor
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
Provides an infinite supply of colors. Conceptually a function from real numbers to colors.
These are used to specify colors in stylings.

Different palettes are appropriate for different plot types.

- If color is mapped to a category you can use 'paletteFromList'.

- For gradient plots and heat maps, use interploated colors (TODO).

- If you simply want to override the color of a particular plot element you can use 'singleColour'.
-}
newtype Palette a = Palette { getPalette :: Double -> AlphaColour a }

instance Show (Palette a) where
  -- TODO nicer
  show (Palette f) = "<<color palette>>"

{-|
Use the same color for everything. Not recommended if your
-}
singleColor :: AlphaColour a -> Palette a
singleColor = Palette . pure

{-|
Use the same color for everything. Not recommended if your
-}
singleColour :: AlphaColour a -> Palette a
singleColour = Palette . pure

{-|
Create a alette from a list (finite or infinite).
-}
paletteFromList :: [AlphaColour a] -> Palette a
paletteFromList xs = Palette $ \n -> cycle xs !! floor n

{-|
Extract color from the given palette.
-}
getColorFromPalette :: Palette a -> Double -> AlphaColour a
getColorFromPalette = getPalette

{-|
Default color for a given palette.
-}
paletteToColor :: Palette a -> AlphaColour a
paletteToColor p = getColorFromPalette p 1
