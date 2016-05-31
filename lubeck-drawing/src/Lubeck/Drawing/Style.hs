
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, TypeFamilies, OverloadedStrings,
  NamedFieldPuns, CPP, NoMonomorphismRestriction, BangPatterns, StandaloneDeriving
  , ScopedTypeVariables #-}

module Lubeck.Drawing.Style
  ( Style
  , styleNamed
  , styleToAttrString
  )
where

import Control.Applicative
import Data.Colour (Colour, AlphaColour, withOpacity)
import Data.Map.Strict(Map)
import Data.Monoid
import Data.Semigroup(Max(..))
import qualified Data.Colour
import qualified Data.Colour.Names as Colors
import qualified Data.Colour.SRGB
import qualified Data.List
import qualified Data.Ord
import qualified Data.Map.Strict as Map
import qualified Data.String
import qualified Data.Sequence as Seq
import Data.Sequence(Seq)
import Data.Foldable(toList)

import Linear.Vector
import Linear.Affine
import Linear.Matrix hiding (translation)
import Linear.Metric
import Linear.V0
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4
import qualified Linear.V1
import qualified Linear.V2
import qualified Linear.V3
import qualified Linear.V4

import qualified Data.List.Split
import Data.String (IsString(..))

import qualified Text.XML.Light
import qualified Text.XML.Light as X

#if MIN_VERSION_linear(1,20,0)
#else
import Linear.Epsilon
#endif

import Lubeck.Str

#ifdef __GHCJS__
import GHCJS.Types(JSVal, JSString)
import Web.VirtualDom.Svg (Svg)
import qualified Web.VirtualDom as VD
import qualified Web.VirtualDom.Svg as E
import qualified Web.VirtualDom.Svg.Attributes as A
#endif

import System.IO.Unsafe(unsafePerformIO)

{-
Implementation notes:

Conceptually, a style is a left-biased string map (MonoidMap Str (First Str))

Why left-biased? Consider @style (styleNamed k1 s1) (style (styleNamed k2 s2) x)@
where we want s1 to take precedence over s2 whenever k1 === k2.

The standard Map is left-biased, so @Map k v ~ MonoidMap k (First v)@

In GHCJS we use a plain JavaScript string with the same behavior for performance.
This works because SVG specifies which key takes precendence if a style attribute contains duplicates:
-- http://www.sitepoint.com/forums/showthread.php?102926-css-duplicate-property-values

-}

{-| -}
#ifdef __GHCJS__
newtype Style = Style_ { getStyle_ :: Str }

instance Monoid Style where
  mempty =  Style_ ""
  mappend = js_appendStyle

-- TODO SVG abstraction leak
{-| -}
styleNamed :: Str -> Str -> Style
styleNamed = js_styleNamed
{-# INLINE styleNamed #-}

{-| -}
styleToAttrString :: Style -> Str
styleToAttrString (Style_ x) = x
-- styleToAttrString = Map.foldrWithKey (\n v rest -> n <> ":" <> v <> "; " <> rest) "" . getStyle_
{-# INLINE styleToAttrString #-}

#else
newtype Style = Style_ { getStyle_ :: Map Str Str }
  deriving (Monoid)

{-| -}
styleNamed :: Str -> Str -> Style
styleNamed k v = Style_ $ Map.singleton k v
{-# INLINE styleNamed #-}

{-| -}
styleToAttrString :: Style -> Str
styleToAttrString = Map.foldrWithKey (\n v rest -> n <> ":" <> v <> "; " <> rest) "" . getStyle_
{-# INLINE styleToAttrString #-}
#endif

{-| -}
emptyStyle :: Style
emptyStyle = mempty
{-# INLINE emptyStyle #-}


{-| -}
apStyle :: Style -> Style -> Style
apStyle = mappend
{-# INLINE apStyle #-}

#ifdef __GHCJS__

foreign import javascript unsafe "$1 + ':' + $2 + ';'"
  js_styleNamed :: Str -> Str -> Style

foreign import javascript unsafe "$2 + $1"
  js_appendStyle :: Style -> Style -> Style
#endif
