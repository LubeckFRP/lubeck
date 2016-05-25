
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, TypeFamilies, OverloadedStrings,
  NamedFieldPuns, CPP, NoMonomorphismRestriction, BangPatterns, StandaloneDeriving
  , ScopedTypeVariables #-}

module Lubeck.Drawing.Types
  -- (
  -- -- * Creating drawings
  -- -- ** Geometry
  --   Point(..)
  -- , V1(..)
  -- , V2(..)
  -- , V3(..)
  -- , V4(..)
  -- , P1
  -- , P2
  -- , P3
  -- , P4
  -- , _x
  -- , _y
  --
  -- , Angle
  -- , acosA
  -- , turn
  -- , angleToRadians
  -- , angleToDegrees
  -- -- TODO move/rename these?
  -- , offsetVectors
  -- , betweenPoints
  --
  -- , Direction
  -- , dir
  -- , fromDirection
  -- , angleBetween
  -- , angleBetweenDirections
  --
  -- , Rect(..)
  -- , fitInsideRect
  --
  -- -- ** Transformations
  -- , Transformation
  -- , negTransformation
  -- , lin
  -- , transp
  -- , transl
  -- , transformVector
  -- , transformPoint
  -- , transformDirection
  -- , transformEnvelope
  -- , transformationToMatrix
  -- -- ** Raw transformations
  -- , rotation
  -- , scaling
  -- , scalingX
  -- , scalingY
  -- , translation
  -- , translationX
  -- , translationY
  -- , shearingX
  -- , shearingY
  -- -- $matrixContructorLayout
  -- , matrix
  -- -- ** Applying transformations
  -- , transform
  --
  -- , rotate
  -- , scale
  -- , scaleX
  -- , scaleY
  -- , scaleXY
  -- , translate
  -- , translateX
  -- , translateY
  -- -- , scaleXY
  -- , shearX
  -- , shearY
  --
  -- -- ** Styling
  -- , Style
  -- , styleNamed
  -- , fillColor
  -- , fillColorA
  -- , strokeColor
  -- , strokeColorA
  -- , strokeWidth
  -- -- *** Rendering styles
  -- , styleToAttrString
  -- -- *** Applying styles
  -- , style
  --
  -- -- *** Line style
  -- , dash
  -- , dashing
  --
  -- -- *** Text
  -- , text
  -- , textMiddle
  -- , textEnd
  -- , textLeftMiddle
  -- , textMiddleMiddle
  -- , textRightMiddle
  -- , TextAnchor(..)
  -- , AlignmentBaseline(..)
  -- , FontStyle(..)
  -- , FontSize(..)
  -- , FontWeight(..)
  -- , TextOptions(..)
  -- , textWithOptions
  --
  -- -- ** Events
  -- , addHandler
  -- -- , addProperty
  --
  -- -- ** Embedded SVG
  -- , Embed(..)
  -- , addEmbeddedSVG
  -- , addEmbeddedSVGFromStr
  --
  -- -- ** Envelopes, Alignment, Juxtaposition
  -- , Envelope
  -- , envelope
  -- -- transformEnvelope
  -- , unitX
  -- , unitY
  -- , posDiagonal
  -- , negDiagonal
  -- , (|||)
  -- , (===)
  -- , juxtapose
  --
  -- , boundaries
  -- , align'
  -- , align
  -- , OctagonSide(..)
  --
  -- -- ** Drawings
  -- , Drawing
  -- -- ** Basic drawings
  -- , transparent
  -- , circle
  -- , square
  -- , triangle
  -- , horizontalLine
  -- , verticalLine
  -- , segments
  -- , polygon
  --
  -- -- ** Utility
  -- , xyAxis
  -- , xyCoords
  -- , showUnitX
  -- , showDirection
  -- , showPoint
  -- , showBoundaries
  -- , showEnvelope
  -- , smokeBackground
  --
  -- -- * Rendering drawings
  -- , OriginPlacement(..)
  -- , RenderingOptions(..)
  -- -- mempty
  -- , toSvg
  -- , toSvgStr
  -- , toSvgAny
  --
  -- -- ** High-performance
  -- , RDrawing
  -- , renderDrawing
  -- , emitDrawing
  -- )
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
