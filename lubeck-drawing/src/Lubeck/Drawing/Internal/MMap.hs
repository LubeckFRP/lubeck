
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, TypeFamilies, OverloadedStrings,
  NamedFieldPuns, CPP, NoMonomorphismRestriction, BangPatterns, StandaloneDeriving
  , ScopedTypeVariables, DeriveTraversable #-}

module Lubeck.Drawing.Internal.MMap
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

import BasePrelude
import Data.Map.Strict(Map)
import qualified Data.Map as Map

{-|
Map with a better Monoid instance.
See https://mail.haskell.org/pipermail/libraries/2012-April/017747.html

-}
newtype MMap k a = MMap { getMMap :: Map k a }
  deriving (Functor, Foldable, Traversable)

instance (Ord k, Monoid v) =>  Monoid (MMap k v) where
  mempty  = MMap $ Map.empty
  mappend (MMap a) (MMap b) = MMap $ Map.unionWith mappend a b

singletonMMap k v = MMap $ Map.singleton k v

toListMMap (MMap m) = Map.toList m
