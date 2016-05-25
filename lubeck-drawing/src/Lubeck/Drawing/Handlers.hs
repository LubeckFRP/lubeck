
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, TypeFamilies, OverloadedStrings,
  NamedFieldPuns, CPP, NoMonomorphismRestriction, BangPatterns, StandaloneDeriving
  , ScopedTypeVariables #-}

-- {-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}

module Lubeck.Drawing.Handlers
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

import BasePrelude hiding (Handler, Handlers)

import Lubeck.Str
import Lubeck.Drawing.Internal.MMap

#ifdef __GHCJS__
import GHCJS.Types(JSVal, JSString)
import Web.VirtualDom.Svg (Svg)
import qualified Web.VirtualDom as VD
import qualified Web.VirtualDom.Svg as E
import qualified Web.VirtualDom.Svg.Attributes as A
#endif


#ifdef __GHCJS__
newtype Handler = Handler (JSVal -> IO ())

-- TODO would be derivable if IO lifted the Monoid...
instance Monoid Handler where
  mempty = Handler (\_ -> pure ())
  mappend (Handler a) (Handler b) = Handler (apSink a b)
    where
      apSink a b x = do
        a x
        b x

newtype Handlers = Handlers_ (MMap Str Handler)
  deriving Monoid

singleTonHandlers :: Str -> (JSVal -> IO ()) -> Handlers
singleTonHandlers attrName sink = Handlers_ $ singletonMMap attrName (Handler sink)

handlersToProperties :: Handlers -> [E.Property]
handlersToProperties (Handlers_ m)
  = fmap (\(n, Handler v) -> VD.on (toJSString n) v) $ toListMMap m
{-# INLINABLE handlersToProperties #-}

#else
type Handler = ()
type Handlers = ()
#endif
