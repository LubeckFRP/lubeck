
{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
  , OverloadedStrings
  , TupleSections
  , ConstraintKinds
  , TemplateHaskell
  #-}

module Lubeck.DV.Styling
  (
  -- * Styling

  -- ** Building a style
    Styling
  -- TODO exort all lenses here
  , renderingRectangle
  , zoom
  , zoomType
  , precision
  , noXY

  , axisStrokeWidth
  , axisStrokeColor

  , axisTextFontFamily
  , axisTextFontWeight
  , axisTextFontStyle
  , axisTextFontSizePx

  , linePlotStrokeColor
  , linePlotStrokeWidth
  , linePlotStroke
  , linePlotFillColor

  , scatterPlotStrokeColor
  , scatterPlotStrokeWidth
  , scatterPlotFillColor
  , scatterPlotShape
  , scatterPlotSize

  , barPlotBarColor
  , barPlotRotateColors
  -- , barPlotWidth
  , barPlotUngroupedOffset
  -- , barPlotGroupedOffset
  -- , barPlotSpaceUsed
  , barPlotOrientation

  , circularPlotColor

  , ratioPlotBackgroundColor
  , ratioPlotForegroundColor

  , tickTextTurn
  , tickTextAnchor
  , tickTextAlignmentBaseline
  , tickTextFontFamily
  , tickTextFontWeight
  , tickTextFontStyle
  , tickTextFontSizePx

  , labelTextAbsOffset
  , labelTextTurn
  , labelTextAnchor
  , labelTextFontFamily
  , labelTextFontWeight
  , labelTextFontStyle
  , labelTextFontSizePx

  , basicTickLength
  , basicTickStrokeWidth
  , basicTickColor

  , backgroundTickStrokeWidthX
  , backgroundTickStrokeWidthY
  , backgroundTickStrokeColorX
  , backgroundTickStrokeColorY

  -- ** Focus/zoom
  , ZoomType(..)
  , focusDefault
  , focusHalfSize
  , focusDoubleSize
  , focusLeft
  , focusRight
  , focusBottomLeft
  , focusFromRectangle

  -- ** Element selection
  , HoverSelect(..)
  , hoverSelectStates
  , HoverSelectUpdate(..)
  , hoverSelectEvents

  , InteractivePalette(..)
  , paletteToInteractive
  , getInteractivePalette

  -- ** Misc helper types
  , VerticalHorizontal(..)

  -- ** Running a style
  -- *** Styled monad
  , Styled
  , getStyled
  -- *** StyledT monad transformer
  , StyledT
  , getStyledT
  -- *** Utility
  , withDefaultStyle

  -- -- ** DV Monad
  -- , DV
  -- , DV_T
  -- , askStyling
  -- , localStyling
  -- , draw
  -- , postDrawing
  )
where

import Prelude hiding (div)
import qualified Prelude

import Control.Applicative
import Control.Lens ()
import Control.Lens.Operators
import Control.Lens.TH (makeLenses)
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Colour (Colour, AlphaColour, withOpacity, blend)
import Data.Monoid
import Data.IntMap(IntMap)
import qualified Data.Colour.Names as Colors

import Linear.Vector
import Linear.Affine
import Linear.V0
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4

import Lubeck.Str
import Lubeck.Drawing
import Lubeck.FRP(Sink, emptySink)
import Lubeck.Drawing.Transformation
import qualified Lubeck.Drawing

import Lubeck.DV.ColorPalette
  ( Palette
  , singleColour
  , paletteFromList
  , getColorFromPalette
  )
import Lubeck.DV.LineStyles
  ( LineStyles
  , defaultLineStyles
  , lineStylesFromList
  , extractLineStyle
  , lineStyleFromLineStyles
  )

data VerticalHorizontal = Vertical | Horizontal
  deriving (Eq, Ord, Show)

-- How to display a bar plot with more than two dimensions
data BarPlotType = Grouped | Stacked | TwoSides
  deriving (Eq, Ord, Show)

focusLeft, focusRight, focusBottomLeft :: Transformation Double
focusLeft       = recip $ rectToTransf (rect 0   0   0.5 1)
focusRight      = recip $ rectToTransf (rect 0.5 0   1   1)
focusBottomLeft = recip $ rectToTransf (rect 0   0   0.5 0.5)
focusDefault    = 1
-- | "Zoom out"
focusHalfSize   =  recip 2 :: Transformation Double
-- | "Zoom in"
focusDoubleSize =  2 :: Transformation Double

focusFromRectangle :: Rect Double -> Transformation Double
focusFromRectangle x = recip $ rectToTransf x

data ZoomType = NoAutoScale | AutoScaleY
  deriving (Eq, Show)

instance Monoid ZoomType where
  mempty = NoAutoScale
  mappend NoAutoScale y = y
  mappend x _ = x

data HoverSelect = NoHoverSelect | Hovering | Selected
  deriving (Eq, Ord, Enum, Show)

data HoverSelectUpdate a
  = HoverSelectMouseUp a
  | HoverSelectMouseDown a
  | HoverSelectMouseOver a
  | HoverSelectMouseOut a
  | HoverSelectMouseMovedInside a
  deriving (Eq, Ord, Show)

{-
Similar to palette, but include alternative colors depending hover/select state.
-}
newtype InteractivePalette a
  = InteractivePalette (HoverSelect -> Palette a)

paletteToInteractive :: Palette a -> InteractivePalette a
paletteToInteractive p = InteractivePalette (const p)

getInteractivePalette :: InteractivePalette a -> HoverSelect -> Palette a
getInteractivePalette (InteractivePalette x) = x


data Styling = Styling
  { _dummy                            :: ()

  -- ^ Rectangle in which the plot will be rendered (default @300 x 300@).
  --
  --   This is the distance between the local origin of the plot and the top-right corner.
  --   The bottom-left corner is always rendered at the local origin, if this is not desired,
  --   use the combinators in Drawing to move it.
  , _renderingRectangle               :: V2 Double

  -- ^ This affine transformation defines a rectangle that is the "current focus"
  --   of the plot (but see zoomType below!).

  --   Every normalizewed data point (i.e. in the UHQ) is transformed using this
  --   transformation, then possibly discarded (if it now falls outside the UHQ),
  --   then transformed into the rendering rectangle.

  --   Some examples:
  --    Focus on the left half of the data set: @focusFromRectangle (rect 0 0 0.5 1))@
  --    Focus on the right half of the data set: @focusFromRectangle (rect 0.5 0 1 1))@
  --    Focus on bottom-left square of the data set: @focusFromRectangle (rect 0 0 0.5 0.5))@
  , _zoom                             :: T2 Double

  -- ^ If AutoScaleY, then the Y value of zoom is ignored, and the plot is automatically fitted
  --   to the visible data points.
  , _zoomType                         :: ZoomType

  -- ^ If Nothing, render all data points.
  --   If @Just n@, render at least n data points.
  , _precision                        :: Maybe Int

  -- ^ If true the standard X/Y axis, ticks and legends are not rendered.
  --   Sensible default for circular plots.
  , _noXY                             :: Any

  , _axisTextFontFamily               :: First Str
  , _axisTextFontWeight               :: FontWeight
  , _axisTextFontStyle                :: FontStyle
  , _axisTextFontSizePx               :: Double

  , _axisStrokeWidth                  :: (Double, Double)
  , _axisStrokeColor                  :: (AlphaColour Double, AlphaColour Double)

  -- Line plots
  , _linePlotStrokeColor              :: Palette Double
  , _linePlotStrokeWidth              :: Double
  , _linePlotStroke                   :: LineStyles
  , _linePlotFillColor                :: Palette Double

  -- Scatter plots
  , _scatterPlotStrokeColor           :: Palette Double
  , _scatterPlotStrokeWidth           :: Double
  , _scatterPlotFillColor             :: Palette Double
  , _scatterPlotSize                  :: Double
  , _scatterPlotShape                 :: ()

  -- Bar plots
  -- Infinite list of bar colours:
  , _barPlotBarColor                  :: InteractivePalette Double

  -- ^ If true, each bar is rendered with a distinct color based on the
  -- colors provided to @barPlotBarColor@, otherwise the default color
  -- of the palette (as provided by @paletteToColor@) is used.
  --
  -- Note that the @color@ aesthetic is currently ignored for bar plots.
  , _barPlotRotateColors              :: Any

  -- , _barPlotWidth                     :: V2 Double -- TODO not used
  , _barPlotUngroupedOffset           :: Double
  -- , _barPlotGroupedOffset             :: V2 Double -- TODO not used
  -- , _barPlotStackedOffset             :: V2 Double
  -- Percentage of horizontal dim taken up by plots, in [0..1] (default 1)
  -- I.e. https://infogr.am/average_temperature_of_6_major_deserts

  -- , _barPlotSpaceUsed                 :: Double -- TODO not used
  -- Is this bar plot transposed or not?
  , _barPlotOrientation               :: VerticalHorizontal

  , _circularPlotColor                :: Palette Double

  , _ratioPlotBackgroundColor         :: Palette Double
  , _ratioPlotForegroundColor         :: Palette Double

  , _tickTextTurn                     :: (Angle Double, Angle Double)
  , _tickTextAnchor                   :: (TextAnchor, TextAnchor)
  , _tickTextAlignmentBaseline        :: (AlignmentBaseline, AlignmentBaseline)
  , _tickTextFontFamily               :: First Str
  , _tickTextFontWeight               :: FontWeight
  , _tickTextFontStyle                :: FontStyle
  , _tickTextFontSizePx               :: Double

  , _labelTextAbsOffset               :: V2 Double
  , _labelTextTurn                    :: Angle Double
  , _labelTextAnchor                  :: TextAnchor
  , _labelTextFontFamily              :: First Str
  , _labelTextFontWeight              :: FontWeight
  , _labelTextFontStyle               :: FontStyle
  , _labelTextFontSizePx              :: Double

  , _basicTickLength                  :: Double
  , _basicTickStrokeWidth             :: Double
  , _basicTickColor                   :: AlphaColour Double

  , _backgroundTickStrokeColorX       :: AlphaColour Double
  , _backgroundTickStrokeColorY       :: AlphaColour Double
  , _backgroundTickStrokeWidthX       :: Double
  , _backgroundTickStrokeWidthY       :: Double

  {-
  Mouse interaction for discrete elements.
  The elements are indexed by their offset in the list provided to 'Lubeck.DV.plot'.
  -}
  , _hoverSelectStates                :: IntMap HoverSelect
  , _hoverSelectEvents                :: Sink (HoverSelectUpdate Int)
  }
  -- deriving (Show)

makeLenses ''Styling


instance Monoid Styling where
  mempty = Styling
    { _dummy                        = mempty
    -- , _renderingRectangle           = V2 300 300
    , _renderingRectangle           = V2 400 300
    , _zoom                         = 1
    , _zoomType                     = mempty
    , _precision                    = Nothing
    , _noXY                         = mempty

    , _axisTextFontFamily           = mempty
    , _axisTextFontWeight           = mempty
    , _axisTextFontStyle            = mempty
    , _axisTextFontSizePx           = 12

    , _axisStrokeWidth              = (1.5, 1.5)
    , _axisStrokeColor              = ( Colors.black `withOpacity` 1
                                      , Colors.black `withOpacity` 1
                                      )

    , _linePlotStrokeColor          = paletteFromList $ fmap (`withOpacity` 0.6) defColorList
    , _linePlotStrokeWidth          = 2.5
    , _linePlotStroke               = defaultLineStyles

    -- TODO should arguably be renamed area/fill
    -- , _linePlotFillColor            = Colors.black `withOpacity` 0
    , _linePlotFillColor            = paletteFromList $ fmap (`withOpacity` 0.2) defColorList

    , _scatterPlotStrokeColor       = paletteFromList $ fmap (`withOpacity` 0.6) defColorList
    , _scatterPlotStrokeWidth       = 1
    -- , _scatterPlotFillColor         = paletteFromList $ fmap (`withOpacity` 0.1) defColorList
    , _scatterPlotFillColor         = singleColour $ Colors.white `withOpacity` 1
    , _scatterPlotSize              = 10 -- TODO should really be a ratio of rendering rectangle (x or y?)
    , _scatterPlotShape             = mempty

    , _barPlotBarColor              =
        InteractivePalette $ \hs -> case hs of
          NoHoverSelect ->
            (paletteFromList $ fmap (`withOpacity` 0.6) $ defColorList)
          Hovering ->
            (paletteFromList $ fmap (`withOpacity` 0.2) $ defColorList)
          Selected ->
            (paletteFromList $ fmap (`withOpacity` 0.9) $ defColorList)
    -- , _barPlotWidth                 = V2 1   0 -- TODO not actually used as other values are relative this anyway
    , _barPlotRotateColors          = mempty
    , _barPlotUngroupedOffset       = 0.5
    -- , _barPlotGroupedOffset         = V2 0   0
    -- , _barPlotStackedOffset         = V2 0   0.1
    -- , _barPlotSpaceUsed             = 9/10
    , _barPlotOrientation           = Vertical

    , _circularPlotColor            = paletteFromList $ fmap (`withOpacity` 0.6) defColorList

    , _ratioPlotBackgroundColor     = singleColour $ Colors.whitesmoke `withOpacity` 0.9
    , _ratioPlotForegroundColor     = singleColour $ Colors.red        `withOpacity` 0.6

    , _tickTextTurn                 = (0/8, 0)
    , _tickTextAnchor               = (TextAnchorMiddle, TextAnchorEnd)
    , _tickTextAlignmentBaseline    = (AlignmentBaselineHanging, AlignmentBaselineAuto)
    , _tickTextFontFamily           = mempty
    , _tickTextFontWeight           = mempty
    , _tickTextFontStyle            = mempty
    , _tickTextFontSizePx           = 12

    , _labelTextAbsOffset            = V2 0 0
    , _labelTextTurn                 = 0
    , _labelTextAnchor               = TextAnchorMiddle
    , _labelTextFontFamily           = mempty
    , _labelTextFontWeight           = mempty
    , _labelTextFontStyle            = mempty
    , _labelTextFontSizePx           = 12

    , _basicTickLength              = 10
    , _basicTickStrokeWidth         = 1
    , _basicTickColor               = Colors.grey       `withOpacity` 1

    , _backgroundTickStrokeColorX   = Colors.lightgrey  `withOpacity` 1
    , _backgroundTickStrokeColorY   = Colors.lightgrey  `withOpacity` 1
    , _backgroundTickStrokeWidthX   = 1
    , _backgroundTickStrokeWidthY   = 1

    , _hoverSelectStates            = mempty
    , _hoverSelectEvents            = emptySink
    }
  mappend = const

{-|
A somewhat arbitrary list. Used as a default palette for most colored elements.
-}
defColorList =
  [ Colors.red
  , Colors.green
  , Colors.blue
  , Colors.pink
  , Colors.orange
  , Colors.purple
  , Colors.paleturquoise
  , Colors.deepskyblue
  , Colors.steelblue
  , Colors.darkgreen
  , Colors.coral
  , Colors.cornsilk
  , Colors.oldlace
  , Colors.crimson
  , Colors.mediumaquamarine
  , Colors.azure
  , Colors.dodgerblue
  , Colors.cornflowerblue
  , Colors.slateblue
  , Colors.goldenrod
  , Colors.magenta
  , Colors.hotpink
  , Colors.darkmagenta
  , Colors.midnightblue
  ]

type Styled = StyledT Identity

newtype StyledT m a = Styled { _getStyled :: ReaderT Styling m a }
  deriving (Functor, Applicative, Monad, MonadReader Styling)

instance (Monad m, Monoid a) => Monoid (StyledT m a) where
  mempty = pure mempty
  mappend = liftA2 mappend

-- | Extract a 'Styled' value.
getStyled :: Styled a -> Styling -> a
getStyled = runReader . _getStyled

-- | Extract a 'Styled' value.
getStyledT :: StyledT m a -> Styling -> m a
getStyledT = runReaderT . _getStyled

withDefaultStyle :: Styled a -> a
withDefaultStyle x = getStyled x mempty

withDefaultStyleT :: StyledT m a -> m a
withDefaultStyleT x = getStyledT x mempty
