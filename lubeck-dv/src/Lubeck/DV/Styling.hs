
{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
  , TemplateHaskell
  , OverloadedStrings
  , TupleSections
  , ConstraintKinds
  #-}

module Lubeck.DV.Styling
  (
  -- * Styling

  -- ** Building a style
    Styling
  -- TODO exort all lenses here
  , renderingRectangle
  , zoom

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

  , barPlotBarColors
  , barPlotWidth
  , barPlotUngroupedOffset
  , barPlotGroupedOffset
  , barPlotSpaceUsed

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
-- How to display a bar plot with more than two dimensions.§
data BarPlotType = Grouped | Stacked | TwoSides

data Styling = Styling
  { _dummy                            :: ()

  -- ^ Rectangle in which the plot will be rendered (default @300 x 300@)
  , _renderingRectangle               :: V2 Double

  -- ^ Linear transformation to apply to data just before rendering (i.e. after
  -- data has been mapped and normalized and thus is already in the UHQ).
  --
  --
  , _zoom                             :: V2 Double

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
  , _barPlotBarColors                 :: Palette Double
  , _barPlotWidth                     :: V2 Double
  , _barPlotUngroupedOffset           :: V2 Double
  , _barPlotGroupedOffset             :: V2 Double
  , _barPlotStackedOffset             :: V2 Double
  -- Percentage of horizontal dim taken up by plots, in [0..1] (default 1)
  -- I.e. https://infogr.am/average_temperature_of_6_major_deserts
  , _barPlotSpaceUsed                 :: Double
  -- Is this bar plot transposed or not?
  -- , _barPlotTransposed                :: Bool

  , _ratioPlotBackgroundColor         :: Palette Double
  , _ratioPlotForegroundColor         :: Palette Double

  -- Color allocator
    -- TODO idea: to allocate colors to categories/dimensions
    -- This could work very well with Control.Monad.Reader.local.

    -- I.e. there should be a function (subStyle 1 4 :: Styling -> Styling) that transforms the styling
    -- (in this case 1 out of 4), by recursively applying over all styles that support this (i.e. the bar colour space).

    -- NOTE there are two ways of using color with this API: through styling (appropriate for
    -- bar groups etc where the color is not strictly bound to the data), or through an extra R dimension ("withColor",
    -- appropriate for heat maps etc)

  -- Axis/ticks
    -- X,Y axis name
      -- NOTE: Standard styles: left/below centered, at end

    -- X Axis standard (usually left) line (strokeWith, strokeColorA)
    -- X Axis opposite                line (strokeWith, strokeColorA)
    -- Y Axis standard                line (strokeWith, strokeColorA)
    -- Y Axis opposite                line (strokeWith, strokeColorA)

    -- Axis arrow end?

    -- NOTE: strike-through/background ticks are rarely used together
    -- X,Y strike-through/background ticks
    -- X,Y tick (length, width, pos rel axis (see below), strokeColorA)
    -- Text position relative tick
    -- Text rotation
      -- NOTE: common(x,y) is (turn/4,0), (turn/8,0), (0,0)

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
  }
  deriving (Show)

makeLenses ''Styling

instance Monoid Styling where
  mempty = Styling
    { _dummy                        = mempty
    -- , _renderingRectangle           = V2 300 300
    , _renderingRectangle           = V2 400 300
    , _zoom                         = V2 1 1

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
    -- , _scatterPlotFillColor         = Colors.red `withOpacity` 0.6
    , _scatterPlotFillColor         = singleColour $ Colors.white `withOpacity` 1
    , _scatterPlotSize              = 10 -- TODO should really be a ratio of rendering rectangle (x or y?)
    , _scatterPlotShape             = mempty

    , _barPlotBarColors             = paletteFromList $ fmap (`withOpacity` 0.6) $ defColorList
    , _barPlotWidth                 = V2 1   0 -- TODO not actually used as other values are relative this anyway
    , _barPlotUngroupedOffset       = V2 0.5 0
    , _barPlotGroupedOffset         = V2 0   0
    , _barPlotStackedOffset         = V2 0   0.1
    , _barPlotSpaceUsed             = 9/10

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
    }
  mappend = const

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


--
-- type DV = DV_T Identity
-- -- DV_S = DV_T Identity
-- -- DV_I = DV_T Behavior
--
-- newtype DV_T m a = DV_T { _getDV_T :: ReaderT Styling (WriterT Drawing m) a }
--   deriving (Functor, Applicative, Monad, MonadReader Styling, MonadWriter Drawing)
-- {-
-- ReaderT Styling (WriterT Drawing m) a
-- Styling -> WriterT Drawing m a
-- Styling -> m (a, Drawing)
-- -}
--
--
-- liftDV :: Monad m => m a -> DV_T m a
-- liftDV = DV_T . lift . lift
--
-- instance (Monad m, Monoid a) => Monoid (DV_T m a) where
--   mempty = pure mempty
--   mappend = liftA2 mappend
--
-- -- | Get current styling (i.e. for drawing)
-- askStyling :: Monad m => DV_T m Styling
-- askStyling = ask
--
-- -- | Apply a local styling (i.e. for subgraphs)
-- localStyling :: Monad m => (Styling -> Styling) -> DV_T m a -> DV_T m a
-- localStyling = local
--
-- -- | Draw something to the screen
-- draw :: Monad m => Drawing -> DV_T m ()
-- draw = tell
--
-- drawM :: Monad m => m Drawing -> DV_T m ()
-- drawM x = pass $ fmap (\d -> ((), (<> d))) $ liftDV x
--
-- -- | Apply a transformation to the current drawing (useful for facets etc).
-- postDrawing :: Monad m => (Drawing -> Drawing) -> DV_T m a -> DV_T m a
-- postDrawing = censor
