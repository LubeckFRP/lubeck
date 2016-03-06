
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections,
  TemplateHaskell, ConstraintKinds, CPP #-}

module Lubeck.DV.Styling
  (
  -- * Styling

  -- ** Building a style
    Styling
  -- TODO exort all lenses here
  , renderingRectangle
  , linePlotStrokeColor
  , linePlotStrokeWidth
  , linePlotStrokeType
  , linePlotFillColor

  , scatterPlotStrokeColor
  , scatterPlotFillColor
  , scatterPlotShape

  , barPlotBarColors
  , barPlotWidth
  , barPlotUngroupedOffset
  , barPlotGroupedOffset
  , barPlotSpaceUsed

  , scatterPlotSize
  , scatterPlotShape
  , ratioPlotBackgroundColor
  , ratioPlotForegroundColor
  , heatMapColour1
  , heatMapColour2

  -- ** Running a style
  -- *** Styled monad
  , Styled(..)
  , getStyled
  , withDefaultStyle
  -- *** StyledT monad transformer
  , StyledT(..)
  , getStyledT
  , withDefaultStyle
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
-- import Data.AffineSpace
import Data.Colour (Colour, AlphaColour, withOpacity, blend)
import Data.Monoid
-- import Data.VectorSpace
import qualified Data.Colour.Names as Colors
-- import qualified Data.VectorSpace as VS

import Linear.Vector
import Linear.Affine
-- import Linear.Matrix hiding (translation)
-- import Linear.Metric -- Needed?
import Linear.V0
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4

import Lubeck.Drawing
import qualified Lubeck.Drawing

#ifdef __GHCJS__
import GHCJS.Types(JSString)
import qualified Data.JSString
import Lubeck.Util(showJS)
#endif


data VerticalHorizontal = Vertical | Horizontal
-- How to display a bar plot with more than two dimensions.§
data BarPlotType = Grouped | Stacked | TwoSides

data Styling = Styling
  { _dummy                            :: ()

  -- ^ Rectangle in which the plot will be rendered (default @300 x 300@)
  , _renderingRectangle               :: V2 Double

  -- Line plots
  , _linePlotStrokeColor              :: AlphaColour Double
  , _linePlotStrokeWidth              :: Double
  , _linePlotStrokeType               :: () -- TODO
  , _linePlotFillColor                :: AlphaColour Double

  -- Scatter plots
  , _scatterPlotStrokeColor           :: AlphaColour Double
  , _scatterPlotStrokeWidth           :: Double
  , _scatterPlotFillColor             :: AlphaColour Double
  , _scatterPlotSize                  :: Double
  , _scatterPlotShape                 :: ()

  -- Bar plots
  -- Infinite list of bar colours:
  , _barPlotBarColors                 :: [AlphaColour Double]
  , _barPlotWidth                     :: V2 Double
  , _barPlotUngroupedOffset           :: V2 Double
  , _barPlotGroupedOffset             :: V2 Double
  , _barPlotStackedOffset             :: V2 Double
  -- Percentage of horizontal dim taken up by plots, in [0..1] (default 1)
  -- I.e. https://infogr.am/average_temperature_of_6_major_deserts
  , _barPlotSpaceUsed                 :: Double

  , _ratioPlotBackgroundColor         :: AlphaColour Double
  , _ratioPlotForegroundColor         :: AlphaColour Double

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

  -- Heat maps and related
  , _heatMapColour1                   :: AlphaColour Double
  , _heatMapColour2                   :: AlphaColour Double

  }
  deriving (Show)

makeLenses ''Styling

instance Monoid Styling where
  mempty = Styling
    { _dummy                        = mempty
    , _renderingRectangle           = V2 300 300

    , _linePlotStrokeColor          = Colors.red `withOpacity` 0.6
    , _linePlotStrokeWidth          = 2.5
    , _linePlotStrokeType           = mempty
    , _linePlotFillColor            = Colors.black `withOpacity` 0

    , _scatterPlotStrokeColor       = Colors.red `withOpacity` 0.6
    , _scatterPlotStrokeWidth       = 1
    , _scatterPlotFillColor         = Colors.red `withOpacity` 0.6
    , _scatterPlotSize              = 10 -- TODO should really be a ratio of rendering rectangle (x or y?)
    , _scatterPlotShape             = mempty

    , _barPlotBarColors             = fmap (`withOpacity` 0.6) $ cycle
                                      [ Colors.red
                                      , Colors.green
                                      , Colors.blue
                                      , Colors.pink
                                      , Colors.orange
                                      , Colors.purple
                                      ]
    , _barPlotWidth                 = V2 1   0 -- TODO not actually used as other values are relative this anyway
    , _barPlotUngroupedOffset       = V2 0.5 0
    , _barPlotGroupedOffset         = V2 0   0
    , _barPlotStackedOffset         = V2 0   0.1
    , _barPlotSpaceUsed             = 9/10

    , _ratioPlotBackgroundColor     = Colors.whitesmoke `withOpacity` 0.9
    , _ratioPlotForegroundColor     = Colors.red        `withOpacity` 0.6

    , _heatMapColour1               = Colors.red        `withOpacity` 1
    , _heatMapColour2               = Colors.purple     `withOpacity` 1
    }
  mappend = const

type Styled = StyledT Identity

newtype StyledT m a = Styled { _getStyled :: ReaderT Styling m a }
  deriving (Functor, Applicative, Monad, MonadReader Styling)

instance
  ( Monad m
  , Monoid a
  ) => Monoid (StyledT m a) where
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
