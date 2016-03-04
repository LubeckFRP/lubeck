
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, OverloadedStrings, TupleSections, ScopedTypeVariables #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import Data.Monoid ((<>))
import GHCJS.Types(JSString, jsval)
import qualified Web.VirtualDom.Html as H
import qualified Data.JSString
import qualified Data.List
import qualified Data.Ord
import System.Random (mkStdGen, randoms, split)
import Control.Lens (view)
import Control.Lens.Operators
import Control.Applicative

import Lubeck.FRP
import Lubeck.App (Html, runAppReactive)
import Lubeck.Forms
import Lubeck.Forms.Basic
import Lubeck.Forms.Button
import Lubeck.Forms.Select
import Lubeck.Drawing
import Lubeck.Util(showJS, parseDateAndTimeToUTC)
import qualified Lubeck.Drawing
import Data.Colour (withOpacity)
import qualified Data.Colour.Names as Colors


-- MAIN
{-
Basic GUI examples:
-}

-- | FRP routing monad. Should be MonadIO but not IO. Could be more specific, i.e. separating polling/sending etc a la reflex.
type FRP = IO

-- A facet is much like a widget or component.
type Facet   r a b = (Signal r, Signal b, Sink a)
type FacetO  r b   = (Signal r, Signal b)

facet :: a -> WidgetT r a (a -> a) -> FRP (Facet r (a -> a) a)
facet initialState widget = do
  (internalSink, internalEvents) <- newEvent
  aS              <- accumS initialState internalEvents
  let htmlS       = fmap (widget internalSink) aS
  return (htmlS, aS, internalSink)

facet2  :: (b -> a -> a) -> (c -> a -> a)
        -> a -> WidgetT r a c
        -> FRP (Facet r b a)
facet2 f g z w = do
  (v,o,i) <- facet z (rmapWidget g w)
  return (v,o,contramapSink f i)

facetOutputOnly :: Monoid a => WidgetT r a c -> FRP (FacetO r a)
facetOutputOnly w = dropInput <$> facet2 (flip const) (flip const) mempty w
  where
    dropInput (v,o,i) = (v,o)


-- Drawing-based GUI

clickableDW :: WidgetT' Drawing ()
clickableDW outp () = mconcat [ci,sq]
  where
    sq = fillColor Colors.grey   square
    ci = fillColor Colors.yellow circle
clickableF :: FRP (FacetO Drawing ())
clickableF = facetOutputOnly clickableDW
-- | A button that displays a boolan state and sends () when clicked.



gui :: FRP (Signal Drawing)
gui = do
  (v1,_) <- clickableF
  (v2,_) <- clickableF
  return $ liftA2 (|||) v1 v2
  -- return mempty

main = do
  (x :: Signal Html) <- fmap (fmap (toSvg drawOpts . scale 200)) $ gui
  runAppReactive x
  where
    drawOpts = mempty { dimensions = P (V2 400 400)}
