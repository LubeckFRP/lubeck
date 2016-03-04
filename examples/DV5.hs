
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, OverloadedStrings, TupleSections #-}

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


facet :: a -> WidgetT r a (a -> a) -> IO (Signal r, Signal a, Sink (a -> a))
facet initialState widget = do
  (internalSink, internalEvents) <- newEvent
  aS              <- accumS initialState internalEvents
  let htmlS       = fmap (widget internalSink) aS
  return (htmlS, aS, internalSink)

facet2  :: (b -> a -> a) -> (c -> a -> a)
        -> a -> WidgetT r a c
        -> IO (Signal r, Signal a, Sink b)
facet2 f g z w = do
  (v,o,i) <- facet z (rmapWidget g w)
  return (v,o,contramapSink f i)


-- Drawing-based GUI



clickableDW :: WidgetT' Drawing ()
clickableDW outp () = mconcat [sq,ci]
  where
    sq = fillColor Colors.grey   square
    ci = fillColor Colors.yellow circle
clickableF :: ()
clickableF = facet2 (flip const) (flip const) mempty clickableDW

-- | A button that displays a boolan state and sends () when clicked.



gui :: Signal Drawing
gui = mempty

main = runAppReactive $ fmap (toSvg drawOpts . scale 200) gui
  where
    drawOpts = mempty { dimensions = P (V2 400 400)}
