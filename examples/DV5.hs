
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



-- Drawing-based GUI

clickableDW :: WidgetT' Drawing ()
clickableDW = undefined

main = runAppReactive mempty
