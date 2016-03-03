
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


-- TODO move
import Data.Time (UTCTime(..), DiffTime, Day(..))


-- TODO Debug
import Control.Concurrent(forkIO, threadDelay)
import Control.Monad(forever)

import Text.Blaze (Markup, Attribute, Tag, AttributeValue, dataAttribute)
import Text.Blaze.Internal (customParent)

import Lubeck.FRP
import Lubeck.DV.Drawing
import Lubeck.DV.SimpleNormalized
import Lubeck.App (Html, runAppReactive)
import Lubeck.Forms
import Lubeck.Forms.Select
  -- (Widget, Widget', component, bothWidget)
import Lubeck.Forms.Basic
import Lubeck.Drawing
import Lubeck.Util(showJS, parseDateAndTimeToUTC)

-- import Linear.Vector
-- import Linear.Affine
-- import Linear.V0
-- import Linear.V1
-- import Linear.V2
-- import Linear.V3
-- import Linear.V4

import qualified Lubeck.Drawing

import Data.Colour (withOpacity)
import qualified Data.Colour.Names as Colors


-- MAIN
main = do
  -- let dr = scale 10 (fillColor Colors.red circle)
  let dr = getStyled mempty $ combine [scatterDataX, scatterData]  ordRandPoints

  let svgStr = toSvgAny mempty dr id $
              \name attrs nodes -> "<" <> name <> " "
                <> mconcat (Data.List.intersperse " " $ fmap (\(k,v) -> k <> "=\"" <> v <> "\"") attrs)
                <> ">"
                <> mconcat nodes <> "</" <> name <> ">"

  putStrLn $ unpackStr svgStr
  where
    -- combine [f, g...] x = mconcat [f x, g x...]
    combine fs x = mconcat $ fmap ($ x) fs


-- Some random series for testing

randPoints, ordRandPoints :: [P2 Double]
ordRandPoints = (Data.List.sortBy (Data.Ord.comparing (view _x)) $ take 10 randPoints)
randPoints    = zipWith (\x y -> P (V2 x y)) rand1 rand2
randVectors   = zipWith (\x y -> (V2 x y)) rand1 rand2

rand1, rand2 :: [Double]
rand1 = randoms $ fst $ split randG
rand2 = randoms $ snd $ split randG

randG = (mkStdGen 8712261455)
-- randG = (mkStdGen 123456789)
-- randG = (mkStdGen 3141599999)
