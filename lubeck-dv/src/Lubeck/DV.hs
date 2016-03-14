
{-# LANGUAGE CPP #-}

module Lubeck.DV
  ( module Lubeck.DV.Animation
  , module Lubeck.DV.Drawing
  , module Lubeck.DV.Interactive
  , module Lubeck.DV.New
  , module Lubeck.DV.SimpleNormalized
  , module Lubeck.DV.Styling
#ifndef __GHCJS__
  , testPlot
#endif
  )
where

import Lubeck.DV.Animation
import Lubeck.DV.Drawing
import Lubeck.DV.Interactive
import Lubeck.DV.New
import Lubeck.DV.SimpleNormalized
import Lubeck.DV.Styling

import Lubeck.Drawing
import Data.Functor.Identity

#ifndef __GHCJS__
-- | Utility. Do not rely oin this in production.
testPlot :: StyledT Identity Drawing -> IO ()
testPlot x = writeFile "./static/tmp/test.svg" $ toSvgStr mempty $ withDefaultStyle x
#endif
