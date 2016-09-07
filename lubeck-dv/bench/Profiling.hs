
{-
For GHC profiling (as opposed to manual benchmarking)

Test using (for example)
  stack --stack-yaml=stack-ghc.yaml build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" \
    && stack exec lubeck-dv-profiling -- +RTS -p

-}

module Main where

import Control.Lens(_1, _2)
import Lubeck.DV
import Lubeck.DV.Styling
import Lubeck.Drawing -- necessary ?
import Lubeck.Str
import System.Random






drawingToSvgString :: RenderingOptions -> Styling -> Styled Drawing -> Str
drawingToSvgString drawOpts style finalD = Lubeck.Drawing.toSvgStr drawOpts $ ($ style) $ Lubeck.DV.Styling.getStyled finalD


-- We will plot N points
n = 1000

dat :: [(Double, Double)]
dat = take n $ zip randomDoubles1 randomDoubles2
  where
    randomDoubles1 = randomRs (0,1::Double) (mkStdGen 123891)
    randomDoubles2 = randomRs (0,1::Double) (mkStdGen 927378)

main = do
  let p = plot dat [x <~ _1, y <~ _2] pointG
  print $ drawingToSvgString mempty mempty $ drawPlot p
