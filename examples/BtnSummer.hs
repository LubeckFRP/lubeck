{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where

import Prelude

import Control.Applicative

import Data.JSString (JSString, pack, unpack)

import qualified Web.VirtualDom as V
import qualified Web.VirtualDom.Html as E
import qualified Web.VirtualDom.Html.Events as EV
import qualified Web.VirtualDom.Html.Attributes as A

import Lubeck.App (Html, runAppReactive)
import Lubeck.Forms
import Lubeck.Forms.Basic
import Lubeck.FRP

-- | Emits the given integer, ignores input.
intButton :: Int -> Widget' Int
intButton n sink _ = E.button
  [ A.type_ "button"
  , A.class_ "btn btn-secondary"
  , EV.click $ \_ -> sink n
  ]
  [ E.text (pack $ show n) ]

intButtons :: Int -> Widget' Int
intButtons n sink val = E.div
  [ A.class_ "row" ] $
  map (\n -> intButton n sink val) [1..n]

-- | Displays an integer, never emits output.
sumWidget :: Widget' Int
sumWidget _ val = E.div
  [ A.class_ "col-xs-2" ]
  [ E.p [ A.class_ "text-center" ]
        [ E.text (pack $ show val) ]
  ]

-- | A button emitting () when clicked.
resetButton :: Widget' ()
resetButton sink _ = E.button
  [ A.type_ "button"
  , A.class_ "btn btn-primary"
  , EV.click $ \_ -> sink ()
  ]
  [ E.text (pack $ "<- Reset") ]

sumAndReset :: Events Int -> Events () -> IO (Behavior Int)
sumAndReset intE resetE = accumB 0 $ merge adder putZ
  where
    adder = fmap (+) intE -- :: Event (Int -> Int)
    putZ = fmap (\() -> const 0) resetE -- :: Event (Int -> Int)

sample2 :: Behavior a -> Events b -> Events c -> Events a
sample2 b e1 e2 = sample b (mappend e1' e2')
  where
    e1' = fmap (\_ -> ()) e1
    e2' = fmap (const ()) e2

main :: IO ()
main = do
  (intBtnDisp, intE) <- component 0 $ intButtons 45
  (resetDisp, resetE) <- component () resetButton
  sumAndResB <- sumAndReset intE resetE
  let sumAndResetE = sample2 sumAndResB intE resetE

  (sumDisp, _) <- componentEvent 0 sumWidget $ sumAndResetE
  runAppReactive $ mconcat [intBtnDisp, sumDisp, resetDisp]
