
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import Control.Applicative

import Data.JSString (JSString, pack, unpack)
import GHCJS.Types (jsval)

import qualified Web.VirtualDom as V
import qualified Web.VirtualDom.Html as E
import qualified Web.VirtualDom.Html.Events as EV
import qualified Web.VirtualDom.Html.Attributes as A

import Lubeck.App (Html, runAppReactive)
import Lubeck.Forms
import Lubeck.Forms.Basic
import Lubeck.Forms.Button
import Lubeck.FRP

sumAndReset :: Events Int -> Events () -> IO (Signal Int)
sumAndReset intE resetE = accumS 0 $ merge adder putZ
  where
    adder = fmap (+) intE -- :: Event (Int -> Int)
    putZ = fmap (\() -> const 0) resetE -- :: Event (Int -> Int)

main :: IO ()
main = do
  (intBtnView, intE) <- component 0 . multiButtonWidget $ map (\x -> (pack (show x), x)) [1..20]
  (resetView, resetE) <- component () $ buttonWidget "Reset Sum"
  sumAndResS <- sumAndReset intE resetE
  let sumView = componentListen intDisplayWidget sumAndResS

  runAppReactive $ mconcat [intBtnView, sumView, resetView]
