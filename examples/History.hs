
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import GHCJS.Types(JSString, jsval)
-- import Web.VirtualDom.Html (p, h1, div, text, form, button, img, hr, a, table, tbody, th, tr, td, input, label, ul, li)
-- import Web.VirtualDom.Html.Events (click, change, keyup, submit, stopPropagation, preventDefault, value)
-- import Web.VirtualDom.Html.Attributes (src, width, class_, href, target, width, src)
import qualified Web.VirtualDom.Html as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events as Ev

import Lubeck.FRP
import Lubeck.App (Html, runAppReactive)
import JavaScript.Cast (cast) -- JSVal -> Maybe a
import Lubeck.Web.History
import Lubeck.Forms
import Lubeck.Forms.Basic (rangeWidget, integerWidget)
import Lubeck.FRP.History

import qualified Unsafe.Coerce

page :: Sink () -> History -> IO (Signal Html)
page saveHistory history = do
  (inputView, intsE) <- componentEvent 0 (rangeWidget 0 100 1) mempty
  intsS              <- stepperS 0 intsE

  subscribeEvent (fmap (const ()) intsE) saveHistory
  intsS' <- chronicleS history intsS

  let outputView    = componentListen integerWidget intsS'
  return $ mconcat [inputView, outputView]

-- MAIN

main :: IO ()
main = do
  history <- newHistory

  (saveHistoryS, saveHistoryE) <- newEvent
  subscribeEvent saveHistoryE $ \_ -> do
    moment <- capture history
    pushState (jsval moment) "" ""
    return ()

  -- onpopstate $ \popStateEvent -> do
    -- case Unsafe.Coerce.unsafeCoerce $ getPopStateEventState (popStateEvent) of

      -- Nothing     -> return ()
      -- Just
      -- moment -> restore history moment

  page saveHistoryS history >>= runAppReactive
