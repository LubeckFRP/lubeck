
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude
import GHCJS.Types(JSString, jsval)
import Web.VirtualDom.Html (text)
import qualified Web.VirtualDom.Html as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events as Ev
import Unsafe.Coerce(unsafeCoerce)

import Lubeck.FRP
import Lubeck.App (Html, runAppReactive)
import Lubeck.Web.History
import Lubeck.Forms
import Lubeck.Forms.Basic (rangeWidget, integerWidget)
import Lubeck.FRP.History
import Lubeck.Util(showJS)

page :: Sink () -> History -> IO (Signal Html)
page saveHistory history = do
  (inputView, intsE) <- componentEvent 0 integerWidget mempty
  intsS              <- stepperS 0 intsE
  intsB              <- stepper  0 intsE

  subscribeEvent (fmap (const ()) intsE) saveHistory
  intsS' <- chronicleS history intsS

  let outputView    = componentListen integerWidget intsS'
  return $ mconcat
    [ pure $ text "Change this value:", inputView
    , pure $ text "See what happens to this value:", outputView
    , pure $ text "Try using the back and forward buttons!"
    ]

-- MAIN

main :: IO ()
main = do
  history <- newHistory

  (saveHistoryS, saveHistoryE) <- newEvent
  subscribeEvent saveHistoryE $ \_ -> do
    moment <- capture history
    pushState (jsval moment) "" ""
    return ()

  onpopstate $ \popStateEvent -> do
    -- TODO extract a Moment/JSString from a JSVal without unsafeCoerce
    case unsafeCoerce $ getState popStateEvent of
      moment -> restore history moment

  page saveHistoryS history >>= runAppReactive
