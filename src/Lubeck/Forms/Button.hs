
{-# LANGUAGE OverloadedStrings #-}

{-|
  Button widgets, used for creating basic on-click behavior.
  
  The main button widget accepts a string as the first argument 
  to be used as the label of the button.

-}
module Lubeck.Forms.Button
  ( buttonWidget 
  , multiButtonWidget
  ) where

import Lubeck.Forms
import Lubeck.FRP

import Data.JSString (JSString, pack)

import qualified Web.VirtualDom as VD
import qualified Web.VirtualDom.Html as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events as Ev 

buttonWidget :: JSString -> Widget' () 
buttonWidget label sink val = E.button
  [ A.class_ "btn btn-primary"
  , Ev.click $ \_ -> sink ()
  ] 
  [ E.text label ]

multiButtonWidget :: [(JSString,b)] -> Widget a b
multiButtonWidget lbls sink val = E.div
  [ A.class_ "btn-group" ] $ 
  map (makeBtn sink) lbls 
  where
    makeBtn :: Sink b -> (JSString,b) -> E.Html
    makeBtn sink (label,e) = buttonWidget label (contramapSink (\() -> e) sink) () 
