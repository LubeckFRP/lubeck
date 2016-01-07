
module Lubeck.Forms where

import Lubeck.FRP
import Lubeck.Html (Html)

type Widget i o = Sink o -> i -> Html
type Widget' a  = Widget a a

component :: a -> Widget' a -> IO (Signal Html, Events a)
component z w = do
  -- Value changed
  (aSink,aEvent) <- newEvent
  aS <- stepperS z aEvent
  let htmlS = fmap (w aSink) aS
  return (htmlS, aEvent)
