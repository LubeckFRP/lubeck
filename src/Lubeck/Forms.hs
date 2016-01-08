
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

-- data Submit a
  -- = Intermittent a -- ^ Store new value internally, don't send on changes.
  -- | Submit         -- ^ Send on changes (as per last @Intermittent@).
--
-- form :: a -> (Widget a (Sink (Submit a))) -> IO (Signal Html, Events a)
