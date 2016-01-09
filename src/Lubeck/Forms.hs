
{-|
Provides high-level way of constructing forms and other interfaces.

/Experimental/
-}
module Lubeck.Forms where

import Lubeck.FRP
import Lubeck.Html (Html)

{--}
type Widget i o = Sink o -> i -> Html

type Widget' a  = Widget a a

lmapWidget :: (b -> a) -> Widget a s -> Widget b s
lmapWidget f w st ab = w st (f ab)

rmapWidget :: (s -> t) -> Widget a s -> Widget a t
rmapWidget f w st ab = w (contramapSink f st) ab

dimapWidget :: (a -> b) -> (c -> d) -> Widget b c -> Widget a d
dimapWidget f g = lmapWidget f . rmapWidget g

mapHtmlWidget :: (Html -> Html) -> Widget a b -> Widget a b
mapHtmlWidget f w = \s -> f . w s



component :: a -> Widget' a -> IO (Signal Html, Events a)
component z w = do
  -- Value changed
  (aSink,aEvent) <- newEvent -- Events a
  aS <- stepperS z aEvent
  let htmlS = fmap (w aSink) aS
  return (htmlS, aEvent)

formComponent :: a -> Widget a (Submit a) -> IO (Signal Html, Events a)
formComponent z w = do
  -- Value changed
  (aSink,aEvent) <- newEvent :: IO (Sink (Submit a), Events (Submit a))
  aS <- stepperS (DontSubmit z) aEvent
  let htmlS = fmap (w aSink . value) aS
  return (htmlS, submits aEvent)


data Submit a
  = DontSubmit a -- ^ Store new value internally, don't send on changes.
  | Submit a     -- ^ Send on changes.

value :: Submit a -> a
value (DontSubmit x) = x
value (Submit x)     = x

submits :: Events (Submit a) -> Events a
submits = filterJust . fmap g
  where
    g (Submit x) = Just x
    g _          = Nothing
