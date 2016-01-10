
{-# LANGUAGE NoMonomorphismRestriction, RankNTypes, TypeFamilies, TemplateHaskell #-}

{-|
Provides high-level way of constructing forms and other interfaces.

/Experimental/
-}
module Lubeck.Forms
  (
  -- * Widget type
    Widget
  , Widget'

  -- ** Mapping over widgets
  , lmapWidget
  , rmapWidget
  , dimapWidget
  , mapHtmlWidget

  -- ** Combining and transforming widgets
  , subWidget
  , bothWidget
  , multiWidget
  -- , mapMWidget

  -- * Components
  , component
  , formComponent

  -- ** Submit type
  , Submit(..)
  , submitValue
  , submits
  ) where

import Lubeck.FRP
import Lubeck.Html (Html)

import Control.Lens (over, set, view, lens, Lens, Lens')
import Control.Lens.TH (makeLenses)

{-|
Provides a way of rendering a value of some type @i@, and emitting updates of some type @i@.
-}
type Widget i o = Sink o -> i -> Html

{-|
A variant of 'Widget' where input and output is the same.
-}
type Widget' a  = Widget a a

-- | Map over the input type of a widget.
lmapWidget :: (b -> a) -> Widget a s -> Widget b s
lmapWidget f w st ab = w st (f ab)

-- | Map over the output type of a widget.
rmapWidget :: (s -> t) -> Widget a s -> Widget a t
rmapWidget f w st ab = w (contramapSink f st) ab

-- | Map over both input and output of a widget.
dimapWidget :: (a -> b) -> (c -> d) -> Widget b c -> Widget a d
dimapWidget f g = lmapWidget f . rmapWidget g

-- | Map over the HTML rendering of a widget.
mapHtmlWidget :: (Html -> Html) -> Widget a b -> Widget a b
mapHtmlWidget f w = \s -> f . w s

-- | Turn a widget of a smaller type into a widget of a larger type using a lens.
--
-- @
-- subWidget _1     :: Widget' a -> Widget' (a, b)
-- subWidget (at 1) :: Widget' Just a -> Widget' (Map Int a)
-- subWidget inside :: Widget' (e -> a) -> Widget' (e -> s)
-- @
subWidget :: Lens' s a -> Widget' a -> Widget' s
subWidget l w o i = w (contramapSink (\x -> set l x i) o) (view l i)

-- | Compose two widgets.
-- Both render the value and the resultant HTML is composed using the given function.
-- Output emitted by either widget is emitted by the resultant widget.
bothWidget :: (Html -> Html -> Html) -> Widget a b -> Widget a b -> Widget a b
bothWidget c w1 w2 o i = w1 o i `c` w2 o i

-- | Compose a widget out of several widgets of the same type.
-- Useful in conjunction with 'subWidget', as in
--
-- @
-- data Many = Many { _foo :: Int, _bar :: Maybe String }
-- makeLenses ''Many
--
-- manyW :: Widget Many Many
-- manyW = multiWidget (\x y -> div () [x,y])
--   [subWidget foo intW, subWidget bar (maybeW stringW)]
-- @
multiWidget :: Foldable t => (Html -> Html -> Html) -> t (Widget a b) -> Widget a b
multiWidget f = foldr1 (bothWidget f)

mapMWidget :: ([Html] -> Html) -> Widget a a -> Widget [a] a
mapMWidget k w o is = k $ fmap (w o) is

--
-- data Many = Many { _foo :: Int, _bar :: Maybe String }
-- makeLenses ''Many
--
-- manyW :: Widget Many Many
-- manyW = multiWidget (\x y -> x)
--   [subWidget foo intW, subWidget bar (maybeW stringW)]
--
-- intW :: Widget' Int
-- stringW :: Widget' String
-- maybeW :: Widget' a -> Widget' (Maybe a)
-- [intW, stringW, maybeW] = undefined


-- | Create a component from a widget and an initial value.
-- The component starts out in an initial state determined by the first argument.
--
-- Both the state and the view signals are updated whenever the user interacts
-- with the component.
component :: a -> Widget' a -> IO (Signal Html, Events a)
component z w = do
  -- Value changed
  (aSink,aEvent) <- newEvent -- Events a
  aS <- stepperS z aEvent
  let htmlS = fmap (w aSink) aS
  return (htmlS, aEvent)

-- | A variant of component that supports chanching its value internally without
-- sending on any updates.
--
-- Appropriate for forms with a \"Submit\" button.
formComponent :: a -> Widget a (Submit a) -> IO (Signal Html, Events a)
formComponent z w = do
  -- Value changed
  (aSink,aEvent) <- newEvent :: IO (Sink (Submit a), Events (Submit a))
  aS <- stepperS (DontSubmit z) aEvent
  let htmlS = fmap (w aSink . submitValue) aS
  return (htmlS, submits aEvent)

-- | A helper type for 'formComponent'.
data Submit a
  = DontSubmit a -- ^ Store new value internally, don't send on changes.
  | Submit a     -- ^ Send on changes.

-- | Extract the value.
submitValue :: Submit a -> a
submitValue (DontSubmit x) = x
submitValue (Submit x)     = x

-- | Extract just submit events, ignore others.
submits :: Events (Submit a) -> Events a
submits = filterJust . fmap g
  where
    g (Submit x) = Just x
    g _          = Nothing
