
{-# LANGUAGE NoMonomorphismRestriction, RankNTypes, TypeFamilies, TemplateHaskell, OverloadedStrings #-}

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
  , isoW
  , possW
  , maybeW
  , altW

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

  -- * Form helpers
  , longStringWidget


  ) where

import Lubeck.FRP
import Lubeck.Html (Html)
import Prelude hiding (div)
import qualified Prelude

import GHCJS.Types(JSString, jsval)

import Web.VirtualDom.Html (p, h1, div, text, form, button, img, hr, a, table, tbody, th, tr, td, input, label)
import Web.VirtualDom.Html.Events (click, change, keyup, submit, stopPropagation, preventDefault, value)
import Web.VirtualDom.Html.Attributes (src, width, class_, href, target, width, src)
import qualified Web.VirtualDom.Html as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events as Ev

import Control.Lens (over, under, set, view, review, preview, lens, Lens, Lens', Prism, Prism', Iso, Iso')
import qualified Control.Lens
import Control.Lens.TH (makeLenses)

{-|
Provides a way of:

- Rendering a value of some type @i@
- Sending updates of some type @o@ in response to user interaction.
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
-- The resulting widget will render part of the larger type that the lens selects.
-- It the resulting widget produces output, this will be used to transform the
-- value rendered by the larger widget. This makes this function especially
-- useful in conjunction with 'multiWidget'.
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
--   [subWidget foo intW, subWidget bar (maybeW (div () ()) stringW)]
-- @
multiWidget :: Foldable t => (Html -> Html -> Html) -> t (Widget a b) -> Widget a b
multiWidget f = foldr1 (bothWidget f)


isoW :: Iso' a b -> Widget' a -> Widget' b
isoW i = dimapWidget (review i) (view i)

-- | Turn a widget of a part type into a widget of an alternative type using a prism.
--
-- The resulting widget will render the part of the value selected by the prism if it
-- is there, otherwise it renders the given default HTML. The default rendering can
-- optionally use the provided sink to initiate a new value (typically some kind of
-- default), in which case this value immediately replaces the previous value.
--
-- @
-- possW _Just :: Widget' a -> Widget' (Maybe a)
-- possW _Left :: Widget' a -> Widget' (Either a b)
-- @
possW :: (Sink a -> Html) -> Prism' s a -> Widget' a -> Widget' s
possW z p w o i = case preview p i of
  Nothing -> z (contramapSink (review p) o)
  Just x  -> w (contramapSink (review p) o) x

maybeW :: Html -> Widget' a -> Widget' (Maybe a)
maybeW z = possW (const z) Control.Lens._Just

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


longStringWidget :: JSString -> Widget' JSString
longStringWidget title update value = div
  [ class_ "form-group" ]
  [ label [] [text title]
  , input
    [ A.type_ "search"
    -- TODO size
    , A.class_ "form-control"
    , A.value value
    , change  $ contramapSink Ev.value update
    , keyup $ contramapSink Ev.value update
    ] []
  ]

-- | Modify a widget to accept 'Maybe' and displays the text nothing on 'Nothing'.
altW :: Html -> Widget a b -> Widget (Maybe a) b
altW alt w s Nothing  = alt
altW alt w s (Just x) = w s x
