
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-|
Provides high-level way of constructing forms and other interfaces.

/Experimental/
-}
module Lubeck.Forms
  (
  -- * Widget type
    Widget
  , Widget'
  , WidgetT
  , WidgetT'

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
  , componentEvent
  , componentSink
  , componentRW
  , componentListen
  , componentR
  , componentW
  , formComponent
  , formComponentExtra1
  , formComponentExtra2
  , formWithValidationComponent
  , formWithValidationComponentExtra2

  -- ** Submit type
  , Submit(..)
  , submitValue
  , submits

  -- * Form helpers
  , longStringWidget


  ) where

import           Lubeck.FRP
import           Lubeck.Html                    (Html)
import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import           Data.Monoid                    ((<>))
import           GHCJS.Types                    (JSString, jsval)

import qualified Web.VirtualDom                 as VD
import           Web.VirtualDom.Html            (a, button, div, form, h1, hr,
                                                 img, input, label, p, table,
                                                 tbody, td, text, th, tr)
import qualified Web.VirtualDom.Html            as E
import           Web.VirtualDom.Html.Attributes (class_, href, src, src, target,
                                                 width, width)
import qualified Web.VirtualDom.Html.Attributes as A
import           Web.VirtualDom.Html.Events     (change, click, keyup,
                                                 preventDefault,
                                                 stopPropagation, submit, value)
import qualified Web.VirtualDom.Html.Events     as Ev

import           Control.Lens                   (Iso, Iso', Lens, Lens', Prism,
                                                 Prism', lens, over, preview,
                                                 review, set, under, view)
import qualified Control.Lens
import           Control.Lens.TH                (makeLenses)

import           BD.Types

{-|
Provides a way of:

- Rendering a value of some type @i@
- Sending updates of some type @o@ in response to user interaction.
-}
type Widget i o = WidgetT Html i o

{-|
Same as 'Widget', except output is generalized to types other than 'Html'.
-}
type WidgetT r i o  = Sink o -> i -> r

{-|
A variant of 'Widget' where input and output is the same.
-}
type Widget' a  = Widget a a

{-|
A variant of 'WidgetT' where input and output is the same.
-}
type WidgetT' r a   = WidgetT r a a


-- | Map over the input type of a widget.
lmapWidget :: (b -> a) -> WidgetT r a s -> WidgetT r b s
lmapWidget f w st ab = w st (f ab)

-- | Map over the output type of a widget.
rmapWidget :: (s -> t) -> WidgetT r a s -> WidgetT r a t
rmapWidget f w st ab = w (contramapSink f st) ab

-- | Map over both input and output of a widget.
dimapWidget :: (a -> b) -> (c -> d) -> WidgetT r b c -> WidgetT r a d
dimapWidget f g = lmapWidget f . rmapWidget g

-- | Map over the HTML rendering of a widget.
mapHtmlWidget :: (r -> r2) -> WidgetT r a b -> WidgetT r2 a b
mapHtmlWidget f w = \s -> f . w s
-- TODO renamve mapView or similar

-- | Turn a widget of a smaller type into a widget of a larger type using a lens.
--
-- The resulting widget will render part of the larger type that the lens selects.
-- It the resulting widget produces output, this will be used to transform the
-- value rendered by the larger widget. This makes this function especially
-- useful in conjunction with 'multiWidget'.
--
-- @
-- subWidget _1     :: Widget' a -> Widget' (a, b)
-- subWidget (at 1) :: Widget' (Just a) -> Widget' (Map Int a)
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


-- | "internal read-write" component.
-- Initialized with initial state and widget, returns signal of html,
-- sink to accept new values and events stream to put user events to and
-- eventually provide them upstream
--
-- This is a basic and full-featured component. All other are just wrappers
-- around this one.
--
-- Values sent into the sink will propagate through the output event stream.
componentRW :: a -> WidgetT r a a -> IO (Signal r, Events a, Sink a)
componentRW initialState widget = do
  (internalSink, internalEvents) <- newEvent
  aS              <- stepperS initialState internalEvents
  let htmlS       = fmap (widget internalSink) aS
  return (htmlS, internalEvents, internalSink)

-- TODO: 
-- generalComponent :: (b -> a -> a) -> (c -> a -> a) -> a -> WidgetT r a b -> Event c -> IO (Signal r, Signal a) 

componentEvent :: a -> WidgetT r a a -> Events a -> IO (Signal r, Events a)
componentEvent initState widget inputs = do
  (signal, outputs, inSink) <- componentRW initState widget
  subscribeEvent inputs inSink
  return (signal, outputs) 

componentSink :: a -> WidgetT r a a -> Sink a -> IO (Signal r, Sink a)
componentSink initState widget outputSink = do
  (htmlSignal, outputs, inSink) <- componentRW initState widget
  subscribeEvent outputs outputSink 
  return (htmlSignal, inSink) 

-- "internal read" component
-- Initialized with initial state and widget, returns signal of html and
-- events to read user events from.
componentR :: a -> Widget' a -> IO (Signal Html, Events a)
componentR initialState widget = do
  (htmlS, internalEvents, _) <- componentRW initialState widget
  return (htmlS, internalEvents)

-- | Alias for `componentR`
component = componentR

-- | "internal write" component.
-- Initialized with initial state and widget, returns signal of html and
-- sink to accept new values
componentW :: a -> Widget' a -> IO (Signal Html, Sink a)
componentW initialState widget = do
  (htmlS, _, internalSink) <- componentRW initialState widget
  return (htmlS, internalSink)

componentListen ::  WidgetT r a b -> Signal a -> Signal r 
componentListen widget signal = fmap (widget emptySink) signal 

repackValue :: (CanSubmit, Submit a) -> (CanSubmit, a)
repackValue (x, Submit y)     = (x, y)
repackValue (x, DontSubmit y) = (x, y)

formWithValidationComponent :: Validator a -> a -> Widget (CanSubmit, a) (Submit a) -> IO (Signal Html, Events a)
formWithValidationComponent validate z w = do
  (aSink, aEvent) <- newEvent                                                   :: IO (Sink (Submit a), Events (Submit a))

  let isValidE    = fmap (validate . submitValue) aEvent                     -- :: Events CanSubmit
  isValidS        <- stepperS (validate z) isValidE
  aEventS         <- stepperS (DontSubmit z) aEvent

  let aS          = liftA2 (,) isValidS aEventS                               -- :: IO (Signal (CanSubmit, Submit a))

  let htmlS = fmap (w aSink . repackValue) aS
  return (htmlS, submits aEvent)

-- | A variant of component that supports chanching its value internally without
-- sending on any updates.
--
-- Appropriate for forms with a \"Submit\" button.
formComponent :: a -> Widget a (Submit a) -> IO (Signal Html, Events a)
formComponent z w = do
  -- Value changed
  (aSink, aEvent) <- newEvent :: IO (Sink (Submit a), Events (Submit a))
  aS <- stepperS (DontSubmit z) aEvent
  let htmlS = fmap (w aSink . submitValue) aS
  return (htmlS, submits aEvent)

-- let's see if this is generalisable later
formComponentExtra1 :: Behavior b -> a -> Widget (b, a) (Submit a) -> IO (Signal Html, Events a)
formComponentExtra1 bB az w = do
  (aSink, aEvent) <- newEvent :: IO (Sink (Submit a), Events (Submit a))

  aS              <- stepperS (DontSubmit az) aEvent   -- :: IO (Signal (Submit a))
  let baS         = snapshotS bB (fmap submitValue aS) -- :: Signal (b, a)

  let htmlS       = fmap (w aSink) baS
  return (htmlS, submits aEvent)

-- let's see if this is generalisable later
formComponentExtra2 :: Behavior b
                    -> Behavior c
                    -> a
                    -> Widget (c, (b, a)) (Submit a)
                    -> IO (Signal Html, Events a)
formComponentExtra2 bB cB az w = do
  (aSink, aEvent) <- newEvent :: IO (Sink (Submit a), Events (Submit a))

  aS              <- stepperS (DontSubmit az) aEvent   -- :: IO (Signal (Submit a))
  let baS         = snapshotS bB (fmap submitValue aS) -- :: Signal (b, a)
  let bcaS        = snapshotS cB baS -- :: Signal (c, (b, a))

  let htmlS       = fmap (w aSink) bcaS
  return (htmlS, submits aEvent)

-- let's see if this is generalisable later
formWithValidationComponentExtra2 :: Behavior b
                                  -> Behavior c
                                  -> Validator a
                                  -> a
                                  -> Widget (CanSubmit, (c, (b, a))) (Submit a)
                                  -> IO (Signal Html, Events a)
formWithValidationComponentExtra2 bB cB validate az w = do
  (aSink, aEvent) <- newEvent :: IO (Sink (Submit a), Events (Submit a))

  let isValidE    = fmap (validate . submitValue) aEvent
  isValidS        <- stepperS (validate az) isValidE

  aS              <- stepperS (DontSubmit az) aEvent   -- :: IO (Signal (Submit a))
  let baS         = snapshotS bB (fmap submitValue aS) -- :: Signal (b, a)
  let bcaS        = snapshotS cB baS                   -- :: Signal (c, (b, a))

  let xS          = liftA2 (,) isValidS bcaS

  let htmlS       = fmap (w aSink) xS
  return (htmlS, submits aEvent)


-- | A helper type for 'formComponentExtra1.
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


longStringWidget :: JSString -> Bool -> Widget' JSString
longStringWidget title focus update value = div
  [ class_ "form-group" ]
  [ label [class_ "control-label col-xs-2"] [text title]
  , div [class_ "col-xs-10"]
      [ input
        ([ A.type_ "search"
        -- TODO size
        , A.class_ "form-control"
        , A.value value
        , change  $ contramapSink Ev.value update
        , keyup $ contramapSink Ev.value update
        ] <> fcs) []
      ]
  ]
  where
    fcs = if focus then [(VD.attribute "autofocus") "true"] else []

-- | Modify a widget to accept 'Maybe' and displays the text nothing on 'Nothing'.
altW :: Html -> Widget a b -> Widget (Maybe a) b
altW alt w s Nothing  = alt
altW alt w s (Just x) = w s x
