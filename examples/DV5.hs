
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, OverloadedStrings, TupleSections, ScopedTypeVariables, TypeFamilies
  , FlexibleContexts #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import Data.Monoid ((<>))
import GHCJS.Types(JSString, jsval)
import qualified Web.VirtualDom.Html as H
import qualified Web.VirtualDom.Svg.Events as SVG
import qualified Data.JSString
import qualified Data.List
import qualified Data.Ord
import System.Random (mkStdGen, randoms, split)
import Control.Lens (view)
import Control.Lens.Operators
import Control.Applicative

import Lubeck.FRP
import Lubeck.App (Html, runAppReactive)
import Lubeck.Forms
import Lubeck.Forms.Basic
import Lubeck.Forms.Button
import Lubeck.Forms.Select
import Lubeck.Drawing
import Lubeck.Util(showJS, parseDateAndTimeToUTC)
import qualified Lubeck.Drawing
import Data.Colour (withOpacity)
import qualified Data.Colour.Names as Colors


-- MAIN
{-
Basic GUI examples:
-}

-- | FRP routing monad. Should be MonadIO but not IO. Could be more specific, i.e. separating polling/sending etc a la reflex.
contramap = contramapSink

-- | Put the constant value in the sink whatever is put into resulting sink (often @()@).
(>$) :: b -> Sink b -> Sink a
(>$) = contramap . const

(>$<) :: (a -> b) -> Sink b -> Sink a
(>$<) = contramap

(>$$<) :: Sink b -> (a -> b) -> Sink a
(>$$<) = flip contramap

type WidgetD a b = WidgetT Drawing a b

data Void

-- Like Affine/AffineSpace except the diff type has no negate
class Monoid (Diff p) => Diffable p where
  type Diff p :: *
  diff :: p -> p -> Diff p
  patch :: p -> Diff p -> p
-- TODO scalar instances
-- TODO:
-- AffineSpace p => AffineSpace (a -> p)
-- (AffineSpace p, AffineSpace q) => AffineSpace (p, q)
-- (AffineSpace p, AffineSpace q, AffineSpace r) => AffineSpace (p, q, r)




-- A facet is much like a widget or component.
type Facet   r a b = (Signal r, Signal b, Sink a)
type FacetO  r b   = (Signal r, Signal b)

facet :: a -> WidgetT r a (a -> a) -> FRP (Facet r (a -> a) a)
facet initialState widget = do
  (internalSink, internalEvents) <- newEvent
  aS              <- accumS initialState internalEvents
  let htmlS       = fmap (widget internalSink) aS
  return (htmlS, aS, internalSink)

facet2  :: (b -> a -> a) -> (c -> a -> a)
        -> a -> WidgetT r a c
        -> FRP (Facet r b a)
facet2 f g z w = do
  (v,o,i) <- facet z (rmapWidget g w)
  return (v,o,contramap f i)
-- Note facet and facet2 can be defined in terms of each other!

facetOutputOnly :: Monoid a => WidgetT r a void -> FRP (FacetO r a)
facetOutputOnly w = dropInput <$> facet2 (flip const) (flip const) mempty w
  where
    dropInput (v,o,i) = (v,o)

facetOutputOnlyWM :: Monoid a => (MouseEv -> a -> a) -> WidgetD a void -> FRP (FacetO Drawing a)
facetOutputOnlyWM f w = dropInput <$> facetI addMouseInteraction <$> facet2 f (flip const) mempty w
  where
    dropInput (v,o,i) = (v,o)

    addMouseInteraction :: Sink MouseEv -> Drawing -> Drawing
    addMouseInteraction s dr = id
      $ addProperty (SVG.onMouseUp   $ Up   >$ s)
      $ addProperty (SVG.onMouseDown $ Down >$ s)
      $ addProperty (SVG.onMouseOver $ Over >$ s)
      $ addProperty (SVG.onMouseOut  $ Out  >$ s)
      $ addProperty (SVG.onMouseMove $ Move >$ s)
      $ dr

-- | Lift a widget that visualizes mouse state to a mouse-state facet.
facetOutputOnlyWM_ :: WidgetD MouseState void -> FRP (FacetO Drawing MouseState)
facetOutputOnlyWM_ = facetOutputOnlyWM (flip patch)

-- Like a standard facet

-- | Gives a facet access to its input.
facetI :: (Sink a -> r -> r) -> Facet r a b -> Facet r a b
facetI f (v,o,i) = (fmap (f i) v,o,i)

facetI2 :: (c -> a) -> (Sink c -> r -> r) -> Facet r a b -> Facet r a b
facetI2 g f = facetI (f . contramap g)



-- Drawing-based GUI
data MouseEv = None | Up | Down | Over | Out | Move deriving (Enum, Eq, Ord, Show, Read)
instance Monoid MouseEv where
  mempty = None
  mappend x y = x -- last event to happen as
data MouseState = MouseState { mouseInside :: Bool, mouseDown :: Bool } deriving (Eq, Ord, Show, Read)
instance Monoid MouseState where
  mempty = MouseState False False -- how do we know this?
  mappend x y = x -- ?
instance Diffable MouseState where
  type Diff MouseState = MouseEv
  diff _ _ = error "TODO MouseState.diff"
  patch (MouseState inside down) Up   = MouseState inside False
  patch (MouseState inside down) Down = MouseState inside True
  patch (MouseState inside down) Over = MouseState True   down
  -- If mouse goes while button is still pressed, release
  patch (MouseState inside down) Out  = MouseState False  False
  patch x _ = x



-- facet2  :: (b -> a -> a) -> (c -> a -> a)
--         -> a -> WidgetT r a c
--         -> FRP (Facet r b a)
bangF :: FRP (FacetO Drawing MouseState)
bangF = facetOutputOnlyWM_ bangW
  where
    bangW :: WidgetD MouseState Void
    bangW _ (MouseState inside down) = mconcat [ci,sq]
      where
        sq = fillColor Colors.grey   square
        ci = fillColor circleCol circle
        circleCol
          | down      = Colors.red
          | inside    = Colors.orange
          | otherwise = Colors.yellow


-- TODO Higher order: dynamically add bang objects to view and broadcast clicks to
-- a central location (with unique ids or similar)
-- TODO make these bang objects draggable/deleteable

-- TODO keyboard
-- TODO score/DAW-style region editor
-- TODO other basic max stuff

-- clickableDW :: WidgetT' Drawing ()
-- clickableDW outp () = mconcat [ci,sq]
--   where
--     sq = fillColor Colors.grey   square
--     ci = fillColor Colors.yellow circle
-- clickableF :: FRP (FacetO Drawing ())
-- clickableF = facetOutputOnly clickableDW

-- How do we make a basic hover component?
-- How do we implement a timely flash, a la "bang" in Max?
-- Do we need Monad Signal here? (that would give us Monad FacetO)




gui :: FRP (Signal Drawing)
gui = do
  (v1,o1) <- bangF
  (v2,o2) <- bangF
  subscribeEvent (updates (liftA2 (,) o1 o2)) print
  return $ (<> smokeBackground) <$> scale 1.88 <$> liftA2 (|||) v1 v2
  -- return mempty

main = do
  useDarkBackground
  (x :: Signal Html) <- fmap (fmap (toSvg drawOpts . scale 200)) $ gui
  runAppReactive x
  where
    drawOpts = mempty { dimensions = P (V2 400 400)}

foreign import javascript unsafe "document.body.style.backgroundColor = 'darkgrey'"
  useDarkBackground :: IO ()
