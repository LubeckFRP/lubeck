{-# LANGUAGE OverloadedStrings  #-}

module Components.Layout
  ( fullsizeLayout2
  , fullsizeLayout2'
  , fullsizeLayout4
  , verticalStackLayout2
  , horizontalStackLayout2
  , toggleLayout2
  , overlayLayout
  , overlayLayout2
  , Layout(..)
  , view
  , name

  , mkLayoutFullsize2
  , mkLayoutFullsize4
  , mkLayoutVerticalStack
  , mkLayoutHorizontalStack
  , mkLayoutToggle
  , mkLayoutOverlay
  , mkLayoutPure
  , mkLayoutPure'

  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import qualified Data.List
import           Data.Maybe
import           Data.Monoid

import qualified Data.JSString
import           GHCJS.Types                    (JSString)

import qualified Web.VirtualDom.Html            as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events     as Ev

import           Lubeck.App                     (Html)
import           Lubeck.Forms
import           Lubeck.Types
import           Lubeck.FRP
import           Lubeck.Util                    (newSyncEventOf, showJS)

import           BD.Types
import           BDPlatform.Types
import           BDPlatform.HTMLCombinators


data LayoutAction = SwitchView Int
  deriving (Show, Eq)


type View     = Signal Html
type ViewName = JSString
data LayoutSpec a = LayoutSpec { _lView :: View
                               , _lActiveView :: Maybe (Signal a)
                               , _lName :: Maybe ViewName
                               }

data Layout = LayoutFullsize2 (LayoutSpec Int)
            | LayoutFullsize4 (LayoutSpec Int)
            | LayoutVerticalStack (LayoutSpec Int)
            | LayoutHorizontalStack (LayoutSpec Int)
            | LayoutToggle (LayoutSpec Int) -- TODO better name
            | LayoutOverlay (LayoutSpec Bool)
            | LayoutOverlay2 (LayoutSpec Int)
            | LayoutPure (LayoutSpec ())
-- TODO instances

mkLayoutFullsize2 a b c       = LayoutFullsize2 $ LayoutSpec a b c
mkLayoutFullsize4 a b c       = LayoutFullsize4 $ LayoutSpec a b c
mkLayoutVerticalStack a b c   = LayoutVerticalStack $ LayoutSpec a b c
mkLayoutHorizontalStack a b c = LayoutHorizontalStack $ LayoutSpec a b c
mkLayoutToggle a b c          = LayoutToggle $ LayoutSpec a b c
mkLayoutOverlay a b c         = LayoutOverlay $ LayoutSpec a b c
mkLayoutOverlay2 a b c        = LayoutOverlay2 $ LayoutSpec a b c
mkLayoutPure a                = LayoutPure $ LayoutSpec a Nothing Nothing
mkLayoutPure' a b             = LayoutPure $ LayoutSpec a Nothing (Just b)

view :: Layout -> View
view (LayoutFullsize2 x)       = _lView x
view (LayoutFullsize4 x)       = _lView x
view (LayoutVerticalStack x)   = _lView x
view (LayoutHorizontalStack x) = _lView x
view (LayoutToggle x)          = _lView x
view (LayoutOverlay x)         = _lView x
view (LayoutOverlay2 x)        = _lView x
view (LayoutPure x)            = _lView x

name :: Layout -> Maybe ViewName
name (LayoutFullsize2 x)       = _lName x
name (LayoutFullsize4 x)       = _lName x
name (LayoutVerticalStack x)   = _lName x
name (LayoutHorizontalStack x) = _lName x
name (LayoutToggle x)          = _lName x
name (LayoutOverlay x)         = _lName x
name (LayoutOverlay2 x)        = _lName x
name (LayoutPure x)            = _lName x

liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> (f a -> f b -> f c -> f d -> f e)
liftA4 f a b c d = f <$> a <*> b <*> c <*> d
liftA6 :: Applicative f => (a -> b -> c -> d -> e -> g -> h) -> f a -> f b -> f c -> f d -> f e -> f g -> f h
liftA6 f a b c d e g = f <$> a <*> b <*> c <*> d <*> e <*> g

tabsW :: [Maybe ViewName] -> Widget LayoutAction LayoutAction
tabsW vts sink action = mconcat
  [ toolbar' $ buttonGroup $ fmap tabButton (zip [0..] vts) ]
  where
    tabButton (idx, title) = button (fromMaybe ("Tab " <> showJS idx) title) (action == SwitchView idx) [Ev.click $ \e -> sink $ SwitchView idx]

--------------------------------------------------------------------------------

layout2 tabs action toolbar l1 l2 =
  panel [ toolbar, body ]
  where
    body = case action of
             SwitchView 0 -> l1
             SwitchView 1 -> l2
             _            -> E.text "Select an option"

fullsizeLayout2' :: Signal Int -> Layout -> Layout -> IO (Layout, Signal Int)
fullsizeLayout2' idxS l1 l2 = do
  (layoutSink, layoutEvents) <- newSyncEventOf (undefined :: LayoutAction)
  z                          <- pollBehavior (current idxS)
  let externalSwitchS        = fmap SwitchView idxS
  let switchE                = layoutEvents `merge` (updates externalSwitchS)
  switchS                    <- stepperS (SwitchView z) switchE

  let tabs                   = [l1, l2]
  let tabsToolbarV           = fmap (tabsW (fmap name tabs) layoutSink) switchS
  let topV                   = liftA4 (layout2 tabs) switchS tabsToolbarV (view l1) (view l2)

  return ((mkLayoutFullsize2 topV (Just (fmap toIdx switchS)) Nothing), (fmap (\(SwitchView x) -> x) switchS))

fullsizeLayout2 :: Signal Int -> Layout -> Layout -> IO Layout
fullsizeLayout2 idxS l1 l2 = fullsizeLayout2' idxS l1 l2 >>= return . fst
--------------------------------------------------------------------------------

layout4 tabs action toolbar l1 l2 l3 l4 =
  panel [ toolbar, body ]
  where
    body = case action of
             SwitchView 0 -> l1
             SwitchView 1 -> l2
             SwitchView 2 -> l3
             SwitchView 3 -> l4
             _            -> E.text "Select an option"

fullsizeLayout4 :: Signal Int -> Layout -> Layout -> Layout -> Layout -> IO Layout
fullsizeLayout4 idxS l1 l2 l3 l4 = do
  (layoutSink, layoutEvents) <- newSyncEventOf (undefined :: LayoutAction)
  z                          <- pollBehavior (current idxS)
  let externalSwitchS        = fmap SwitchView idxS
  let switchE                = layoutEvents `merge` (updates externalSwitchS)
  switchS                    <- stepperS (SwitchView z) switchE

  let tabs                   = [l1, l2, l3, l4]
  let tabsToolbarV           = fmap (tabsW (fmap name tabs) layoutSink) switchS
  let topV                   = liftA6 (layout4 tabs) switchS tabsToolbarV (view l1) (view l2) (view l3) (view l4)

  return $ mkLayoutFullsize4 topV (Just (fmap toIdx switchS)) Nothing

toIdx (SwitchView n) = n

--------------------------------------------------------------------------------

layoutVStack l1 l2 = panel [l1, l2]

verticalStackLayout2 :: Layout -> Layout -> IO Layout
verticalStackLayout2 l1 l2 = do
  let topV = layoutVStack <$> (view l1) <*> (view l2)
  return $ mkLayoutVerticalStack topV Nothing Nothing

--------------------------------------------------------------------------------

layoutHStack l1 l2 = panel' . row $
  [ col1 [l1]
  , col2 [l2]
  ]
  where
    row x  = E.div [A.class_ "row"] x
    col1 x = E.div [A.class_ "col-xs-2"] x
    col2 x = E.div [A.class_ "col-xs-10"] x

horizontalStackLayout2 :: Layout -> Layout -> IO Layout
horizontalStackLayout2 l1 l2 = do
  let topV = layoutHStack <$> (view l1) <*> (view l2)
  return $ mkLayoutHorizontalStack topV Nothing Nothing

--------------------------------------------------------------------------------

layoutToggle2 z l1 l2 =
  panel' body
  where
    body = case z of
             0 -> l1
             1 -> l2
             _ -> E.text "Select an option"

toggleLayout2 :: Signal Int -> Layout -> Layout -> IO Layout
toggleLayout2 z l1 l2 = do
  let topV = layoutToggle2 <$> z <*> (view l1) <*> (view l2)
  return $ mkLayoutToggle topV (Just z) Nothing

--------------------------------------------------------------------------------

layoutPopup z l1 l2 = panel body
  where
    body = if z then [l1, l2] else [l1]

overlayLayout :: Signal Bool -> Layout -> Layout -> IO Layout
overlayLayout z l1 l2 = do
  let topV = layoutPopup <$> z <*> (view l1) <*> (view l2)
  return $ mkLayoutOverlay topV (Just z) Nothing

--------------------------------------------------------------------------------

layoutPopup2 z l1 l2 l3 = panel body
  where
    body = case z of
      0 -> [l1]
      1 -> [l1, l2]
      2 -> [l1, l3]
      _ -> [l1]

overlayLayout2 :: Signal Int -> Layout -> Layout -> Layout -> IO Layout
overlayLayout2 z l1 l2 l3 = do
  let topV = layoutPopup2 <$> z <*> (view l1) <*> (view l2) <*> (view l3)
  return $ mkLayoutOverlay2 topV (Just z) Nothing
