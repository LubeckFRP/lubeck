{-# LANGUAGE OverloadedStrings  #-}

module Components.Layout
  ( fullsizeLayout2
  , fullsizeLayout4
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import qualified Data.List
import qualified Data.Maybe
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
import           Lubeck.Util                    (newSyncEventOf)

import           BD.Types
import           BDPlatform.Types
import           BDPlatform.HTMLCombinators


data LayoutAction = SwitchView Int
  deriving (Show, Eq)

liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> (f a -> f b -> f c -> f d -> f e)
liftA4 f a b c d = f <$> a <*> b <*> c <*> d
liftA6 :: Applicative f => (a -> b -> c -> d -> e -> g -> h) -> f a -> f b -> f c -> f d -> f e -> f g -> f h
liftA6 f a b c d e g = f <$> a <*> b <*> c <*> d <*> e <*> g

tabsW :: [ViewTitle] -> Widget LayoutAction LayoutAction
tabsW vts sink action = mconcat
  [ toolbar' $ buttonGroup $ fmap tabButton (zip [0..] vts) ]
  where
    tabButton (idx, title) = button title (action == SwitchView idx) [Ev.click $ \e -> sink $ SwitchView idx]

layout2 tabs action toolbar v1 v2 =
  panel [ toolbar, body ]
  where
    body = case action of
             SwitchView 0 -> v1
             SwitchView 1 -> v2
             _            -> E.text "Select an option"

layout4 tabs action toolbar v1 v2 v3 v4 =
  panel [ toolbar, body ]
  where
    body = case action of
             SwitchView 0 -> v1
             SwitchView 1 -> v2
             SwitchView 2 -> v3
             SwitchView 3 -> v4
             _            -> E.text "Select an option"

type View      = Signal Html
type ViewTitle = JSString
type ViewSpec  = (ViewTitle, View)

fullsizeLayout2 :: Int -> ViewSpec -> ViewSpec -> IO View
fullsizeLayout2 z v1 v2 = do
  (layoutSink, layoutEvents) <- newSyncEventOf (undefined :: LayoutAction)
  layoutSig                  <- stepperS (SwitchView z) layoutEvents

  let tabs                   = [v1, v2]
  let tabsToolbarV           = fmap (tabsW (fmap fst tabs) layoutSink) layoutSig
  let view                   = liftA4 (layout2 tabs) layoutSig tabsToolbarV (snd v1) (snd v2)

  return view

fullsizeLayout4 :: Int -> ViewSpec -> ViewSpec -> ViewSpec -> ViewSpec -> IO View
fullsizeLayout4 z v1 v2 v3 v4 = do
  (layoutSink, layoutEvents) <- newSyncEventOf (undefined :: LayoutAction)
  layoutSig                  <- stepperS (SwitchView z) layoutEvents

  let tabs                   = [v1, v2, v3, v4]
  let tabsToolbarV           = fmap (tabsW (fmap fst tabs) layoutSink) layoutSig
  let view                   = liftA6 (layout4 tabs) layoutSig tabsToolbarV (snd v1) (snd v2) (snd v3) (snd v4)

  return view
