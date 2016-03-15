{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}


module Components.Grid
  ( gridComponent
  , GridCommand(..)
  , GridAction(..)
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import qualified Data.Set as Set

import           GHCJS.Concurrent               (synchronously)

import qualified Web.VirtualDom.Html            as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events     as Ev

import           Lubeck.App                     (Html)
import           Lubeck.Forms
import           Lubeck.FRP
import           Lubeck.Util

import           BD.Types
import           BDPlatform.HTMLCombinators


data GridAction a = Select [a] | Delete [a] | Other [a] deriving Show
data GridCommand a = Replace [a]

gridW :: Ord a => (a -> Html) -> Widget ([a], Set.Set a) (GridAction a)
gridW _     _        ([], _)              = contentPanel $ E.text "No items"
gridW itemW gridSink (items, selectedSet) = contentPanel $ E.div []
  [ E.div [A.style "margin-left: -20px;"] (map (itemWrapperW itemW selectedSet gridSink) items) ]

itemWrapperW :: Ord a => (a -> Html) -> Set.Set a -> Widget a (GridAction a)
itemWrapperW itemW selectedSet gridSink x =
  let selIcon = if Set.member x selectedSet then "check-square" else "check-square-o"

  in E.div [A.class_ "grid-item-wrapper"]
      [ buttonIcon_ "btn-link grid-item-delete" "" "trash"          False [Ev.click $ \_ -> gridSink $ Delete [x]]
      , buttonIcon_ "btn-link grid-item-select" "" selIcon          False [Ev.click $ \_ -> gridSink $ Select [x]]
      , buttonIcon_ "btn-link grid-item-other"  "" "circle-o"       False [Ev.click $ \_ -> gridSink $ Other  [x]]
      , itemW x
      ]

gridComponent :: Ord a => [a] -> Widget a b -> IO (Signal Html, Sink (GridCommand a), Events (GridAction a), Events b)
gridComponent as itemW = do
  (itemSink', itemEvents)           <- newEventOf (undefined                     :: b)
  (actionsSink', actionsEvents)     <- newEventOf (undefined                     :: (GridAction a))
  (lifecycleSink', lifecycleEvents) <- newEventOf (undefined                     :: (GridCommand a))
  let itemSink                      = synchronously . itemSink'
  let actionsSink                   = synchronously . actionsSink'
  let lifecycleSink                 = synchronously . lifecycleSink'

  let selE                          = fmap commandToSelection actionsEvents -- :: Events (Set.Set a -> Set.Set a)
  selectedS                         <- accumS (Set.empty :: Set.Set a) selE

  asS                               <- stepperS [] (fmap (\(Replace as) -> as) lifecycleEvents)

  let asAndSelS                     = liftA2 (,) asS selectedS                 --  :: Signal ([a], Set.Set a)

  let view                          = fmap (gridW (itemW itemSink) actionsSink) asAndSelS

  return (view, lifecycleSink, actionsEvents, itemEvents) -- TODO need to return selectedS (or beh)

  where
    commandToSelection :: Ord a => GridAction a -> (Set.Set a -> Set.Set a)
    commandToSelection (Select y') x = let y = Set.fromList y' in
                                       if Set.null (Set.intersection x y)
                                         then Set.union x y
                                         else Set.union (Set.difference x y) (Set.difference y x)
    commandToSelection _           x = x
