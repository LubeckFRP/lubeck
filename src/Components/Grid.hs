{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}


module Components.Grid
  ( gridComponent
  , GridCommand(..)
  , GridAction(..)
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           GHCJS.Concurrent               (synchronously)

import qualified Web.VirtualDom.Html            as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events     as Ev

import           Lubeck.App                     (Html)
import           Lubeck.Forms
import           Lubeck.FRP
import           Lubeck.Util

import           BD.Types


data GridAction a = Select [a] | Delete [a] | Other [a] deriving Show
data GridCommand a = Replace [a]

gridW :: (a -> Html) -> Widget [a] (GridAction a)
gridW _     _        []    = contentPanel $ E.text "No items"
gridW itemW gridSink items = contentPanel $ E.div []
  [ E.div [A.style "margin-left: -20px;"] (map (itemWrapperW itemW gridSink) items) ]

itemWrapperW :: (a -> Html) -> Widget a (GridAction a)
itemWrapperW itemW gridSink x =
  E.div [A.class_ "grid-item-wrapper"]
    [ E.div [A.class_ "grid-item-delete", Ev.click $ \_ -> gridSink $ Delete [x]] [E.text "[x]"]
    , E.div [A.class_ "grid-item-select", Ev.click $ \_ -> gridSink $ Select [x]] [E.text "[v]"]
    , E.div [A.class_ "grid-item-other" , Ev.click $ \_ -> gridSink $ Other  [x]] [E.text "[o]"]
    , itemW x
    ]

gridComponent :: [a] -> Widget a b -> IO (Signal Html, Sink (GridCommand a), Events (GridAction a), Events b)
gridComponent as itemW = do
  (itemSink', itemEvents)           <- newEventOf (undefined                     :: b)
  (actionsSink', actionsEvents)     <- newEventOf (undefined                     :: (GridAction a))
  (lifecycleSink', lifecycleEvents) <- newEventOf (undefined                     :: (GridCommand a))
  let itemSink                      = synchronously . itemSink'
  let actionsSink                   = synchronously . actionsSink'
  let lifecycleSink                 = synchronously . lifecycleSink'

  asS                               <- stepperS [] (fmap (\(Replace as) -> as) lifecycleEvents)
  let view                          = fmap (gridW (itemW itemSink) actionsSink) asS

  return (view, lifecycleSink, actionsEvents, itemEvents)
