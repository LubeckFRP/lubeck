{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module Components.BusyIndicator
  ( busyIndicatorComponent
  , BusyCmd(..)
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import qualified Data.List
import           Data.Monoid
import           Data.String                    (fromString)
import           GHCJS.Types                    (JSString)

import           Web.VirtualDom.Html            (Property, br, button, div,
                                                 form, h1, hr, img, p, table,
                                                 tbody, td, text, th, thead, tr)
import qualified Web.VirtualDom.Html            as E
import           Web.VirtualDom.Html.Attributes (class_)
import qualified Web.VirtualDom.Html.Attributes as A
import           Web.VirtualDom.Html.Events     (change, click, preventDefault,
                                                 stopPropagation, submit, value)

import           Lubeck.App                     (Html)
import           Lubeck.Forms
import           Lubeck.FRP

import           BD.Types


row6H content = div [class_ "row busy-indicator"] [ div [class_ "col-md-6 col-lg-4 col-md-offset-3 col-lg-offset-4"] [content] ]
infoPanel content = row6H $ div [class_ "alert alert-info text-center "] [content]

data BusyCmd = PushBusy | PopBusy deriving (Show)
type BusyStack = [Bool]

showJS :: Show a => a -> JSString
showJS = fromString . show

busyW :: Widget' BusyStack
busyW _ [] = mempty
busyW _ bs = infoPanel $ div [] [text $ "Working... (" <> showJS (length bs) <> ")"]

-- | Hopefully a reusable busy indicator component.
-- It is initialized with initial stack of busy tasks,
-- and returns a signal of html and a sink
-- to send push or pop busy tasks to.
--
-- It will keep showing busy indicator until busy stack will be empty.
busyIndicatorComponent :: BusyStack -> IO (Signal Html, Sink BusyCmd)
busyIndicatorComponent initialBusyStack = do
  (externalSink :: Sink BusyCmd, externalEvents :: Events BusyCmd) <- newEvent

  let busyCmds = fmap applyBusyCmd externalEvents :: Events (BusyStack -> BusyStack)

  busyStackS              <- accumS initialBusyStack busyCmds :: IO (Signal BusyStack)
  let htmlS       = fmap (busyW emptySink) busyStackS

  return (htmlS, externalSink)

  where
    applyBusyCmd :: BusyCmd -> (BusyStack -> BusyStack)
    applyBusyCmd PushBusy s = s <> [True]
    applyBusyCmd PopBusy [] = [] -- XXX maybe fail here, popping empty busy stack must be a runtime or logical error
    applyBusyCmd PopBusy s  = tail s
