{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module Components.BusyIndicator
  ( busyIndicatorComponent
  , BusyCmd(..)
  , withBusy
  , withBusy2
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import qualified Data.List
import           Data.Monoid


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
import           Lib.Helpers


data BusyCmd = PushBusy | PopBusy deriving (Show)
type BusyStack = [Bool] -- can be Int, for example, but the idea is to save some additional info about busy actions later

-- FIXME what about lazyness etc?
withBusy sink f = \x -> do
  sink PushBusy
  y <- f x
  sink PopBusy
  return y

withBusy2 sink f = \x y -> do
  sink PushBusy
  z <- f x y
  sink PopBusy
  return z


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

  busyStackS   <- accumS initialBusyStack busyCmds :: IO (Signal BusyStack)
  let htmlS    = fmap (busyW emptySink) busyStackS

  return (htmlS, externalSink)

  where
    applyBusyCmd :: BusyCmd -> (BusyStack -> BusyStack)
    applyBusyCmd PushBusy s = s <> [True]
    applyBusyCmd PopBusy [] = [] -- XXX maybe fail here, popping empty busy stack must be a runtime or logical error
    applyBusyCmd PopBusy s  = tail s
