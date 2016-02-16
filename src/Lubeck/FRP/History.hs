
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Lubeck.FRP.History
    ( History
    -- ** Creating History objects
    , newHistory
    -- ** Tracking state
    , chronicle
    , chronicleS
    -- * Capturing and restoring moments in history
    , Moment
    , capture
    , restore
    ) where

import Lubeck.FRP
import GHCJS.Types(JSString, IsJSVal(..), jsval)
import Data.JSString(pack, unpack)
import qualified Data.Maybe
import qualified Data.Map
import Data.Map (Map)

newtype History = History ()

newtype Moment = Moment JSString
  deriving (IsJSVal)

-- TODO efficient compare
-- Why is JSString not an Eq instance?
instance Eq Moment where
  Moment x == Moment y  =  unpack x == unpack y

-- | Create a new 'History'.
newHistory :: IO History
newHistory = undefined
-- (capture :: Sink/Event Moment, restore :: Sink/Event Moment)

-- | Samples the given behaviorwhenever 'capture' is called and sends
-- an update on the returned event whenever 'restore' is called.
chronicle  :: History -> Behavior a -> IO (Events a)
chronicle = undefined
-- Create event that snapshots given B on H.capture :: Event (a, Moment)
-- Accumulate behavior :: B (Map Moment a)
-- Create event that looks up map :: E (Maybe a)
-- Scatter :: E a

-- | Samples the given beh/signal whenever 'capture' is called and sends an update
-- on the returned signal whenever 'restore' is called. Otherwise the returned signal
-- behaves like the given signal.
--
-- TODO beware of propagation order
--
chronicleS :: History -> Signal a -> IO (Signal a)
chronicleS = undefined
-- chronicle on (current S)
-- Step from (pollB $ current S) to chronicle result

-- | Capture the current value of all chronicled behaviors and signals in the history.
capture       :: History -> IO Moment
capture = undefined
-- Create new Moment
-- Send capture event

-- | Restore the given moment of all chronicled behaviors and signals in the history.
restore    :: Moment -> IO ()
restore = undefined
-- Send restore event
