
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, CPP #-}

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

#ifdef __GHCJS__
import GHCJS.Types(JSString, IsJSVal(..), jsval)
import Data.JSString(pack, unpack)
import Lubeck.Util(showJS)
#endif

import qualified Data.Maybe
import qualified Data.Map
import Data.Map (Map)

import Data.Monoid
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar(TVar, newTVarIO, readTVar, modifyTVar)
import System.IO.Unsafe(unsafePerformIO)

#ifdef __GHCJS__
type Str = JSString
toStr = showJS
#else
type Str = String
toStr = show
#endif


data History = History
  { captureS :: Sink Moment
  , restoreS :: Sink Moment
  , captureE :: Events Moment
  , restoreE :: Events Moment
  }

newtype Moment = Moment Str
#ifdef __GHCJS__
  deriving (IsJSVal)
#endif

#ifdef __GHCJS__
-- These are in https://github.com/ghcjs/ghcjs-base/blob/master/Data/JSString.hs#L189
-- TODO are we behind this version?
instance Eq Moment where
  Moment x == Moment y  =  unpack x == unpack y
instance Ord Moment where
  Moment x <= Moment y  =  unpack x <= unpack y
#else
instance Eq Moment where
  Moment x == Moment y  =  x == y
instance Ord Moment where
  Moment x <= Moment y  =  x <= y

#endif

-- | Create a new 'History'.
newHistory :: IO History
newHistory = do
  (cs, ce) <- newEvent
  (rs, re) <- newEvent
  return $ History cs rs ce re

-- | Samples the given behavior whenever 'capture' is called and sends
-- an update on the returned event whenever 'restore' is called.
chronicle  :: History -> Behavior a -> IO (Events a)
chronicle h b = do
  let captures = snapshot b (captureE h) -- :: (Events (a, Moment))
  values <- accumB mempty (fmap (\(value, moment) -> Data.Map.insert moment value) captures) -- :: IO (Behavior (Map Moment a))
  return $ filterJust $ snapshotWith (\values moment -> Data.Map.lookup moment values) values (restoreE h)

-- | Samples the given signal whenever 'capture' is called and sends an update
-- on the returned signal whenever 'restore' is called. Otherwise the returned signal
-- behaves like the given signal.
--
-- Note that the order in which 'restore' reverts the signals created using 'chronicleS' is undefined.
--
chronicleS :: History -> Signal a -> IO (Signal a)
chronicleS h s = do
  initialValue <- pollBehavior (current s)
  restoreE <- chronicle h (current s)
  stepperS initialValue (restoreE <> updates s)

-- | Capture the current value of all chronicled behaviors and signals in the history.
capture :: History -> IO Moment
capture h = do
  m <- nextMoment
  captureS h m
  return m
  where
    moments_ = unsafePerformIO $ newTVarIO 0
    nextMoment :: IO Moment
    nextMoment = do
      atomically $ modifyTVar moments_ succ
      atomically $ fmap (Moment . toStr) $ readTVar moments_

-- | Restore the given moment of all chronicled behaviors and signals in the history.
restore :: History -> Moment -> IO ()
restore h m = restoreS h m
