
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Lubeck.FRP.History
    ( History
    -- ** Creating History objects
    , newHistory
    -- ** Tracking state
    , chronicle
    , chronicleS
    -- * Capturing and restoring moments in history
    , Moment(..)
    , capture
    , restore
    ) where

import Lubeck.FRP
import GHCJS.Types(JSString, IsJSVal(..), jsval)
import Data.JSString(pack, unpack)
import qualified Data.Maybe
import qualified Data.Map
import Data.Map (Map)

import Data.Monoid
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar(TVar, newTVarIO, readTVar, modifyTVar)
import System.IO.Unsafe(unsafePerformIO)
import Lubeck.Util(showJS)

data History = History
  { captureS :: Sink Moment
  , restoreS :: Sink Moment
  , captureE :: Events Moment
  , restoreE :: Events Moment
  }

newtype Moment = Moment JSString
  deriving (IsJSVal)

-- These are in https://github.com/ghcjs/ghcjs-base/blob/master/Data/JSString.hs#L189
-- TODO are we behind this version?
instance Eq Moment where
  Moment x == Moment y  =  unpack x == unpack y
instance Ord Moment where
  Moment x <= Moment y  =  unpack x <= unpack y

-- | Create a new 'History'.
newHistory :: IO History
newHistory = do
  (cs, ce) <- newEvent
  (rs, re) <- newEvent
  return $ History cs rs ce re

-- | Samples the given behaviorwhenever 'capture' is called and sends
-- an update on the returned event whenever 'restore' is called.
chronicle  :: History -> Behavior a -> IO (Events a)
chronicle h b = do
  let captures = snapshot b (captureE h) -- :: (Events (a, Moment))
  values <- accumB mempty (fmap (\(value, moment) -> Data.Map.insert moment value) captures) -- :: IO (Behavior (Map Moment a))

  -- TODO this line crashes!
  return $ filterJust $ snapshotWith (\values moment -> Data.Map.lookup moment values) values (restoreE h)

  -- return $ filterJust $
    -- snapshotWith (\values moment -> Just undefined) values (restoreE h)

  -- return (fmap undefined $ restoreE h)

-- | Samples the given beh/signal whenever 'capture' is called and sends an update
-- on the returned signal whenever 'restore' is called. Otherwise the returned signal
-- behaves like the given signal.
--
-- TODO beware of propagation order
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
      atomically $ fmap (Moment . showJS) $ readTVar moments_

-- | Restore the given moment of all chronicled behaviors and signals in the history.
restore :: History -> Moment -> IO ()
restore h m = restoreS h m
