
module Lubeck.FRP.Moment
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
import qualified Data.Maybe
import qualified Data.Map
import Data.Map (Map)

data History
data Moment (Eq, IsJSVal)

-- | Create a new 'History'.
newHistory :: IO History
newHistory = undefined

-- | Samples the given behaviorwhenever capture' is called and sends
-- an update on the returned event whenever ’restore' is called.
chronicle  :: History -> Behavior a -> IO (Events a)
chronicle = undefined

-- | Samples the given beh/signal whenever ’Moment' is called and sends an update
-- on the returned signal whenever ’restore' is called. Otherwise the returned signal
-- behaves like the given signal.
chronicleS :: History -> Signal a -> IO (Signal a)
chronicleS = undefined

-- | Capture the current value of all chronicled behaviors and signals in the history.
capture       :: History -> IO Moment
capture = undefined

-- | Restore the given moment of all chronicled behaviors and signals in the history.
restore    :: Moment -> IO ()
restore = undefined
