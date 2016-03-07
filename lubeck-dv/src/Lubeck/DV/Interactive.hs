
module Lubeck.DV.Interactive
  (
    MouseState
  , CurrentZoom
  , InteractiveT(..)
  )
where

import Control.Monad.Identity
import Control.Monad.Reader

-- Not going to work because of reader collissions
--
-- We should use (Signal/Behavior Drawing) w.o. event handlers but with time
-- or a monadic generalization of this
--
-- See https://github.com/BeautifulDestinations/lubeck/issues/30

type MouseState = ()
type CurrentZoom = ()

type InteractiveT m a = ReaderT MouseState m a
