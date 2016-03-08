
module Lubeck.DV.Animation
  (
    -- AnimatedT(..)
  )
where

import Control.Monad.Identity
import Control.Monad.Reader
import Data.Time (DiffTime)

-- Not going to work because of reader collissions
--
-- We should use (Signal/Behavior Drawing) w.o. event handlers but with time
-- or a monadic generalization of this
--
-- See https://github.com/BeautifulDestinations/lubeck/issues/30

-- type AnimatedT m a = ReaderT DiffTime m a
