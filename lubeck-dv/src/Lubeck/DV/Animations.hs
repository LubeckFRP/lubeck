
module Lubeck.DV.Animation
  ()
where

import Control.Monad.Identity
import Control.Monad.Reader
import Data.Time (DiffTime)

type AnimatedT m a = ReaderT DiffTime m a
