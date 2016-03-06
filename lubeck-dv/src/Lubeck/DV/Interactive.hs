
module Lubeck.DV.Interactive
  ()
where

import Control.Monad.Identity
import Control.Monad.Reader

type MouseState = ()

type AnimatedT m a = ReaderT MouseState m a
