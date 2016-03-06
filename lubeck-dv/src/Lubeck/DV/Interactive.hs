
module Lubeck.DV.Interactive
  (
    MouseState
  , InteractiveT(..)
  )
where

import Control.Monad.Identity
import Control.Monad.Reader

type MouseState = ()

type InteractiveT m a = ReaderT MouseState m a
