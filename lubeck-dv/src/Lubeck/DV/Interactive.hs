
module Lubeck.DV.Interactive
  (
    MouseState
  , CurrentZoom
  , InteractiveT(..)
  )
where

import Control.Monad.Identity
import Control.Monad.Reader

type MouseState = ()
type CurrentZoom = ()

type InteractiveT m a = ReaderT MouseState m a
