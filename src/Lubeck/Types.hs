{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lubeck.Types
  (
  -- * Notifications
    Notification(..)
  , apiError
  , blError
  , notImplError
  -- * Forms
  , FormValid
  , Validator
  - ** IO
  , FormValidIO
  , ValidatorIO
  ) where

import           Control.Monad
import qualified Data.Aeson.Types
import           Data.Data
import           Data.Monoid
import qualified GHC.Generics     as GHC

import           GHCJS.Types      (JSString)
import           BD.Types


data Notification = NError AppError | NInfo JSString | NWarning JSString | NSuccess JSString

apiError     = NError . ApiError
blError      = NError . BLError
notImplError = NError . NotImplementedError

data FormValid e = FormValid | FormNotValid e
type Validator a e = a -> FormValid e

data FormValidIO a = FormValidIO | FormNotValidIO a
type ValidatorIO a e = a -> IO (FormValidIO e)
