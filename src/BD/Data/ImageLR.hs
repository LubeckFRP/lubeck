{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BD.Data.ImageLR where

import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson.Types
import           Data.Data
import           Data.Monoid
import           Data.String      (fromString)
import           Data.Time.Clock  (UTCTime)
import qualified GHC.Generics     as GHC

import           GHCJS.Types      (JSString)

import           BD.Api
import           BD.Data.AdTypes
import           BD.Types

import           Data.Bifunctor   (first)

data Image = Image
 { id :: Int
 , img_url :: Text
 , filename :: Text
 , img_src :: Text
 , bd_shared :: Bool
 } deriving (GHC.Generic, Show, Eq, Ord)

instance ToJSON Image
instance FromJSON Image
