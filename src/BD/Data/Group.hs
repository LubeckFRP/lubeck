{-# LANGUAGE OverloadedStrings          #-}

module BD.Data.Group
    ( Member(..)
    , Group(..)
    , GroupsList(..)
    , loadGroups
    , loadGroup
    ) where

import           Control.Monad
import qualified Control.Monad.Parallel as MP
import           Data.Monoid
import qualified Data.Set                  as Set
import           Data.Bifunctor (bimap, first)

import           GHCJS.Types      (JSString)

import           BD.Api
import           BD.Types


type Member = JSString

data Group = Group { name :: JSString
                   , members :: Set.Set Member }

type GroupsList = [Group]

loadGroups :: IO [Either AppError Group]
loadGroups = do
  groupnames <- loadGroupsNames
  case groupnames of
    Left e   -> return [Left e]
    Right xs -> MP.mapM loadGroup xs

loadGroup :: JSString -> IO (Either AppError Group)
loadGroup gn = do
  ms <- loadGroupMembers gn
  case ms of
    Left e   -> return $ Left e
    Right ms -> return $ Right $ Group gn (Set.fromList ms)

loadGroupsNames :: IO (Either AppError [JSString])
loadGroupsNames = getAPIEither BD.Api.internalAPI "account-groups" >>= return . bimap ApiError id

loadGroupMembers :: JSString -> IO (Either AppError [JSString])
loadGroupMembers groupname = getAPIEither BD.Api.internalAPI ("account-groups/" <> groupname <> "/accounts") >>= return . bimap ApiError id
