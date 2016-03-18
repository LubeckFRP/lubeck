{-# LANGUAGE OverloadedStrings          #-}

module BD.Data.Group
    ( Group(..)
    , GroupsList(..)
    , GroupName(..)
    , MemberName(..)
    , GroupsNamesList(..)
    , GroupMemberNamesList(..)
    , loadGroupsNames
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
import qualified BD.Data.Account                as Ac


data Group = Group { name    :: GroupName
                   , members :: Set.Set Ac.Account }

type GroupsList = [Group]

type GroupName            = JSString
type MemberName           = JSString
type GroupsNamesList       = [GroupName]
type GroupMemberNamesList = [MemberName]

loadGroupsNames :: IO (Either AppError GroupsNamesList)
loadGroupsNames = getAPIEither BD.Api.internalAPI "account-groups" >>= return . bimap ApiError id

loadGroupMemberNames :: JSString -> IO (Either AppError GroupMemberNamesList)
loadGroupMemberNames groupname = getAPIEither BD.Api.internalAPI ("account-groups/" <> groupname <> "/accounts") >>= return . bimap ApiError id

loadGroupAccounts :: GroupMemberNamesList -> IO [Either AppError Ac.Account]
loadGroupAccounts = MP.mapM Ac.getUserOrError

loadGroup :: GroupName -> IO (Group, [AppError])
loadGroup groupname = do
  res <- loadGroupMemberNames groupname
  res' <- case res of
            Left e     -> return [Left e]
            Right accs -> loadGroupAccounts accs

  return (group groupname res', errors res')

  where
    group n r    = Group n (Set.fromList $ accounts r)

    accounts :: [Either AppError Ac.Account] -> [Ac.Account]
    accounts r   = fromRight <$> filter (not . matchError) r

    errors :: [Either AppError Ac.Account] -> [AppError]
    errors r     = fromLeft <$> filter matchError r

    fromLeft  (Left  x) = x
    fromRight (Right x) = x

    matchError x = case x of
                     (Left _) -> True
                     _        -> False
