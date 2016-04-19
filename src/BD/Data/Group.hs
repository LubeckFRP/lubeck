{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}

module BD.Data.Group
    ( Group(..)
    , GroupsList(..)
    , GroupName(..)
    , MemberName(..)
    , GroupsNamesList(..)
    , GroupMemberNamesList(..)
    , loadGroupsNames
    , loadGroup
    , addAccountsToGroup
    , removeAccountsFromGroup
    , deleteGroup
    , undeleteGroup
    , undeleteGroup'
    ) where

import           Control.Monad
import qualified Control.Monad.Parallel as MP
import           Data.Monoid
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Set                  as Set
import           Data.Bifunctor (bimap, first)

import           GHCJS.Types      (JSString)
import qualified GHC.Generics     as GHC

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

data AccountInGroupToggle = AccountInGroupToggle
  { status :: Int
  , group_name :: JSString
  , account_id :: Int
  } deriving (GHC.Generic)

instance ToJSON AccountInGroupToggle

data GroupExists = GroupExists
  { ge_group_name  :: JSString
  , ge_status      :: Int
  } deriving (GHC.Generic)

instance ToJSON GroupExists where
  toJSON = Data.Aeson.Types.genericToJSON
    Data.Aeson.Types.defaultOptions { Data.Aeson.Types.fieldLabelModifier = drop 3 }

groupExistsToggle :: GroupExists -> IO (Either AppError Ok)
groupExistsToggle x = postAPIEither BD.Api.internalAPI "events/group-exists" x >>= return . bimap ApiError id

deleteGroup grp        = groupExistsToggle $ GroupExists (name grp) 0
undeleteGroup grp      = groupExistsToggle $ GroupExists (name grp) 1
undeleteGroup' grpname = groupExistsToggle $ GroupExists grpname 1

addAccountsToGroup :: GroupName -> [Int] -> IO [Either AppError Ok]
addAccountsToGroup = toggleAccountsInGroup 1

toggleAccountsInGroup :: Int -> GroupName -> [Int] -> IO [Either AppError Ok]
toggleAccountsInGroup status grp = MP.mapM go
  where
    go a = do
      let payload = AccountInGroupToggle status grp a
      postAPIEither BD.Api.internalAPI "events/account-in-group" payload >>= return . bimap ApiError id

removeAccountsFromGroup :: GroupName -> [Int] -> IO [Either AppError Ok]
removeAccountsFromGroup = toggleAccountsInGroup 0

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
