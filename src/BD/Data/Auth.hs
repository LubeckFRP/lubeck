{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE JavaScriptFFI              #-}

module BD.Data.Auth
    ( AuthInfo (..)
    , AuthSession (..)
    , ChangePasswordForm (..)
    , CreateUserForm (..)
    , authenticateOrError
    , createUserOrError
    , changePasswordOrError
    ) where

import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson.Types
import           Data.Aeson.Types
import           Data.Data
import qualified Data.HashMap.Strict       as H
import           Data.Bifunctor (bimap, first)
import           Data.Foldable (asum)
import           Data.Monoid
import           Data.String      (fromString)
import           Data.Time.Clock  (UTCTime)
import qualified GHC.Generics     as GHC

import           GHCJS.Types      (JSString)

import           BD.Api
import           BD.Types

import           Lubeck.FRP


data AuthInfo = AuthInfo JSString AuthSession | NoAuthToken JSString deriving (GHC.Generic, Show, Eq)

-- account_id: 435201892
-- expires_at: "2016-03-13T13:58:20.460Z"
-- is_admin: false
-- token: "47812c49-4580-4544-9bc7-10c66167996e"
-- username: "forbestravelguide"
data AuthSession = AuthSession { account_id :: Int
                               , expires_at :: JSString -- TODO parse date
                               , is_admin   :: Bool
                               , token      :: JSString
                               , username   :: JSString
                               } deriving (GHC.Generic, Show, Eq)

instance FromJSON AuthSession

data ChangePasswordForm = ChangePasswordForm { oldpassword :: JSString
                                             , newpassword :: JSString } deriving (GHC.Generic, Show)

instance ToJSON ChangePasswordForm

data CreateUserForm = CreateUserForm { cu_username     :: JSString
                                     , cu_password     :: JSString
                                     , cu_account_name :: JSString
                                     , cu_is_admin     :: Bool } deriving (GHC.Generic, Show)

instance ToJSON CreateUserForm where
  toJSON = Data.Aeson.Types.genericToJSON
    Data.Aeson.Types.defaultOptions { Data.Aeson.Types.fieldLabelModifier = drop 3 }


instance FromJSON AuthInfo where
  parseJSON (Object o) = parseResponse hasToken o
    where
      hasToken = H.member "token" o

      parseResponse :: Bool -> Object -> Parser AuthInfo
      parseResponse True  obj = AuthInfo <$> obj .: "token" <*>  obj .: "session"
      parseResponse False obj = pure $ NoAuthToken "Hmm, access denied."

  parseJSON _ = return $ NoAuthToken "Sorry, access denied."

changePasswordOrError :: ChangePasswordForm -> IO (Either AppError Ok)
changePasswordOrError x = postAPIEither BD.Api.defaultAPI "change-password" x >>= return . bimap ApiError id

createUserOrError :: CreateUserForm -> IO (Either AppError Ok)
createUserOrError x = postAPIEither BD.Api.defaultAPI "create-user" x >>= return . bimap ApiError id

authenticateOrError :: (JSString, JSString) -> IO (Either AppError AuthInfo)
authenticateOrError (unm, psw) = do
  res <- getAPIEither' api "get-auth-token" :: IO (Either JSString AuthInfo)
  return $ case res of
    Left a                -> Left . ApiError $ "Sorry, access denied."
    Right (NoAuthToken s) -> Left . ApiError $ s
    Right (AuthInfo t s)  -> Right $ AuthInfo t s

  where
    api = BD.Api.defaultAPI { headers = [authHeader] }
    authHeader = ("Authorization", "Basic " <> base64encode (unm <> ":" <> psw))
    base64encode = btoa

foreign import javascript unsafe "btoa($1)" btoa :: JSString -> JSString
