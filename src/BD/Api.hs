
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, OverloadedStrings, GADTs, DeriveGeneric, DeriveDataTypeable, QuasiQuotes #-}

module BD.Api (
  getAPI,
  getAPIEither,
  unsafeGetAPI,
  Envelope(..),
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson
import qualified Data.Aeson.Types
import Data.Data
import Data.Monoid
import Data.Text(Text)
import Data.Time.Clock (UTCTime)
import GHCJS.Marshal(toJSVal_aeson)

import qualified GHC.Generics as GHC

import GHCJS.Types(JSVal, JSString, jsval)
import qualified Data.JSString
import JavaScript.Web.XMLHttpRequest -- TODO
import GHCJS.Foreign.QQ (js, jsu, jsu')

{-|
Make a GET request into the BD API.

API specification
<https://github.com/BeautifulDestinations/beautilytics/wiki/API-specification>

The @path@ parameter is everything after the @...\/api\/v1\/@ part. You must specify
the correct return type (as determined by the specification) or the request will
fail with a parse error. Note that most endpoints are wrapped in an 'Envelope'
(see example below).

Usage:

@
data Api
instance MonadError JSString Api
instance MonadIO Api

getAccount :: JSString -> Api (Envelope Account)
getAccount name = getAPI "\/" <> name <> "\/account"
@
-}
getAPI :: (FromJSON a, Monad m, MonadError s m, s ~ JSString, MonadIO m) => JSString -> m a
getAPI path = do
  result <- liftIO $ xhrByteString request
  case contents result of
    Nothing          -> throwError "getAPI: No response"
    Just byteString  -> case Data.Aeson.decodeStrict byteString of
      Nothing -> throwError "getAPI: Parse error"
      Just x  -> return x
  where
    request = Request {
            reqMethod          = GET
          , reqURI             = "http://data.beautifuldestinations.com/api/v1/" <> path
          , reqLogin           = Nothing
          , reqHeaders         = []
          , reqWithCredentials = False
          , reqData            = NoData
          }

postAPI :: (ToJSON a, FromJSON b, Monad m, MonadError s m, s ~ JSString, MonadIO m) => JSString -> a -> m b
postAPI path value = do
  body <- liftIO $ encodeJSString value
  result <- liftIO $ xhrByteString (request body)
  case contents result of
    Nothing          -> throwError "getAPI: No response"
    Just byteString  -> case Data.Aeson.decodeStrict byteString of
      Nothing -> throwError "getAPI: Parse error"
      Just x  -> return x
  where
    request body = Request {
            reqMethod          = POST
          , reqURI             = "http://data.beautifuldestinations.com/api/v1/" <> path
          , reqLogin           = Nothing
          , reqHeaders         = []
          , reqWithCredentials = False
          , reqData            = (StringData $ body)
          }

{-|
Same as 'getAPI', with the 'MonadError' specialized to 'Either'.
-}
getAPIEither :: FromJSON a => JSString -> IO (Either JSString a)
getAPIEither = runExceptT . getAPI

{-|
Same as 'getAPI' but throws an IO exception upon failure.
-}
unsafeGetAPI :: FromJSON a => JSString -> IO a
unsafeGetAPI = fmap unsafeGetRight . getAPIEither


{-|
Same as 'postAPI', with the 'MonadError' specialized to 'Either'.
-}
postAPIEither :: (ToJSON a, FromJSON b) => JSString -> a -> IO (Either JSString b)
postAPIEither p = runExceptT . postAPI p

{-|
Same as 'postAPI' but throws an IO exception upon failure.
-}
unsafePostAPI :: (ToJSON a, FromJSON b) => JSString -> a -> IO b
unsafePostAPI p = fmap unsafeGetRight . postAPIEither p

unsafeGetRight :: Either JSString b -> b
unsafeGetRight (Left  e) = error (Data.JSString.unpack e)
unsafeGetRight (Right x) = x

encodeJSString :: ToJSON a => a -> IO JSString
encodeJSString value = do
  jsval <- toJSVal_aeson value
  return $ stringify jsval
-- Use toJSVal_aeson to get a JSVal
-- Pass this to a JSON.stringify wrapper

stringify :: JSVal -> JSString
stringify object = [jsu'| JSON.stringify(`object) |]

data Envelope a = Envelope { payload :: a } deriving (GHC.Generic,Show, Eq, Data, Typeable)

instance ToJSON a => ToJSON (Envelope a)
instance FromJSON a => FromJSON (Envelope a)
