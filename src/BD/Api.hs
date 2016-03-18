{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE GADTs              #-}

module BD.Api (
  API(..),
  defaultAPI,
  internalAPI,

  getAPI',
  getAPIEither',

  getAPI,
  getAPIEither,
  unsafeGetAPI,

  postAPI,
  postAPIEither,
  unsafePostAPI,

  postFileAPI,
  postFileAPIEither,

  deleteAPI,
  deleteAPIEither,

  Envelope(..),
  Ok(..),
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson
import qualified Data.Aeson.Types
import Data.Aeson.Types
import Data.ByteString
import Data.Data
import Data.Foldable (asum)
import Data.Monoid
import Data.Text(Text)
import Data.Time.Clock (UTCTime)
import qualified GHC.Generics as GHC

import GHCJS.Marshal(toJSVal_aeson)
import GHCJS.Types(JSVal, JSString, jsval)
import qualified Data.JSString
import JavaScript.Web.XMLHttpRequest -- TODO
import GHCJS.Foreign.QQ (js, jsu, jsu')
import Data.String (fromString)

import BD.Types
import BDPlatform.Config (xhrWithCredentials)

type Header = (JSString, JSString) -- JavaScript.Web.XMLHttpRequest uses it internally, but not exports

data API = API {
    baseURL :: JSString
  , headers :: [Header]
}

apiBaseURL :: JSString
--apiBaseURL = "http://localhost:3567/api/v1/"
apiBaseURL = "https://data.beautifuldestinations.com/api/v1/"

internalApiBaseURL :: JSString
internalApiBaseURL = "https://data.beautifuldestinations.com/api/v1/internal/"

defaultAPI :: API
defaultAPI = API apiBaseURL []

internalAPI :: API
internalAPI = API internalApiBaseURL []

showJS :: Show a => a -> JSString
showJS = fromString . show

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
getAPI' :: (FromJSON a, Monad m, MonadError s m, s ~ JSString, MonadIO m) => API -> JSString -> m a
getAPI' api path = do
  eitherResult <- liftIO (try $ xhrByteString request :: IO (Either XHRError (Response ByteString)) )
  case eitherResult of
    Left s       -> throwError ("getAPI': " <> showJS s)
    Right result -> case contents result of
      Nothing          -> throwError "getAPI': No response"
      Just byteString  -> case Data.Aeson.decodeStrict byteString of
        Nothing -> throwError $ "getAPI': Parse error " <> showJS byteString
        Just x  -> return x
  where
    request = Request { reqMethod          = GET
                      , reqURI             = baseURL api <> path
                      , reqLogin           = Nothing
                      , reqHeaders         = headers api
                      , reqWithCredentials = xhrWithCredentials
                      , reqData            = NoData
                      }

{-|
Same as `getAPI'`, but without the ability to set headers.
-}
getAPI :: (FromJSON a, Monad m, MonadError s m, s ~ JSString, MonadIO m) => API -> JSString -> m a
getAPI api path = getAPI' (api { headers = [] }) path

{-|
Same as `getAPI'`, with the `MonadError` specialized to `Either`.
-}
getAPIEither' :: FromJSON a => API -> JSString -> IO (Either JSString a)
getAPIEither' api path = runExceptT $ getAPI' api path


{-|
Make a POST request into the BD API.

For API specification and paths, see above.

Usage:

@
data Api
instance MonadError JSString Api
instance MonadIO Api

postQuery :: Query -> Api (Envelope QueryId)
postQuery = postAPI "\/internal\/queries\/"
@
-}
postAPI :: (ToJSON a, FromJSON b, Monad m, MonadError s m, s ~ JSString, MonadIO m) => API -> JSString -> a -> m b
postAPI api path value = do
  body         <- liftIO $ encodeJSString value
  eitherResult <- liftIO (try $ xhrByteString (request body) :: IO (Either XHRError (Response ByteString)))
  case eitherResult of
    Left s       -> throwError ("postAPI: " <> showJS s)
    Right result -> case contents result of
      Nothing          -> throwError "postAPI: No response"
      Just byteString  -> case Data.Aeson.decodeStrict byteString of
        Nothing -> throwError "postAPI: Parse error"
        Just x  -> return x
  where
    request body = Request { reqMethod          = POST
                           , reqURI             = baseURL api <> path
                           , reqLogin           = Nothing
                           , reqHeaders         = []
                           , reqWithCredentials = xhrWithCredentials
                           , reqData            = StringData body
                           }

postFileAPI :: (FromJSON b, Monad m, MonadError s m, s ~ JSString, MonadIO m)
            => API -> JSString -> [(JSString, FormDataVal)] -> m b
postFileAPI api path files = do
  eitherResult <- liftIO (try $ xhrByteString (request files) :: IO (Either XHRError (Response ByteString)))
  case eitherResult of
    Left s       -> throwError ("postFileAPI: " <> showJS s)
    Right result -> case contents result of
      Nothing         -> throwError "postFileAPI: No response"
      Just byteString -> case Data.Aeson.decodeStrict byteString of
        Nothing -> throwError "postFileAPI: Parse error"
        Just x  -> return x
  where
    request files = Request { reqMethod          = POST
                            , reqURI             = baseURL api <> path
                            , reqLogin           = Nothing
                            , reqHeaders         = []
                            , reqWithCredentials = xhrWithCredentials
                            , reqData            = FormData files
                            }


postFileAPIEither :: FromJSON a => API -> JSString -> [(JSString, FormDataVal)] -> IO (Either JSString a)
postFileAPIEither api path = runExceptT . postFileAPI api path


{-|
Make a DELETE request into the BD API.

API specification
<https://github.com/BeautifulDestinations/beautilytics/wiki/API-specification>

The @path@ parameter is everything after the @...\/api\/v1\/@ part. You must specify
the correct return type (as determined by the specification) or the request will
fail with a parse error. Note that most endpoints are wrapped in an 'Envelope'.

-}
deleteAPI :: (FromJSON a, Monad m, MonadError s m, s ~ JSString, MonadIO m) => API -> JSString -> m a
deleteAPI api path = do
  eitherResult <- liftIO (try $ xhrByteString request :: IO (Either XHRError (Response ByteString)) )
  case eitherResult of
    Left s       -> throwError ("deleteAPI : " <> showJS s)
    Right result -> case contents result of
      Nothing         -> throwError "deleteAPI : No response"
      Just byteString -> case Data.Aeson.decodeStrict byteString of
        Nothing -> throwError "deleteAPI: Parse error"
        Just x  -> return x
  where
    request = Request {
            reqMethod          = DELETE
          , reqURI             = baseURL api <> path
          , reqLogin           = Nothing
          , reqHeaders         = []
          , reqWithCredentials = xhrWithCredentials
          , reqData            = NoData
          }

{-|
Same as 'deleteAPI', with the 'MonadError' specialized to 'Either'.
-}
deleteAPIEither :: FromJSON a => API -> JSString -> IO (Either JSString a)
deleteAPIEither api = runExceptT . deleteAPI api

{-|
Same as 'getAPI', with the 'MonadError' specialized to 'Either'.
-}
getAPIEither :: FromJSON a => API -> JSString -> IO (Either JSString a)
getAPIEither api path = runExceptT $ getAPI api path

{-|
Same as 'getAPI' but throws an IO exception upon failure.
-}
unsafeGetAPI :: FromJSON a => API -> JSString -> IO a
unsafeGetAPI api path = unsafeGetRight <$> getAPIEither api path

{-|
Same as 'postAPI', with the 'MonadError' specialized to 'Either'.
-}
postAPIEither :: (ToJSON a, FromJSON b) => API -> JSString -> a -> IO (Either JSString b)
postAPIEither api path = runExceptT . postAPI api path

{-|
Same as 'postAPI' but throws an IO exception upon failure.
-}
unsafePostAPI :: (ToJSON a, FromJSON b) => API -> JSString -> a -> IO b
unsafePostAPI api path = fmap unsafeGetRight . postAPIEither api path

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

instance ToJSON a   => ToJSON (Envelope a)
instance FromJSON a => FromJSON (Envelope a)

data Ok = Ok JSString | Nok JSString deriving (GHC.Generic, Show, Eq, Data, Typeable)

instance FromJSON Ok where
  parseJSON = withObject "API response" $ \o ->
    asum [ Ok  <$> o .: "status"
         , Nok <$> o .: "error" ]
