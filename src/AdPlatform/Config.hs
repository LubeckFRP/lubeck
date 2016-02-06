module AdPlatform.Config where

-- Get auth token using username/password or launch the app without it.
-- This will be required when the API will require authentication.
useAuth            = True

-- Pass credentials (eg. cookies) with CORS XHR requests.
-- Currently disabled because of bad `Access-Control-Allow-Origin: *` header set by the server.
-- Once server will be fixed and the API will require authentication set this to True.
xhrWithCredentials = False
