{-| Module containing various utility functions.

-}

module Utils
  ( curlPutString        -- :: URLString
                         -- -> [String] -> Maybe String -> IO (CurlCode, String)
  , curlPostString       -- :: URLString
                         -- -> [String] -> Maybe String -> IO (CurlCode, String)
  ) where

import Constants       as C

import Data.Maybe         (fromMaybe)
import Data.IORef         (newIORef, readIORef)
import Network.Curl


-- | Performs a PUT request, and returns the response code and body.
curlPutString :: URLString -> [String] -> Maybe String -> IO (CurlCode, String)
curlPutString = createRequest "PUT"

-- | Performs a POST request, and returns the response code and body.
curlPostString :: URLString -> [String] -> Maybe String -> IO (CurlCode, String)
curlPostString = createRequest "POST"

-- | Performs a PUT or POST request and returns the response code and body.
createRequest :: String                -- ^ request method
              -> URLString             -- ^ URL string
              -> [String]              -- ^ request post fields
              -> Maybe String          -- ^ request content-type
              -> IO (CurlCode, String) -- ^ (response code, response body)
createRequest method url pfs ctype =
  let content_type = fromMaybe C.curlPostDefaultContentType ctype
  in initialize >>= \ h -> do
    ref <- newIORef []
    setopt h (CurlCookieJar "cookies")
    setopt h (CurlFailOnError True)
    setopt h (CurlURL url)
    setopt h (CurlCustomRequest method)
    setopt h (CurlPostFields pfs)
    setopt h (CurlHttpHeaders ["Content-Type: " ++ content_type])
    setopt h (CurlWriteFunction (gatherOutput ref))
    rc <- perform h
    lss <- readIORef ref
    return (rc, concat $ reverse lss)
