{-| Module containing various utility functions.

-}

module Utils
  ( curlPutString        -- :: URLString
                         -- -> [String] -> Maybe String -> IO (CurlCode, String)
  ) where

import Constants       as C

import Data.Maybe         (fromMaybe)
import Data.IORef         (newIORef, readIORef)
import Network.Curl


-- | Performs a PUT request, and returns the response code and body.
curlPutString :: URLString -> [String] -> Maybe String -> IO (CurlCode, String)
curlPutString url pfs ctype =
  let content_type = fromMaybe C.curlPostDefaultContentType ctype
  in initialize >>= \ h -> do
    ref <- newIORef []
    setopt h (CurlCookieJar "cookies")
    setopt h (CurlFailOnError True)
    setopt h (CurlURL url)
    setopt h (CurlCustomRequest "PUT")
    setopt h (CurlPostFields pfs)
    setopt h (CurlHttpHeaders ["Content-Type: " ++ content_type])
    setopt h (CurlWriteFunction (gatherOutput ref))
    rc <- perform h
    lss <- readIORef ref
    return (rc, concat $ reverse lss)
