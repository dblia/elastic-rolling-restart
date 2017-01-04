{-| Module containing various utility functions.

-}

{-

Copyright (c) 2016, 2017 Dimitrios Bliamplias

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

-}

module Elastic.RollingRestart.Utils.Curl
  ( curlPutString        -- :: URLString
                         -- -> [String] -> Maybe String -> IO (CurlCode, String)
  , curlPostString       -- :: URLString
                         -- -> [String] -> Maybe String -> IO (CurlCode, String)
  ) where

import Elastic.RollingRestart.Constants as C

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
    setopt' h (CurlCookieJar "cookies")
    setopt' h (CurlFailOnError True)
    setopt' h (CurlURL url)
    setopt' h (CurlCustomRequest method)
    setopt' h (CurlPostFields pfs)
    setopt' h (CurlHttpHeaders ["Content-Type: " ++ content_type])
    setopt' h (CurlWriteFunction (gatherOutput ref))
    rc <- perform h
    lss <- readIORef ref
    return (rc, concat $ reverse lss)
 where setopt' :: Curl -> CurlOption -> IO ()
       setopt' c_obj c_opt = do
         _ <- setopt c_obj c_opt
         return ()
