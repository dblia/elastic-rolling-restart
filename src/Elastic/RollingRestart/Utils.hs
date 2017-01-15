{-| Utility functions.

-}

{-

Copyright (c) 2017 Dimitrios Bliamplias

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

module Elastic.RollingRestart.Utils
  ( getHostByIP     -- :: String -> IO String
  ) where

import Data.ByteString.Char8  (pack, unpack)
import Network.DNS.Lookup     (lookupRDNS)
import Network.DNS.Resolver   (defaultResolvConf, makeResolvSeed, withResolver)
import Text.Printf            (printf)


-- | Helper to perform a reverse lookup on the given IP address.
getHostByIP :: String -> IO String
getHostByIP ip = do
  let ip' = pack ip
  rs <- makeResolvSeed defaultResolvConf
  withResolver rs $ \resolver -> lookupRDNS resolver ip' >>= \r -> case r of
    Right domain -> return $ init $ unpack $ head domain
    Left dns_err -> fail $ printf "DNS error: %s" (show dns_err)
