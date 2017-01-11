{-| Module defining the tool's constants.

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

module Elastic.RollingRestart.Constants
  ( pollWaitInterval              -- :: Int
  , pollWaitCount                 -- :: Int

  , curlQueryTimeout              -- :: Long
  , connectTimeout                -- :: Long
  , curlOpts                      -- :: [CurlOption]
  , curlPostDefaultContentType    -- :: String
  , curlContentTypeJson           -- :: Maybe String

  , shardAllocEnable              -- :: String
  , shardAllocDisable             -- :: String

  , esCatHealth                   -- :: String
  , esCatMaster                   -- :: String
  , esClusterSettings             -- :: String
  , esNodeShutdown                -- :: String

  , sshParams                     -- :: [String]

  , cmdSSH                        -- :: String
  , cmdSystemctl                  -- :: String
  , cmdSudo                       -- :: String
  ) where

import Network.Curl (Long, CurlOption(..))


-- | Poll wait interval (3 seconds).
pollWaitInterval :: Int
pollWaitInterval = 3 * 1000000  -- 10 ^ 6

-- | How many times to poll before giving up.
pollWaitCount :: Int
pollWaitCount = 20

-- | Curl query timeout.
curlQueryTimeout :: Long
curlQueryTimeout = 60

-- | Curl connection timeout.
connectTimeout :: Long
connectTimeout = 15

-- | Custom curl options.
curlOpts :: [CurlOption]
curlOpts =
  [ CurlTimeout curlQueryTimeout
  , CurlConnectTimeout connectTimeout
  ]

-- | Default Content-Type for PUT/POST requests.
curlPostDefaultContentType :: String
curlPostDefaultContentType = "application/x-www-form-urlencoded"

-- | Content-Type for JSON data.
curlContentTypeJson :: Maybe String
curlContentTypeJson = Just "application/json"

-- | Shard allocation enable string.
shardAllocEnable :: String
shardAllocEnable = "enable"

-- | Shard allocation disable string.
shardAllocDisable :: String
shardAllocDisable = "disable"

-- | Elasticsearch API cat-health endpoint.
esCatHealth :: String
esCatHealth = "/_cat/health"

-- | Elasticsearch API cat-master endpoint.
esCatMaster :: String
esCatMaster = "/_cat/master"

-- | Elasticsearch API cluster-settings endpoint.
esClusterSettings :: String
esClusterSettings = "/_cluster/settings"

-- | Elasticsearch API node-shutdow endpoint.
esNodeShutdown :: String
esNodeShutdown = "/_cluster/nodes/_local/_shutdown"

-- | Parameters for SSH remote command execution.
sshParams :: [String]
sshParams =
  [ "-q"
  , "-t"
  , "-oBatchMode=yes"
  , "-oConnectTimeout=5"
  , "-oStrictHostKeyChecking=no"
  ]

-- | ssh command executable.
cmdSSH :: String
cmdSSH = "/usr/bin/ssh"

-- | systemctl command executable.
cmdSystemctl :: String
cmdSystemctl = "/bin/systemctl"

-- | sudo command executable.
cmdSudo :: String
cmdSudo = "/usr/bin/sudo"
