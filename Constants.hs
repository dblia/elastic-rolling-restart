{-| Module defining the tool's constants.

-}

module Constants
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
  , esClusterSettings             -- :: String
  ) where

import Network.Curl (Long, CurlOption(..))


-- | Poll wait interval (3 seconds).
pollWaitInterval :: Int
pollWaitInterval = 3 * 10 ^ 6

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

-- | Elasticsearch API cluster-settings endpoint.
esClusterSettings :: String
esClusterSettings = "/_cluster/settings"
