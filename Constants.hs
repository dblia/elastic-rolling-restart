{-
  Module constants.
-}

module Constants where

import Network.Curl (Long, CurlOption(..))


-- | set the poll wait interval to 3 seconds
pollWaitInterval :: Int
pollWaitInterval = 3 * 10 ^ 6

-- | pollWaitCount times before giving up
pollWaitCount :: Int
pollWaitCount = 20

-- | curl query timeout
queryTimeout :: Long
queryTimeout = 60

-- | curl connection timeout
connTimeout :: Long
connTimeout = 15

-- | define curl options
curlOpts :: [CurlOption]
curlOpts =
  [ CurlTimeout queryTimeout
  , CurlConnectTimeout connTimeout
  ]

curlPostDefaultContentType :: String
curlPostDefaultContentType = "application/x-www-form-urlencoded"

curlContentTypeJson :: Maybe String
curlContentTypeJson = Just "application/json"

shardAllocEnable :: String
shardAllocEnable = "enable"

shardAllocDisable :: String
shardAllocDisable = "disable"

-- | Elasticsearch cat-health endpoint
esCatHealth :: String
esCatHealth = "/_cat/health"

-- | Elasticsearch cluster settings endpoint
esClusterSettings :: String
esClusterSettings = "/_cluster/settings"
