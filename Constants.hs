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

-- | Elasticsearch cat-health endpoint
esCatHealth :: String
esCatHealth = "/_cat/health"
