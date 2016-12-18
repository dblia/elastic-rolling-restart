{-| Module implementing the Elasticsearch related logic.

-}

module Elastic.RollingRestart.Client
  ( waitForStatus         -- :: String -> [String] -> IO String
  , shardAllocToggle      -- :: String -> String -> IO String
  , nodeShutdown          -- :: String -> IO String
  , waitForNodeStop       -- :: String -> IO ()
  , waitForNodeJoin       -- :: String -> IO ()
  ) where

import Elastic.RollingRestart.Constants as C

import Elastic.RollingRestart.Utils.JData (shardAllocSettings)
import Elastic.RollingRestart.Utils.Curl  (curlPutString, curlPostString)

import Control.Concurrent (threadDelay)
import Data.List          (isInfixOf)
import Network.Curl
import Text.Printf        (printf)


-- | Wait for the desired cluster health status.
waitForStatus :: String -> [String] -> IO String
waitForStatus host status_list =
  let es_url = host ++ C.esCatHealth
  in wait' C.pollWaitCount C.pollWaitInterval es_url status_list
  where
    wait' :: Int -> Int -> URLString -> [String] -> IO String
    wait' 0 _ url _ = fail $ "Sorry mate, no more retries left: " ++ url
    wait' cnt period url st_list = do
      (code, body) <- curlGetString url C.curlOpts
      case code of
        CurlOK ->
          if any (`isInfixOf` body) st_list
          then return body
          else do
            threadDelay period
            wait' (cnt-1) period url st_list
        _      -> fail $ printf "Curl error for '%s': %s" url (show code)

-- | Toggle between shard allocation transient settings.
shardAllocToggle :: String -> String -> IO String
shardAllocToggle action host = do
  let es_url = host ++ C.esClusterSettings
  (rcode, resp) <- curlPutString es_url req_body C.curlContentTypeJson
  case rcode of
    CurlOK ->
      if "\"acknowledged\":true" `isInfixOf` resp
      then return "Done"
      else fail $ printf "Failed allocation %s acknowledge for %s" action host
    _      -> fail $ printf "Curl error for '%s': %s" es_url (show rcode)
  where req_body :: [String]
        req_body = case action of
          "enable"  -> [shardAllocSettings "all"]
          "disable" -> [shardAllocSettings "none"]
          _         -> error $ printf "Unknown action argument: %s" action

-- | Request a node shutdown.
nodeShutdown :: String -> IO String
nodeShutdown host = do
  let es_url = host ++ C.esNodeShutdown
  (rcode, resp) <- curlPostString es_url [] Nothing
  case rcode of
    CurlOK -> return resp
    _      -> fail $ printf "Curl error for '%s': %s" es_url (show rcode)

-- | Wait from a node stop responding.
waitForNodeStop :: URLString -> IO ()
waitForNodeStop = wait' C.pollWaitCount C.pollWaitInterval
  where
    wait' :: Int -> Int -> URLString -> IO ()
    wait' 0 _ host = fail $ "Sorry mate, no more retries left: " ++ host
    wait' cnt period host = do
      threadDelay period
      (rcode, _resp) <- curlGetString host C.curlOpts
      case rcode of
        CurlOK -> do
          threadDelay period
          wait' (cnt-1) period host
        _      -> return ()

-- | Wait from a node start responding.
waitForNodeJoin :: URLString -> IO ()
waitForNodeJoin = wait' C.pollWaitCount C.pollWaitInterval
  where
    wait' :: Int -> Int -> URLString -> IO ()
    wait' 0 _ host = fail $ "Sorry mate, no more retries left: " ++ host
    wait' cnt period host = do
      threadDelay period
      (rcode, _resp) <- curlGetString host C.curlOpts
      case rcode of
        CurlOK -> return ()
        _      -> do
          threadDelay period
          wait' (cnt-1) period host
