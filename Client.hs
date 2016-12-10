{-| Module implementing the Elasticsearch related logic.

-}

module Client
  ( waitForStatus         -- :: String -> [String] -> IO String
  , shardAllocToggle      -- :: String -> String -> IO String
  ) where

import Constants       as C
import JData              (shardAllocSettings)
import Utils              (curlPutString)

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
        CurlOK    ->
          if any (`isInfixOf` body) st_list
          then return body
          else do
            threadDelay period
            wait' (cnt-1) period url st_list
        otherwise -> fail $ printf "Curl error for '%s': %s" url (show code)

-- | Toggle between shard allocation transient settings.
shardAllocToggle :: String -> String -> IO String
shardAllocToggle action host = do
  let es_url = host ++ C.esClusterSettings
  (rcode, resp) <- curlPutString es_url req_body C.curlContentTypeJson
  case rcode of
    CurlOK    ->
      if "\"acknowledged\":true" `isInfixOf` resp
      then return "Done"
      else fail $ printf "Failed allocation %s acknowledge for %s" action host
    otherwise -> fail $ printf "Curl error for '%s': %s" es_url (show rcode)
  where req_body :: [String]
        req_body = case action of
          "enable"  -> [shardAllocSettings "all"]
          "disable" -> [shardAllocSettings "none"]
          otherwise -> error $ printf "Unknown action argument: %s" action
