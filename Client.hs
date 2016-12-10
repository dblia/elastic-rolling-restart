{-
  Elasticsearch client module.
-}

module Client where

import Constants       as C
import Json               (shardAllocSettings)
import Utils              (curlPutString)

import Control.Concurrent (threadDelay)
import Data.List          (isInfixOf)
import Network.Curl
import Text.Printf        (printf)


-- | wait for the desired cluster health status
waitForStatus :: String -> [String] -> IO String
waitForStatus hostname status_list =
  let es_url = hostname ++ C.esCatHealth
  in wait' C.pollWaitCount C.pollWaitInterval es_url status_list
  where
    wait' :: Int -> Int -> URLString -> [String] -> IO String
    wait' 0 _ url _ = fail $ "Sorry mate, no more retries left: " ++ url
    wait' cnt period url st_list = do
      (code, body) <- curlGetString url C.curlOpts
      case code of
        CurlOK    ->
          case or $ map (\status -> isInfixOf status body) st_list of
            True  -> return body
            False -> do
              threadDelay period
              wait' (cnt-1) period url st_list
        otherwise -> fail $ printf "Curl error for '%s': %s" url (show code)


shardAllocToggle :: String -> String -> IO String
shardAllocToggle action hostname = do
  let es_url = hostname ++ C.esClusterSettings
  (rcode, resp) <- curlPutString es_url req_body C.curlContentTypeJson
  case rcode of
    CurlOK    ->
      case isInfixOf "\"acknowledged\":true" resp of
        True  -> return "Done"
        False -> fail $ printf "Failed acknowledge of allocation '%s'\
                              \ for '%s'" action hostname
    otherwise -> fail $ printf "Curl error for '%s': %s" es_url (show rcode)
  where req_body :: [String]
        req_body = case action of
          "enable"  -> [shardAllocSettings "all"]
          "disable" -> [shardAllocSettings "none"]
          otherwise -> error $ printf "Wrong action given: '%s'" action
