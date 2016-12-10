{-
  Elasticsearch client module.
-}

module Client where

import Constants       as C

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
