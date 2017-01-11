{-| Module implementing the core Elasticsearch logic.

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

module Elastic.RollingRestart.Client
  ( waitForStatus         -- :: String -> [String] -> IO String
  , shardAllocToggle      -- :: String -> String -> IO String
  , nodeShutdown          -- :: String -> IO String
  , waitForNodeStop       -- :: String -> IO ()
  , waitForNodeJoin       -- :: String -> IO ()
  , getClusterName        -- :: URLString -> IO String
  , getMasterNodeID       -- :: URLString -> IO String
  , getNodeIDs            -- :: URLString -> IO [String]
  ) where

import Elastic.RollingRestart.Constants as C

import Elastic.RollingRestart.Utils.Curl  (curlPutString, curlPostString)
import Elastic.RollingRestart.Utils.JData.ClusterInfo
import Elastic.RollingRestart.Utils.JData.NodesInfo
import Elastic.RollingRestart.Utils.JData.ShardAlloc

import Control.Concurrent (threadDelay)
import Data.Aeson         (decode, eitherDecode)
import Data.List          (isInfixOf)
import Data.List.Split    (splitOn)
import Data.Map           (foldWithKey)
import Network.Curl
import Text.Printf        (printf)

import qualified Data.ByteString.Lazy.Char8 as BS8

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

-- | Toggle between shard allocation settings.
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

-- | Request a node shutdown via the Elasticsearch shutdown API.
nodeShutdown :: String -> IO String
nodeShutdown host = do
  let es_url = host ++ C.esNodeShutdown
  (rcode, resp) <- curlPostString es_url [] Nothing
  case rcode of
    CurlOK -> return resp
    _      -> fail $ printf "Curl error for '%s': %s" es_url (show rcode)

-- | Wait for a node to stop responding.
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

-- | Wait for a node to start responding.
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

-- | Retrieve the Elasticsearch cluster name.
getClusterName :: URLString -> IO String
getClusterName es_url = do
  (rcode, resp) <- curlGetString es_url []
  case rcode of
    CurlOK -> do
      let resp_stripped = unwords $ words resp
      case (decode $ BS8.pack resp_stripped :: Maybe ClusterInfo) of
        Just (ClusterInfo _ _ cl_name _ _) -> return cl_name
        Nothing  -> fail $ printf "Couldn't parse cluster name for '%s'" es_url
    _      -> fail $ printf "Curl error for '%s': %s" es_url (show rcode)

-- | Retrieve the Elasticsearch cluster master node ID.
getMasterNodeID :: URLString -> IO String
getMasterNodeID host = do
  let es_url = host ++ C.esCatMaster
  (rcode, resp) <- curlGetString es_url []
  case rcode of
    CurlOK -> return $ head $ splitOn " " resp
    _      -> fail $ printf "Curl error for '%s': %s" es_url (show rcode)

-- | Retrieve the Elasticsearch cluster node IDs.
getNodeIDs :: URLString -> IO [String]
getNodeIDs host = do
  let es_url = host ++ C.esNodes
  (rcode, resp) <- curlGetString es_url []
  case rcode of
    CurlOK -> do
      let resp_strip = unwords $ words resp
      case (eitherDecode $ BS8.pack resp_strip :: Either String NodesAll) of
        Right (NodesAll _ (NodeList nodes_info)) ->
          return $ foldWithKey (\key _val ks -> key:ks) [] nodes_info
        Left  err -> fail err
    _      -> fail $ printf "Curl error for '%s': %s" es_url (show rcode)
