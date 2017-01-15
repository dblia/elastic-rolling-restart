{-| Module implementing the tool's core logic.

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

module Main where

import Elastic.RollingRestart.Client        as Cli
import Elastic.RollingRestart.Constants     as C
import Elastic.RollingRestart.OptsParser       (parseOpts, argParser)
import Elastic.RollingRestart.Utils
import Elastic.RollingRestart.Utils.Process

import Data.List.Split  (splitOn)
import Network.Curl
import Text.Printf      (printf)


-- | Helper function to restart a single ElasticSearch process.
singleNodeRestart :: String             -- ^ master node url
                  -> (String, String)   -- ^ restarted node (url, fqdn) tuple
                  -> String             -- ^ Elasticsearch service name
                  -> String             -- ^ Elasticsearch cluster name
                  -> IO ()
singleNodeRestart master node service cluster_name = do

  let node_url = fst node
  let node_fqdn = snd node

  putStrLn $ printf "\nRestarting host `%s`:" node_fqdn

  -- | Verify green cluster status.
  putStrLn " - Verifying `green` cluster status"
  _ <- Cli.waitForStatus master ["green"]

  -- | Disable routing shard allocation.
  putStrLn $ printf " - Disabling shard allocation for `%s`" cluster_name
  _ <- Cli.shardAllocToggle C.shardAllocDisable master

  -- | Request ElasticSearch process to shutdown.
  putStrLn " - Requesting ElasticSearch process to shutdown:"
  --Cli.nodeShutdown node >>= (\out -> putStrLn $ out ++ "\n")
  _ <- Cli.nodeShutdown node_url

  -- | Wait for node to stop.
  putStrLn "   * Waiting for node to stop ..."
  Cli.waitForNodeStop node_url
  putStrLn "   * Node stopped successfully!"

  -- | Restart the ElasticSearch process.
  putStrLn " - Re-starting ElasticSearch process:"
  let [hostname, _] = splitOn ":" node_url
  let cmd = unwords [C.cmdSudo, C.cmdSystemctl, "start", service]
  _ <- runCmd cmd (Just hostname)

  -- | Wait for node to respond after restart.
  putStrLn "   * Waiting for node to re-join the cluster ..."
  Cli.waitForNodeJoin node_url
  putStrLn "   * Node re-joined the cluster!"

  -- | Verify at least yellow cluster status.
  putStrLn " - Verifying `yellow` cluster status, at least"
  _ <- Cli.waitForStatus master ["yellow", "green"]

  -- | Re-enabling routing shard allocation.
  putStrLn $ printf " - Enabling shard allocation for `%s`" cluster_name
  _ <- Cli.shardAllocToggle C.shardAllocEnable master

  -- | Verify green cluster status.
  putStrLn " - Verifying `green` cluster status"
  _ <- Cli.waitForStatus master ["green"]

  putStrLn $ printf "Host `%s` restarted successfully!" node_fqdn


-- | Implements the core logic for performing a rolling-restart.
main :: IO ()
main = withCurlDo $ do

  -- | Parse command-line arguments.
  (hostname, service) <- parseOpts >>= argParser

  -- | Retrieve the cluster name.
  cluster_name <- Cli.getClusterName hostname
  -- | Retrieve the cluster node URLs.
  node_urls <- Cli.getNodeURLs hostname
  -- | Retrieve the cluster node FQDNs.
  node_fqdns <- mapM (getHostByIP . takeWhile (/= ':')) node_urls
  -- | Concatenate node urls and their FQDNs
  let node_addrs = zip node_urls node_fqdns

  -- | Retrieve the cluster master node.
  let master = last node_addrs
  -- | Get all cluster nodes except the master node.
  let nodes = init node_addrs

  -- | Output the information about the given cluster.
  putStrLn $ "Cluster: " ++ cluster_name
  putStrLn $ "Master:  " ++ snd master
  putStr "Nodes:  "
  mapM_ (\tup -> putStr $ " " ++ snd tup) nodes
  putStr "\n"
  putStrLn $ "Service: " ++ service

  -- | Rolling restart the given cluster nodes.
  mapM_ (\n_tupl ->
    singleNodeRestart (fst master) n_tupl service cluster_name) nodes

  -- | Finally, restart the master node.
  let node = head nodes
  singleNodeRestart (fst node) master service cluster_name

  putStrLn $ printf "\nRolling restart of cluster `%s` completed!" cluster_name
