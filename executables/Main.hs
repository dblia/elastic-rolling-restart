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
import Elastic.RollingRestart.Utils.Process

import Data.List.Split  (splitOn)
import Network.Curl
import Text.Printf      (printf)


-- | Helper function to restart a single ElasticSearch process.
singleNodeRestart :: String -> String -> String -> String -> IO ()
singleNodeRestart master node service cluster_name = do

  putStrLn $ printf "\nRestarting host `%s`:" node

  -- | Verify green cluster status.
  putStrLn " - Verifying `green` cluster status"
  _ <- Cli.waitForStatus master ["green"]

  -- | Disable routing shard allocation.
  putStrLn $ printf " - Disabling shard allocation for `%s`" cluster_name
  _ <- Cli.shardAllocToggle C.shardAllocDisable master

  -- | Request ElasticSearch process to shutdown.
  putStrLn " - Requesting ElasticSearch process to shutdown:"
  --Cli.nodeShutdown node >>= (\out -> putStrLn $ out ++ "\n")
  _ <- Cli.nodeShutdown node

  -- | Wait for node to stop.
  putStrLn "   * Waiting for node to stop ..."
  Cli.waitForNodeStop node
  putStrLn "   * Node stopped successfully!"

  -- | Restart the ElasticSearch process.
  putStrLn " - Re-starting ElasticSearch process:"
  let [hostname, _] = splitOn ":" node
  let cmd = unwords [C.cmdSudo, C.cmdSystemctl, "start", service]
  _ <- runCmd cmd (Just hostname)

  -- | Wait for node to respond after restart.
  putStrLn "   * Waiting for node to re-join the cluster ..."
  Cli.waitForNodeJoin node
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

  putStrLn $ printf "Host `%s` restarted successfully!" node


-- | Implements the core logic for performing a rolling-restart.
main :: IO ()
main = withCurlDo $ do

  -- | Parse command-line arguments.
  (master, nodes, service) <- parseOpts >>= argParser

  -- | Retrieve the cluster name.
  cluster_name <- Cli.getClusterName master

  -- | Output the information about the given cluster.
  putStrLn $ "Cluster: " ++ cluster_name
  putStrLn $ "Master:  " ++ master
  putStr "Nodes:  "
  mapM_ (\str -> putStr $ " " ++ str) nodes
  putStr "\n"
  putStrLn $ "Service: " ++ service

  -- | Rolling restart the given cluster nodes.
  mapM_ (\node -> singleNodeRestart master node service cluster_name) nodes

  -- | Finally, restart the master node.
  let node = master
  let dummy_master = head nodes
  singleNodeRestart dummy_master node service cluster_name

  putStrLn $ printf "\nRolling restart of cluster `%s` completed!" cluster_name
