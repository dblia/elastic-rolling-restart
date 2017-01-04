{-| Module implementiing the tool's core logic.

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

import Elastic.RollingRestart.Client     as Cli
import Elastic.RollingRestart.Constants  as C
import Elastic.RollingRestart.OptsParser    (parseOpts, argParser)
import Elastic.RollingRestart.Utils.Process

import Data.List.Split  (splitOn)
import Network.Curl
import Text.Printf      (printf)

-- | Helper function to restart a single node
restart :: String -> String -> String -> IO ()
restart master node service = do

  putStrLn $ printf ">>>>>> Restarting node '%s'\n" node

  -- | Verify green cluster status.
  putStrLn ">>> Verifying green cluster status"
  body <- Cli.waitForStatus master ["green"]
  putStrLn body

  -- | Disable routing shard allocation.
  putStrLn ">>> Disabling routing shard allocation"
  _out <- Cli.shardAllocToggle C.shardAllocDisable master
  putStrLn $ _out ++ "\n"

  -- | Request node shutdown for node.
  putStrLn $ ">>> Requesting node shutdown for " ++ node
  Cli.nodeShutdown node >>= (\out -> putStrLn $ out ++ "\n")

  -- | Wait for node to stop.
  putStrLn $ printf ">>> Waiting for node '%s' to stop" node
  Cli.waitForNodeStop node
  putStr "\n"

  -- | Restart the node.
  putStrLn $ printf ">>> Restarting node '%s'" node
  let [hostname, _] = splitOn ":" node
  let cmd = unwords [C.cmdSudo, C.cmdSystemctl, "start", service]
  runCmd cmd (Just hostname) >>= putStrLn

  -- | Wait for node to respond after restart.
  putStrLn $ printf ">>> Waiting for node '%s' to re-join the cluster" node
  Cli.waitForNodeJoin node
  putStr "\n"

  -- | Verify at least yellow cluster status.
  putStrLn ">>> Verifying at least yellow cluster status"
  Cli.waitForStatus master ["yellow", "green"] >>= putStrLn

  -- | Re-enabling routing shard allocation.
  putStrLn ">>> Re-enabling routing shard allocation"
  _out <- Cli.shardAllocToggle C.shardAllocEnable master
  putStrLn $ _out ++ "\n"

  -- | Verify green cluster status.
  putStrLn ">>> Verifying green cluster status"
  Cli.waitForStatus master ["green"] >>= putStrLn

  putStrLn $ printf ">>>>>> Node '%s' restarted successfully!\n" node

main :: IO ()
main = withCurlDo $ do
  -- | Parse command-line arguments.
  (master, nodes, service) <- parseOpts >>= argParser
  putStrLn $ "Service: " ++ service
  putStrLn $ "Master: " ++ master
  putStr "Nodes: "
  mapM_ (\str -> putStr $ " " ++ str) nodes
  putStrLn "\n"

  -- | Rolling restart the cluster nodes.
  mapM_ (\node -> restart master node service) nodes

  -- | Finally, restart the master node.
  let node = master
  let new_master = head nodes
  putStrLn $ printf "New master is '%s', last node '%s'" new_master node
  restart new_master node service
