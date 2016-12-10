{-| Module implementiing the tool's core logic.

-}

module Main where

import Client        as Cli
import Constants     as C
import JData            (shardAllocSettings)
import OptsParser       (parseOpts, argParser)
import Process
import Utils            (curlPutString)

import Network.Curl
import Text.Printf      (printf)


main :: IO ()
main = withCurlDo $ do
  -- | Parse command-line arguments.
  (master, nodes) <- parseOpts >>= argParser
  putStrLn $ "Master: " ++ master
  putStr "Nodes: "
  mapM_ (\str -> putStr $ " " ++ str) nodes
  putStrLn "\n"

  -- | Verify green cluster status.
  putStrLn ">>> Verifying green cluster status"
  body <- Cli.waitForStatus master ["green"]
  putStrLn body

  -- | Disable routing shard allocation.
  putStrLn ">>> Disabling routing shard allocation"
  out <- Cli.shardAllocToggle C.shardAllocDisable master
  putStrLn $ out ++ "\n"

  -- | Request node shutdown for node.
  let node = head nodes
  putStrLn $ ">>> Requesting node shutdown for " ++ node
  out <- Cli.nodeShutdown node
  putStrLn $ out ++ "\n"

  -- | Wait for node to stop.
  putStrLn $ printf ">>> Waiting for node '%s' to stop" node
  Cli.waitForNodeStop node
  putStr "\n"

  -- | Restart the node.
  putStrLn $ printf ">>> Restarting node '%s'" node
  Process.runCmd "sudo systemctl start esctl@1.service" Nothing
  putStr "\n"

  -- | Wait for node to respond after restart.
  putStrLn $ printf ">>> Waiting for node '%s' to re-join the cluster" node
  Cli.waitForNodeJoin node
  putStr "\n"
