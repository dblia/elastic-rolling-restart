{-| Module implementiing the tool's core logic.

-}

module Main where

import Client        as Cli
import Constants     as C
import OptsParser       (parseOpts, argParser)
import JData            (shardAllocSettings)
import Utils            (curlPutString)

import Network.Curl


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

  -- | Request node shutdown for node
  let node = head nodes
  putStrLn $ ">>> Requesting node shutdown for " ++ node
  out <- Cli.nodeShutdown node
  putStrLn $ out ++ "\n"
