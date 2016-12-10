{-
  Module core logic.
-}

module Main where

import Client    as Cli
import Constants as C
import OptsParser   (parseOpts, argParser)
import Json         (shardAllocSettings)
import Utils        (curlPutString)

import Network.Curl


main :: IO ()
main = withCurlDo $ do
  (master, nodes) <- parseOpts >>= argParser
  putStrLn $ "Master: " ++ master
  putStr "Nodes: "
  mapM_ (\str -> putStr $ " " ++ str) nodes
  putStrLn "\n"
  -- Verify green cluster status
  putStrLn ">>> Verifying green cluster status"
  body <- Cli.waitForStatus master ["green"]
  putStrLn body

  putStrLn ">>> Disabling routing shard allocation"
  out <- Cli.shardAllocToggle C.shardAllocDisable master
  putStrLn out
