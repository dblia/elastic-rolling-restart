{-
  Module core logic.
-}

module Main where

import Client as Cli
import OptsParser (parseOpts, argParser)

import Network.Curl


main :: IO ()
main = withCurlDo $ do
  (master, nodes) <- parseOpts >>= argParser
  putStrLn $ "Master: " ++ master
  putStr "Nodes: "
  mapM_ (\str -> putStr $ " " ++ str) nodes
  putStrLn ""
  putStrLn ""
  -- Verify green cluster status
  body <- Cli.waitForStatus master ["green"]
  putStr body
