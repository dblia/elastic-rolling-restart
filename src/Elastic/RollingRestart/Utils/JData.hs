{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-| JSON data definitions and utility functions.

-}

module Elastic.RollingRestart.Utils.JData
  ( shardAllocSettings        -- :: String -> String
  ) where

import Data.Aeson   ((.=), FromJSON(..), ToJSON(..), Value(..), object, encode)
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as BS8


-- | Elasticsearch cluster.routing.allocation.enable setting dict.
data AllocationEnable = AllocationEnable
  { clusterRoutingAllocationEnable :: String } deriving (Show, Generic)

instance FromJSON AllocationEnable
instance ToJSON AllocationEnable where
  toJSON (AllocationEnable value) =
    object ["cluster.routing.allocation.enable" .= value]

-- | Elasticsearch cluster transient setting dict.
data TransientSettings = TransientSettings
  { transient :: AllocationEnable } deriving (Show, Generic)

instance FromJSON TransientSettings
instance ToJSON TransientSettings


-- | Builds a transient settings dictionary for an Elasticsearch cluster.
shardAllocSettings :: String -> String
shardAllocSettings toggle =
  let settings = TransientSettings $ AllocationEnable toggle
  in BS8.unpack $ encode settings
