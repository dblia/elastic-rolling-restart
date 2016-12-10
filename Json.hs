{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Json where

import Data.Aeson   ((.=), FromJSON(..), ToJSON(..), Value(..), object, encode)
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as BS


data TransientSettings = TransientSettings
  { clusterRoutingAllocationEnable :: String } deriving (Show, Generic)

data ClusterSettings = ClusterSettings
  { transient :: TransientSettings } deriving (Show, Generic)

instance FromJSON TransientSettings
instance ToJSON TransientSettings where
  -- this generates a Value
  toJSON (TransientSettings value) =
    object ["cluster.routing.allocation.enable" .= value]

  -- this encodes directly to a bytestring Builder (requires Data.Aeson > 10 )
  --toEncoding (TransientSettings value) =
  --  pairs ("cluster.routing.allocation.enable" .= value)

instance FromJSON ClusterSettings
instance ToJSON ClusterSettings


shardAllocSettings :: String -> String
shardAllocSettings toggle =
  let settings = ClusterSettings $ TransientSettings toggle
  in BS.unpack $ encode $ settings
