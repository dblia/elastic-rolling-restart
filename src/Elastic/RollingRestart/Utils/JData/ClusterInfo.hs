{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

{-| Elasticsearch Cluster info JSON definitions and utility functions.

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

module Elastic.RollingRestart.Utils.JData.ClusterInfo
  ( ClusterInfo(..)    -- :: ClusterInfo (Int String String VersionInfo String)
  ) where

import Data.Aeson   (FromJSON(..), ToJSON(..))
import GHC.Generics


-- | Elasticsearch version info definition.
data VersionInfo = VersionInfo {
    number          :: String
  , build_hash      :: String
  , build_timestamp :: String
  , build_snapshot  :: Bool
  , lucene_version  :: String
} deriving (Show, Generic)

instance FromJSON VersionInfo
instance ToJSON VersionInfo

-- | Elasticsearch cluster info definition.
data ClusterInfo = ClusterInfo {
    status       :: Int
  , name         :: String
  , cluster_name :: String
  , version      :: VersionInfo
  , tagline      :: String
} deriving (Show, Generic)

instance FromJSON ClusterInfo
instance ToJSON ClusterInfo
