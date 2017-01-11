{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

{-| Elasticsearch nodes API JSON definitions and utility functions.

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

module Elastic.RollingRestart.Utils.JData.NodesInfo
  ( NodesAll(..)       -- :: NodesAll (String, NodeList)
  , NodeInfo(..)       -- :: NodeInfo (String String String String String String String)
  , NodeList(..)       -- :: NodeList (Map String NodeInfo)
  ) where

import Data.Aeson   (FromJSON(..), ToJSON(..))
import Data.Functor ((<$>))
import Data.Map     (Map)
import GHC.Generics


-- | Elasticsearch nodes definition.
data NodeInfo = NodeInfo {
    name              :: String
  , transport_address :: String
  , host              :: String
  , ip                :: String
  , version           :: String
  , build             :: String
  , http_address      :: String
} deriving (Eq, Show, Generic)

instance FromJSON NodeInfo
instance ToJSON NodeInfo

newtype NodeList = NodeList (Map String NodeInfo)
  deriving (Show, Generic)

instance FromJSON NodeList where
  parseJSON val = NodeList <$> parseJSON val
instance ToJSON NodeList

data NodesAll = NodesAll {
    cluster_name :: String
  , nodes        :: NodeList
} deriving (Show, Generic)

instance FromJSON NodesAll
instance ToJSON NodesAll
