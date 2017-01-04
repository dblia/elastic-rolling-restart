{-| Command-line options parser module.

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

module Elastic.RollingRestart.OptsParser
  ( Opts(..)        -- :: Opts
  , optParser       -- :: Parser Opts
  , parseOpts       -- :: IO Opts
  , argParser       -- :: Opts -> IO (String, [String])
  ) where

import Data.List.Split     (splitOn)
import Options.Applicative


-- | Command line arguments.
data Opts = Opts
  { optsMaster  :: String
  , optsNodes   :: String
  , optsService :: String
  } deriving (Eq, Ord, Show)


-- | Command line argument parser.
optParser :: Parser Opts
optParser = helper <*> _parser
  where
    _parser :: Parser Opts
    _parser = Opts
      <$> strOption
          ( long "master"
           <> short 'm'
           <> metavar "MASTER"
           <> help "The cluster master node; used to coordinate the rolling\
                   \ restart operation.")
      <*> strOption
            ( long "nodes"
           <> short 'n'
           <> metavar "NODES"
           <> help "The cluster node hostnames to restart; nodes should be\
                   \ concatenated with commas: host1:port[,host2:port].")
      <*> strOption
            ( long "service"
           <> short 's'
           <> metavar "SERVICE"
           <> help "The elasticsearch service name.")

-- | Parse command-line argument by displaying custom description.
parseOpts :: IO Opts
parseOpts = execParser $ info optParser $
     fullDesc
  <> progDesc "ElasticSearch cluster rolling restart tool."

-- | Parse command-line argument and return them properly.
argParser :: Opts -> IO (String, [String], String)
argParser opts =
  case opts of
    Opts { optsMaster  = master
         , optsNodes   = nodes
         , optsService = service
         } -> return (master, splitOn "," nodes, service)
