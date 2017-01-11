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

import Options.Applicative


-- | Command-line arguments definition.
data Opts = Opts
  { optsHost :: String      -- ^ the elasticsearch hostname
  , optsPort :: String      -- ^ port of the elasticsearch host
  , optsService :: String   -- ^ the elasticsearch service name
  } deriving (Eq, Ord, Show)


-- | Command-line arguments parser.
optParser :: Parser Opts
optParser = helper <*> _parser
  where
    _parser :: Parser Opts
    _parser = Opts
      <$> strOption
          ( long "host"
           <> short 'H'
           <> metavar "HOSTNAME"
           <> help "Hostname or IP of an elasticsearch node")
      <*> strOption
            ( long "port"
           <> value "9200"
           <> short 'p'
           <> metavar "PORT"
           <> help "Port of the elasticsearch host (default: 9200)")
      <*> strOption
            ( long "service"
           <> value "elasticsearch.service"
           <> short 's'
           <> metavar "SVC"
           <> help "The ES service name (default: elasticsearch.service)")

-- | Parse command-line arguments and add a custom description.
parseOpts :: IO Opts
parseOpts = execParser $ info optParser $
     fullDesc
  <> progDesc "ElasticSearch cluster rolling restart tool."

-- | Parse command-line arguments and return them properly.
argParser :: Opts -> IO (String, String)
argParser opts =
  case opts of
    Opts { optsHost = host
         , optsPort = port
         , optsService = service
         } -> return (host ++ ":" ++  port, service)
