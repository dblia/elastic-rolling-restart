{-
  Options Parser module.
-}

module OptsParser
  ( Opts(..)
  , optParser
  , parseOpts
  , argParser
  ) where

import Data.List.Split     (splitOn)
import Options.Applicative


-- | Command line arguments.
data Opts = Opts
  { optsMaster :: String
  , optsNodes  :: String
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


parseOpts :: IO Opts
parseOpts = execParser $ info optParser $
     fullDesc
  <> progDesc "ElasticSearch cluster rolling restart tool."


argParser :: Opts -> IO (String, [String])
argParser opts =
  case opts of
    Opts { optsMaster = master
         , optsNodes  = nodes
         } -> return (master, splitOn "," nodes)
