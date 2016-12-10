{-| Utility functions for managing processes.

-}

module Process
  ( runCmd                  -- :: String -> Maybe String -> IO String
  ) where

import qualified Control.Exception as E

import Data.List.Split (splitOn)
import System.Exit
import System.Process  (readProcessWithExitCode)
import Text.Printf     (printf)

import Constants        as C


-- | Exit the program in case of an error, or return the result value.
exitIfErr :: (ExitCode, String, String) -> IO String
exitIfErr (ExitSuccess, stdout, _) = return stdout
exitIfErr (ExitFailure rcode, _, stderr) =
  fail $ printf "Command failed with exit code %s: %s" (show rcode) stderr

-- | Prepares the given command to be executed by `readProcessWithExitCode`.
-- This function receives the command as a String and an optional host name to
-- execute the command, and returns the command as a tuple of the executable's
-- name and the command's list of arguments.
prepareCommand :: String -> Maybe String -> (FilePath, [String])
prepareCommand command hostname =
  let cmd = splitOn " " command
  in case hostname of
    Just host -> (C.cmdSSH, C.sshParams ++ [host] ++ cmd)
    Nothing   -> (head cmd, tail cmd)

-- | Execute the given shell command.
-- The command can be executed either locally, or on a remote host via SSH.
runCmd :: String          -- ^ Command to be executed
       -> Maybe String    -- ^ Host to execute the command via SSH (optional)
       -> IO String       -- ^ Output of the command
runCmd command target = do
  let (cmd, params) = prepareCommand command target
  (E.try $ readProcessWithExitCode cmd params stdin ::
    IO (Either IOError (ExitCode, String, String))) >>= either ioError exitIfErr
  where stdin :: String
        stdin = ""
