{-| Utility functions for managing processes.

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

module Elastic.RollingRestart.Utils.Process
  ( runCmd                  -- :: String -> Maybe String -> IO String
  ) where

import Elastic.RollingRestart.Constants as C

import qualified Control.Exception as E

import Data.List.Split (splitOn)
import System.Exit
import System.Process  (readProcessWithExitCode)
import Text.Printf     (printf)


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
  putStrLn $ printf "Runnning cmd: %s %s " cmd (unwords params)
  (E.try $ readProcessWithExitCode cmd params stdin ::
    IO (Either IOError (ExitCode, String, String))) >>= either ioError exitIfErr
  where stdin :: String
        stdin = ""
