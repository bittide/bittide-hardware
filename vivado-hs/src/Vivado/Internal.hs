-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Vivado.Internal where

import Prelude

import Control.Exception (Exception, finally, throwIO)
import Control.Monad (forM_, unless, void)
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.List.Extra (isPrefixOf, splitOn, trim)
import Data.Maybe (fromJust)
import Data.Sequence (Seq ((:|>)))
import Data.String.Interpolate (__i)
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)
import System.Directory.Extra (removeFile)
import System.Environment (getEnv, setEnv)
import System.IO (Handle)
import System.Process

import System.IO qualified as IO
import System.IO.Temp qualified as Temp

data VivadoHandle = VivadoHandle
  { stdin :: Handle
  -- ^ Handle to write to
  , stdout :: Handle
  -- ^ Handle to read from. Note that TCL does not use stderr.
  , process :: ProcessHandle
  , logHandle :: Handle
  -- ^ Handle to full log: everything that Vivado writes to stdout, and everything
  -- that we write to Vivado.
  , logPath :: FilePath
  , prettyLogHandle :: Handle
  -- ^ Handle to pretty log: everything that Vivado writes to stdout, and everything
  -- that we write to Vivado, but without the magic and error handling cruft.
  , prettyLogPath :: FilePath
  }

data TclException = TclException
  { cmd :: String
  , stdout :: String
  , retCode :: ErrorCode
  , errMsg :: String
  , logPath :: String
  , prettyLogPath :: String
  }
  deriving (Typeable)

instance Show TclException where
  show :: TclException -> String
  show (TclException{..}) =
    "TclException: "
      <> [__i|
    Got return code #{retCode} while executing:

      #{cmd}

    Error message was:

      #{errMsg}

    Output before and during the crash:

      #{trim stdout}

    Log up to the crash:

      Full: #{logPath}
      Pretty: #{prettyLogPath}
  |]

instance Exception TclException

{- | Magic string that we'll instruct Vivado to echo back to us to signal that
it has finished processing a command.
-}
magic :: String
magic = "471ac6f71ba9bd7982741d53edfe809d50f43035645fe99f890761b2bf1ef6bfac18"

type ErrorCode = String
data Filter = Continue | Stop | StopWithError ErrorCode

{- | Utility function that reads lines from a handle, and applies a filter to
each line. If the filter returns 'Continue', the function will continue
reading lines. If the filter returns @Stop Ok@, the function will return
successfully. If the filter returns @Stop e@, the function will return a
'Just' with the supplied error message.
-}
expectLine ::
  (HasCallStack) =>
  VivadoHandle ->
  (String -> IO Filter) ->
  IO (Seq String, Maybe ErrorCode)
expectLine v f = go mempty
 where
  go :: Seq String -> IO (Seq String, Maybe ErrorCode)
  go acc = do
    line <- IO.hGetLine v.stdout

    IO.hPutStrLn v.logHandle line
    unless (magic `isPrefixOf` line) $
      IO.hPutStrLn v.prettyLogHandle line

    let lines' = acc :|> line
    f line >>= \case
      Continue -> go lines'
      Stop -> return (lines', Nothing)
      StopWithError code -> return (lines', Just code)

-- | Write a line to the Vivado handle
writeLine :: VivadoHandle -> String -> String -> IO ()
writeLine v prettyS s = do
  forM_ (lines prettyS) $ \l -> IO.hPutStrLn v.prettyLogHandle (">>> " <> l)
  forM_ (lines s) $ \l -> IO.hPutStrLn v.logHandle (">>> " <> l)
  IO.hPutStrLn v.stdin s

{- | Execute a command in Vivado and return the resulting standard output and
the command result.

Careful: do not use this function with unverified user input, as it does not
attempt to sanitize the input.
-}
exec :: VivadoHandle -> String -> IO (String, String)
exec v cmd = do
  -- handle exceptionHandler $ do
  -- Write a line that would let Vivado run the command in a catch construct and
  -- print our magic string after that. If an error occurred, the return code is
  -- included and what is returned is the error message. Otherwise the command
  -- result is returned.
  writeLine
    v
    cmd
    [__i|
    if { [catch {#{cmd}} result_#{magic} opt_dict_#{magic}] } {
      puts {}
      puts -nonewline {#{magic} ERR }
      puts [dict get $opt_dict_#{magic} {-code}]
      puts $result_#{magic}
    } else {
      puts {}
      puts {#{magic} OK}
      puts $result_#{magic}
    }
  |]

  -- Discard the line with the magic string at the end
  (stdout :|> _, mErr) <- expectLine v filtUntilMagic
  let stdoutS = intercalate "\n" $ toList stdout

  -- The return value
  (retVal, _) <- expectLine v filtUntilEnd
  let retValS = intercalate "\n" $ toList retVal

  case mErr of
    Nothing ->
      return (stdoutS, retValS)
    Just returnCode -> do
      throwIO
        ( TclException
            { cmd = cmd
            , stdout = stdoutS
            , retCode = returnCode
            , errMsg = retValS
            , logPath = v.logPath
            , prettyLogPath = v.prettyLogPath
            }
        )
 where
  filtUntilMagic :: String -> IO Filter
  filtUntilMagic line
    | magic `isPrefixOf` line = case splitOn " " line of
        [_magic, "OK"] -> return Stop
        [_magic, "ERR", code] -> return (StopWithError code)
        _ -> error $ "Unexpected magic string format: " <> line
    | otherwise = return Continue

  filtUntilEnd :: String -> IO Filter
  filtUntilEnd _ = do
    inputAvailable <- IO.hReady v.stdout
    return $ if inputAvailable then Continue else Stop

{- | Execute a command in Vivado and ignore the command result.

Careful: do not use this function with unverified user input, as it does not
attempt to sanitize the input.
-}
exec_ :: VivadoHandle -> String -> IO ()
exec_ v cmd = void (exec v cmd)

{- | Execute a command in Vivado, print the resulting standard output and return
the command result.

Careful: do not use this function with unverified user input, as it does not
attempt to sanitize the input.
-}
execPrint :: VivadoHandle -> String -> IO String
execPrint v cmd = do
  (stdout, result) <- exec v cmd
  putStr stdout
  return result

{- | Execute a command in Vivado, print the resulting standard output and ignore
the command result.

Careful: do not use this function with unverified user input, as it does not
attempt to sanitize the input.
-}
execPrint_ :: VivadoHandle -> String -> IO ()
execPrint_ v cmd = do
  (stdout, _) <- exec v cmd
  putStr stdout

{- | Run a block of code with a Vivado handle. Example usage:

> import qualified Vivado as V
>
> V.with $ \v -> do
>   output <- V.exec v "puts hello"
>   putStrLn output
-}
with :: (VivadoHandle -> IO a) -> IO a
with f = do
  systemTmpDirEnv <- getEnv "VIVADO_HS_LOG_DIR"
  systemTmpDir <-
    if systemTmpDirEnv == ""
      then Temp.getCanonicalTemporaryDirectory
      else pure systemTmpDirEnv
  putStrLn $ "Using tmpdir: " <> systemTmpDir
  (logPath, logHandle) <- Temp.openTempFile systemTmpDir "vivado-hs.log"
  putStrLn $ "Using log path: " <> logPath
  (prettyLogPath, prettyLogHandle) <- Temp.openTempFile systemTmpDir "pretty-vivado-hs.log"
  putStrLn $ "Using pretty log path: " <> prettyLogPath

  a <-
    finally
      -- do:
      ( do
          setEnv "XILINX_LOCAL_USER_DATA" "no" -- Prevents multiprocessing issues
          withCreateProcess vivadoProc $
            \(fromJust -> stdin) (fromJust -> stdout) _stderr process -> do
              IO.hSetBuffering stdout IO.LineBuffering
              IO.hSetBuffering stdin IO.LineBuffering
              let v = VivadoHandle{..}
              f v
      )
      -- finally:
      ( do
          IO.hClose logHandle
          IO.hClose prettyLogHandle
      )

  -- Remove log files if there weren't any exceptions
  removeFile logPath
  removeFile prettyLogPath

  pure a
 where
  vivadoProc =
    (proc "vivado" ["-mode", "tcl"])
      { std_in = CreatePipe
      , std_out = CreatePipe
      }
