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
import Data.List.Extra (isPrefixOf, splitOn, trim)
import Data.Maybe (fromJust)
import Data.Sequence (Seq ((:|>)))
import Data.String.Interpolate (__i)
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)
import System.Directory.Extra (removeFile)
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
  , stdout :: [String]
  , error :: String
  , logPath :: String
  , prettyLogPath :: String
  }
  deriving (Typeable)

instance Show TclException where
  show :: TclException -> String
  show (TclException{error = e, ..}) =
    "TclException: "
      <> [__i|
    Got error while executing:

      #{cmd}

    Error was:

      #{e}

    Output before and during the crash:

      #{trim (unlines stdout)}

    Log up to the crash:

      Full: #{logPath}
      Pretty: #{prettyLogPath}
  |]

instance Exception TclException

{- | Magic string that Vivado will echo back to us to signal that it has
finished processing a command.
-}
magic :: String
magic = "471ac6f71ba9bd7982741d53edfe809d50f43035645fe99f890761b2bf1ef6bfac18"

data Error = Ok | Error String
data Filter = Continue | Stop Error

{- | Utility function that reads lines from a handle, and applies a filter to
each line. If the filter returns 'Continue', the function will continue
reading lines. If the filter returns @Stop Ok@, the function will return
successfully. If the filter returns @Stop e@, the function will return a
'Just' with the supplied error message.
-}
expectLine ::
  (HasCallStack) =>
  VivadoHandle ->
  (String -> Filter) ->
  IO (Seq String, Maybe String)
expectLine v f = go mempty
 where
  go lines0 = do
    line <- IO.hGetLine v.stdout
    IO.hPutStrLn v.logHandle line
    unless (magic `isPrefixOf` line) $
      IO.hPutStrLn v.prettyLogHandle line

    let
      lines1 = lines0 :|> line
      cont = go lines1
    if null line
      then cont
      else case f line of
        Continue -> cont
        Stop Ok -> pure (lines1, Nothing)
        Stop (Error e) -> pure (lines1, Just e)

-- | Write a line to the Vivado handle
writeLine :: VivadoHandle -> String -> String -> IO ()
writeLine v prettyS s = do
  forM_ (lines prettyS) $ \l -> IO.hPutStrLn v.prettyLogHandle (">>> " <> l)
  forM_ (lines s) $ \l -> IO.hPutStrLn v.logHandle (">>> " <> l)
  IO.hPutStrLn v.stdin s

{- | Execute a command in Vivado and return the output

Careful: do not use this function with unverified user input, as it does not
attempt to sanitize the input.
-}
exec :: VivadoHandle -> String -> IO String
exec v cmd = do
  writeLine
    v
    cmd
    [__i|
    if { [catch {#{cmd}} error_#{magic}] } {
      puts -nonewline {#{magic} }
      puts -nonewline {ERR }
      puts $error_#{magic}
    } else {
      puts {#{magic} OK}
    }
  |]

  expectLine v go >>= \case
    (stdout, Just e) -> do
      IO.hPutStrLn v.prettyLogHandle e
      throwIO
        ( TclException
            { cmd
            , stdout = toList stdout
            , error = e
            , logPath = v.logPath
            , prettyLogPath = v.prettyLogPath
            }
        )
    (stdout, Nothing) -> pure (unlines (toList (seqInit stdout)))
 where
  seqInit (x :|> _) = x
  seqInit x = x

  go :: String -> Filter
  go s =
    case splitOn " " (trim s) of
      (w : ws) | w == magic ->
        case ws of
          ["OK"] -> Stop Ok
          ("ERR" : err) -> Stop (Error (unwords err))
          [] -> error "Internal error: unexpected bare magic string"
          err -> error $ "Internal error: unexpected magic string arguments: " <> unwords err
      _ -> Continue

{- | Execute a command in Vivado, ignore its output

Careful: do not use this function with unverified user input, as it does not
attempt to sanitize the input.
-}
exec_ :: VivadoHandle -> String -> IO ()
exec_ v cmd = void (exec v cmd)

{- | Run a block of code with a Vivado handle. Example usage:

> import qualified Vivado as V
>
> V.with $ \v -> do
>   output <- V.exec v "puts hello"
>   putStrLn output
-}
with :: (VivadoHandle -> IO a) -> IO a
with f = do
  systemTmpDir <- Temp.getCanonicalTemporaryDirectory
  (logPath, logHandle) <- Temp.openTempFile systemTmpDir "vivado-hs.log"
  (prettyLogPath, prettyLogHandle) <- Temp.openTempFile systemTmpDir "pretty-vivado-hs.log"

  a <-
    finally
      -- do:
      ( withCreateProcess vivadoProc $
          \(fromJust -> stdin) (fromJust -> stdout) _stderr process -> do
            IO.hSetBuffering stdout IO.LineBuffering
            IO.hSetBuffering stdin IO.LineBuffering
            let v = VivadoHandle{..}
            exec_ v "puts init"
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
