-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}

module Gdb where

import Prelude hiding (last)

import Control.Concurrent (withMVar)
import Control.Monad (void)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromJust)
import Data.String.Interpolate (i)
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import GHC.Stack (HasCallStack)
import Numeric (showHex)
import Project.Handle (
  Error (Error, Ok),
  readUntil,
 )
import System.IO (BufferMode (LineBuffering), Handle, hPutStrLn, hSetBuffering)
import System.Posix (sigINT, signalProcess)
import System.Process (
  CreateProcess (std_err, std_in, std_out),
  StdStream (CreatePipe),
  cleanupProcess,
  createProcess,
  proc,
  waitForProcess,
 )
import System.Process.Internals (
  ProcessHandle (ProcessHandle, phandle),
  ProcessHandle__ (ClosedHandle, OpenExtHandle, OpenHandle),
 )
import System.Timeout.Extra (tryWithTimeout)
import "bittide-extra" Control.Exception.Extra (brackets, preferMainBracket)

import qualified Data.List as L
import qualified "extra" Data.List.Extra as L

data Gdb = Gdb
  { stdin :: !Handle
  , stdout :: !Handle
  , stderr :: !Handle
  , process :: !ProcessHandle
  }

{- | Starts GDB and performs an action. At the end, whether an exception occurred
or not, it will terminate the GDB process. If it cannot terminate the process
it will throw an exception. If both the main action and cleanup thrown an
exception, the main action's exception will be thrown.
-}
withGdb :: (MonadIO m, MonadMask m, HasCallStack) => (Gdb -> m a) -> m a
withGdb = preferMainBracket (liftIO start) (liftIO . stop)

-- | Like 'withGdb', but spawns multiple processes
withGdbs :: (MonadIO m, MonadMask m, HasCallStack) => Int -> ([Gdb] -> m a) -> m a
withGdbs n = brackets (L.replicate n (liftIO start)) (liftIO . stop)

{- | Start a GDB process and pipe its stdin/stdout/stderr. Note that it is always
preferable to use one of 'withGdb' / 'withGdbs'.
-}
start :: (HasCallStack) => IO Gdb
start = do
  (gdbStdin, gdbStdout, gdbStderr, gdbPh) <-
    createProcess $
      (proc "gdb" [])
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }

  let
    gdb =
      Gdb
        { stdin = fromJust gdbStdin
        , stdout = fromJust gdbStdout
        , stderr = fromJust gdbStderr
        , process = gdbPh
        }

  hSetBuffering gdb.stdin LineBuffering
  hSetBuffering gdb.stdout LineBuffering

  pure gdb

{- | Clean up a GDB process. GDB is asked nicely to quit and waited for. If it
doesn't within 15 seconds, an exception is thrown. Note that this means the
process might still be running.
-}
stop :: (HasCallStack) => Gdb -> IO ()
stop gdb = do
  -- TODO: If GDB doesn't quit after asking nicely, kill it with force.
  cleanupProcess (Just gdb.stdin, Just gdb.stdout, Just gdb.stderr, gdb.process)
  exitCode <- tryWithTimeout "Stop GDB" 10_000_000 (waitForProcess gdb.process)
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure exitCode_ -> error [i|GDB exited with failure code #{exitCode_}|]

{- | Magic string that we'll instruct GDB to echo back to us to signal that
it has finished processing a command.
-}
magic :: String
magic = "8716da0aaa27b0074dd57b25287e7e4f952422378c65b809f1fa3e14056d62e64ebb"

{- | Read a command from GDB and return the raw output. The output is not
processed in any way, so it may contain leading and trailing whitespace.

Note that this function assumes only a single command is sent to GDB. If
multiple commands are sent, the output will contain REPL prompts such as
@(gdb)@.
-}
readCommandRaw :: (HasCallStack) => Gdb -> String -> IO String
readCommandRaw gdb cmd = do
  hPutStrLn gdb.stdin [i|echo \\n> #{cmd}\\n|]
  hPutStrLn gdb.stdin [i|echo #{magic}|]
  hPutStrLn gdb.stdin (L.trimEnd cmd)
  hPutStrLn gdb.stdin [i|echo #{magic}|]

  void $
    tryWithTimeout ("Wait for magic (start)") 15_000_000 $
      readUntil gdb.stdout [i|#{magic}(gdb) |]

  tryWithTimeout ("Wait for magic (end)") 15_000_000 $
    readUntil gdb.stdout [i|(gdb) #{magic}|]

{- | Read a command from GDB and return the output as a list of lines. Each line
is stripped of trailing whitespace (see 'Data.Char.isSpace'). Any leading and
trailing empty lines are removed. See 'readCommandRaw' if you need access to
the raw output.
-}
readCommand :: (HasCallStack) => Gdb -> String -> IO [String]
readCommand gdb cmd = do
  output <- readCommandRaw gdb cmd
  pure
    . L.dropWhile (== "")
    . L.dropWhileEnd (== "")
    . L.map L.trimEnd
    . L.lines
    $ output

-- | Execute a command in GDB and read its output. Errors if there is output.
readCommand0 :: (HasCallStack) => Gdb -> String -> IO ()
readCommand0 gdb cmd = do
  result <- readCommand gdb cmd
  case result of
    [] -> return ()
    ls -> error [i|Command '#{cmd}' returned one or more lines, expected no output: #{ls}|]

{- | Read a single line of output from GDB after executing a command. Errors if
the output is not exactly one line.
-}
readCommand1 :: (HasCallStack) => Gdb -> String -> IO String
readCommand1 gdb cmd = do
  result <- readCommand gdb cmd
  case result of
    [] -> error "GDB returned no output, expected at least one line"
    [l] -> return l
    ls -> error [i|Command '#{cmd}' returned multiple lines, expected one: #{ls}|]

-- | Like 'readCommand', but ignore output
runCommand :: (HasCallStack) => Gdb -> String -> IO ()
runCommand gdb cmd = void (readCommandRaw gdb cmd)

-- | Like 'runCommand', but takes a list of commands and runs them sequentially.
runCommands :: (HasCallStack) => Gdb -> [String] -> IO ()
runCommands gdb commands = mapM_ (runCommand gdb) commands

-- | Make GDB echo a string to its output
echo :: (HasCallStack) => Gdb -> String -> IO ()
echo gdb s = runCommand gdb [i|echo \\n#{s}\\n|]

continue :: (HasCallStack) => Gdb -> IO ()
continue gdb =
  -- Note that we can't use 'readCommand' here, because this command won't return
  hPutStrLn gdb.stdin "continue"

{- | Send @SIGINT@ to the GDB process. This will pause the execution of the
program being debugged and return control to GDB.
-}
interrupt :: (HasCallStack) => Gdb -> IO ()
interrupt gdb =
  case gdb.process of
    ProcessHandle{phandle} ->
      withMVar phandle $ \ph ->
        case ph of
          OpenHandle pid -> signalProcess sigINT pid
          OpenExtHandle{} -> error "Not supported: OpenExtHandle is a Windows-only handle"
          ClosedHandle{} -> error "Process handle is closed"

{- | Load a preset binary onto the CPU. This will also run 'compareSections' to
ensure that the binary was loaded correctly.
-}
loadBinary :: (HasCallStack) => Gdb -> IO Error
loadBinary gdb = do
  output <- readCommand gdb "load"

  if
    | any ("Remote communication error." `L.isPrefixOf`) output ->
        pure $ Error [i|GDB remote communication error: #{output}|]
    | any ("Start address 0x80000000" `L.isPrefixOf`) output ->
        compareSections gdb
    | otherwise -> do
        pure $ Error [i|Loading binary failed, unexpected output: #{output}|]

{- | Runs "compare-sections" in gdb and parses the resulting output.
If any of the hash values do not match, this function will throw an error.
-}
compareSections :: (HasCallStack) => Gdb -> IO Error
compareSections gdb = do
  output <- readCommand gdb "compare-sections"
  if all isMatch output
    then pure Ok
    else pure $ Error [i|Unexpected output from compare-sections: #{output}|]
 where
  isMatch :: String -> Bool
  isMatch s =
    case words s of
      ["Section", _name, "range", _start, "--", _end, "matched."] -> True
      _ -> False

-- | Enables logging to a file in gdb.
setLogging :: (HasCallStack) => Gdb -> FilePath -> IO ()
setLogging gdb logPath = do
  runCommands gdb $
    [ "set logging file " <> logPath
    , "set logging overwrite on"
    , "set logging enabled on"
    ]

dumpMemoryRegion ::
  (HasCallStack) =>
  -- | GDB process handles
  Gdb ->
  -- | File path to dump the memory region to (binary format)
  FilePath ->
  -- | Start address of the memory region
  Integer ->
  -- | End address of the memory region (exclusive)
  Integer ->
  IO ()
dumpMemoryRegion gdb filePath start_ end = readCommand0 gdb cmd
 where
  cmd = [i|dump binary memory #{filePath} 0x#{showHex start_ ""} 0x#{showHex end ""}|]

-- | Sets the target to be debugged in gdb, must be a port number.
setTarget :: (HasCallStack) => Gdb -> Int -> IO ()
setTarget gdb port = do
  runCommand gdb ("target extended-remote :" <> show port)

-- | Sets the file to be debugged in gdb.
setFile :: (HasCallStack) => Gdb -> FilePath -> IO ()
setFile gdb filePath = do
  runCommand gdb ("file " <> filePath)

setTimeout :: (HasCallStack) => Gdb -> Maybe Int -> IO ()
setTimeout gdb Nothing =
  runCommand gdb ("set remotetimeout unlimited")
setTimeout gdb (Just (show -> time)) = do
  runCommand gdb ("set remotetimeout " <> time)

-- | Sets breakpoints on functions in gdb.
setBreakpoints :: (HasCallStack) => Gdb -> [String] -> IO ()
setBreakpoints gdb = runCommands gdb . fmap ("break " <>)

{- | Sets a hook to run when a breakpoint is hit.
The hook will print the register values, backtrace, and quit gdb.
-}
setBreakpointHook :: (HasCallStack) => Gdb -> IO ()
setBreakpointHook gdb =
  runCommand gdb . unlines $
    [ "define hook-stop"
    , "printf \"!!! program stopped executing !!!\\n\""
    , "i r"
    , "bt"
    , "end"
    ]

{- | Execute a single command and read its (single line) output

XXX: It does more than that
-}
readSingleGdbValue :: (HasCallStack) => Gdb -> String -> String -> IO String
readSingleGdbValue gdb _value cmd = do
  output <- readCommand1 gdb cmd
  return
    . L.trim
    . L.drop 2
    . L.dropWhile (/= ':')
    $ output
