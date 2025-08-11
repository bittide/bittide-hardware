-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}

module Gdb where

import Prelude

import Control.Concurrent (withMVar)
import Control.Monad (forM_)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromJust, fromMaybe)
import Data.String.Interpolate (i)
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import GHC.Stack (HasCallStack)
import Numeric (showHex)
import Project.Handle (
  Error (Error, Ok),
  Filter (Continue, Stop),
  expectLine,
  readUntil,
  readUntilLine,
  waitForLine,
 )
import System.IO (BufferMode (LineBuffering), Handle, hPutStrLn, hSetBuffering)
import System.Posix (sigINT, signalProcess)
import System.Process
import System.Process.Internals (
  ProcessHandle (ProcessHandle, phandle),
  ProcessHandle__ (ClosedHandle, OpenExtHandle, OpenHandle),
 )
import System.Timeout (timeout)
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

runCommands :: Handle -> [String] -> IO ()
runCommands h commands =
  forM_ commands $ \command -> do
    putStrLn $ "gdb-in: " <> command
    hPutStrLn h command

echo :: Handle -> String -> IO ()
echo h s = runCommands h ["echo \\n" <> s <> "\\n"]

-- | Strip any leading @(gdb)@ and whitespace from a line
stripOutput :: String -> String
stripOutput s0 = s3
 where
  s1 = L.trim s0
  s2 = fromMaybe s1 (L.stripPrefix "(gdb)" s1)
  s3 = L.trim s2

continue :: Gdb -> IO ()
continue gdb = runCommands gdb.stdin ["continue"]

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
loadBinary :: Gdb -> IO Error
loadBinary gdb = do
  runCommands gdb.stdin ["load"]
  let
    expectedResponse s
      | "Remote communication error." `L.isPrefixOf` s =
          Stop (Error ("GDB remote communication error: " <> s))
      | "Start address 0x80000000" `L.isPrefixOf` s = Stop Ok
      | otherwise = Continue
  result <- timeout 60_000_000 $ expectLine gdb.stdout expectedResponse
  case result of
    Just _ -> compareSections gdb
    Nothing -> pure $ Error "Loading binary timed out"

{- | Runs "compare-sections" in gdb and parses the resulting output.
If any of the hash values do not match, this function will throw an error.
-}
compareSections :: Gdb -> IO Error
compareSections gdb = do
  let
    startMsg = "Comparing sections"
    doneMsg = "Comparing done"
  echo gdb.stdin startMsg
  waitForLine gdb.stdout startMsg
  runCommands gdb.stdin ["compare-sections"]
  echo gdb.stdin doneMsg
  sectionLines <- readUntilLine gdb.stdout doneMsg
  mapM_ (putStrLn . ("Got: " <>)) sectionLines
  pure $
    if all isMatchOrEmpty sectionLines
      then Ok
      else Error "Some sections did not match"
 where
  isMatchOrEmpty :: String -> Bool
  isMatchOrEmpty (stripOutput -> s) =
    case words s of
      ["Section", _name, "range", _start, "--", _end, "matched."] -> True
      [] -> True
      _ -> False

-- | Enables logging to a file in gdb.
setLogging :: Gdb -> FilePath -> IO ()
setLogging gdb logPath = do
  runCommands gdb.stdin $
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
dumpMemoryRegion gdb filePath start_ end = do
  runCommands
    gdb.stdin
    [ [i|dump binary memory #{filePath} 0x#{showHex start_ ""} 0x#{showHex end ""}|]
    , "echo dump_done\\n"
    ]

  let
    expectedResponse "(gdb) (gdb) dump_done" = Stop Ok
    expectedResponse s = Stop (Error ("Unexpected response: " <> s))

  result <- timeout 60_000_000 $ expectLine gdb.stdout expectedResponse
  pure $ case result of
    Just () -> ()
    Nothing -> error "Dumping samples timed out"

-- | Sets the target to be debugged in gdb, must be a port number.
setTarget :: Gdb -> Int -> IO ()
setTarget gdb port = do
  runCommands gdb.stdin ["target extended-remote :" <> show port]

-- | Sets the file to be debugged in gdb.
setFile :: Gdb -> FilePath -> IO ()
setFile gdb filePath = do
  runCommands gdb.stdin ["file " <> filePath]

setTimeout :: Gdb -> Maybe Int -> IO ()
setTimeout gdb Nothing = do
  runCommands gdb.stdin ["set remotetimeout unlimited"]
setTimeout gdb (Just (show -> time)) = do
  runCommands gdb.stdin ["set remotetimeout " <> time]

-- | Sets breakpoints on functions in gdb.
setBreakpoints :: Gdb -> [String] -> IO ()
setBreakpoints gdb breakpoints = do
  let echoMsg = "breakpoints set"
  runCommands gdb.stdin $ (fmap ("break " <>) breakpoints)
  echo gdb.stdin echoMsg
  result <- timeout 10_000_000 $ readUntil gdb.stdout echoMsg
  case result of
    Just _ -> pure ()
    Nothing -> error "Setting breakpoints timed out"

{- | Sets a hook to run when a breakpoint is hit.
The hook will print the register values, backtrace, and quit gdb.
-}
setBreakpointHook :: Gdb -> IO ()
setBreakpointHook gdb = do
  runCommands
    gdb.stdin
    [ "define hook-stop"
    , "printf \"!!! program stopped executing !!!\\n\""
    , "i r"
    , "bt"
    , "quit 1"
    , "end"
    ]

-- | Execute a single command and read its (single line) output
readSingleGdbValue :: Gdb -> String -> String -> IO String
readSingleGdbValue gdb value cmd = do
  let
    startString = "START OF READ (" <> value <> ")"
    endString = "END OF READ (" <> value <> ")"
  runCommands
    gdb.stdin
    [ "printf \"" <> startString <> "\\n\""
    , cmd
    , "printf \"" <> endString <> "\\n\""
    ]
  _ <-
    tryWithTimeout ("GDB read prepare: " <> value) 15_000_000 $
      readUntil gdb.stdout startString
  untrimmed <-
    tryWithTimeout ("GDB read: " <> value) 15_000_000 $
      readUntil gdb.stdout endString
  let
    trimmed = L.trim untrimmed
    gdbLines = L.lines trimmed
    outputLine = fromJust $ L.find ("(gdb)" `L.isPrefixOf`) gdbLines
    untilColon = L.dropWhile (/= ':') outputLine
    lineFinal = L.trim $ L.drop 2 untilColon
  return lineFinal
