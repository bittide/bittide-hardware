-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}

module Bittide.Instances.Hitl.Utils.Gdb where

import Bittide.Instances.Hitl.Utils.Program
import Clash.Prelude
import Control.Concurrent (withMVar)
import Control.Monad (forM_)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Maybe (fromJust, fromMaybe)
import Data.String.Interpolate (i)
import GHC.Stack (HasCallStack)
import Numeric (showHex)
import Project.Handle
import System.IO
import System.Posix (sigINT, signalProcess)
import System.Process
import System.Process.Internals (
  ProcessHandle (ProcessHandle, phandle),
  ProcessHandle__ (ClosedHandle, OpenExtHandle, OpenHandle),
 )
import System.Timeout

import qualified Data.List as L
import qualified "extra" Data.List.Extra as L

withGdb :: (MonadIO m, MonadMask m) => (ProcessHandles -> m a) -> m a
withGdb action = do
  (gdb, clean) <- liftIO startGdb
  finally (action gdb) (liftIO clean)

startGdb :: IO (ProcessHandles, IO ())
startGdb = (\(a, _, c) -> (a, c)) <$> startGdbH

startGdbH :: IO (ProcessHandles, ProcessHandle, IO ())
startGdbH = do
  let
    gdbProc = (proc "gdb" []){std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}

  gdbHandles@(gdbStdin, gdbStdout, gdbStderr, gdbPh) <-
    createProcess gdbProc

  let
    gdbHandles' =
      ProcessHandles
        { stdinHandle = fromJust gdbStdin
        , stdoutHandle = fromJust gdbStdout
        , stderrHandle = fromJust gdbStderr
        , process = gdbPh
        }

  pure (gdbHandles', gdbPh, cleanupProcess gdbHandles)

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

continue :: ProcessHandles -> IO ()
continue gdb = runCommands gdb.stdinHandle ["continue"]

{- | Send @SIGINT@ to the GDB process. This will pause the execution of the
program being debugged and return control to GDB.
-}
interrupt :: (HasCallStack) => ProcessHandles -> IO ()
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
loadBinary :: ProcessHandles -> IO Error
loadBinary gdb = do
  runCommands gdb.stdinHandle ["load"]
  let
    expectedResponse s
      | "Remote communication error." `L.isPrefixOf` s =
          Stop (Error ("GDB remote communication error: " <> s))
      | "Start address 0x80000000" `L.isPrefixOf` s = Stop Ok
      | otherwise = Continue
  result <- timeout 60_000_000 $ expectLine gdb.stdoutHandle expectedResponse
  case result of
    Just _ -> compareSections gdb
    Nothing -> pure $ Error "Loading binary timed out"

{- | Runs "compare-sections" in gdb and parses the resulting output.
If any of the hash values do not match, this function will throw an error.
-}
compareSections :: ProcessHandles -> IO Error
compareSections gdb = do
  let
    startMsg = "Comparing sections"
    doneMsg = "Comparing done"
  echo gdb.stdinHandle startMsg
  waitForLine gdb.stdoutHandle startMsg
  runCommands gdb.stdinHandle ["compare-sections"]
  echo gdb.stdinHandle doneMsg
  sectionLines <- readUntilLine gdb.stdoutHandle doneMsg
  mapM_ (putStrLn . ("Got: " <>)) sectionLines
  pure
    $ if all isMatchOrEmpty sectionLines
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
setLogging :: ProcessHandles -> FilePath -> IO ()
setLogging gdb logPath = do
  runCommands gdb.stdinHandle
    $ [ "set logging file " <> logPath
      , "set logging overwrite on"
      , "set logging enabled on"
      ]

dumpMemoryRegion ::
  (HasCallStack) =>
  -- | GDB process handles
  ProcessHandles ->
  -- | File path to dump the memory region to (binary format)
  FilePath ->
  -- | Start address of the memory region
  Integer ->
  -- | End address of the memory region (exclusive)
  Integer ->
  IO ()
dumpMemoryRegion gdb filePath start end = do
  runCommands
    gdb.stdinHandle
    [ [i|dump binary memory #{filePath} 0x#{showHex start ""} 0x#{showHex end ""}|]
    , "echo dump_done\\n"
    ]

  let
    expectedResponse "(gdb) (gdb) dump_done" = Stop Ok
    expectedResponse s = Stop (Error ("Unexpected response: " <> s))

  result <- timeout 60_000_000 $ expectLine gdb.stdoutHandle expectedResponse
  pure $ case result of
    Just () -> ()
    Nothing -> error "Dumping samples timed out"

-- | Sets the target to be debugged in gdb, must be a port number.
setTarget :: ProcessHandles -> Int -> IO ()
setTarget gdb port = do
  runCommands gdb.stdinHandle ["target extended-remote :" <> show port]

-- | Sets the file to be debugged in gdb.
setFile :: ProcessHandles -> FilePath -> IO ()
setFile gdb filePath = do
  runCommands gdb.stdinHandle ["file " <> filePath]

setTimeout :: ProcessHandles -> Maybe Int -> IO ()
setTimeout gdb Nothing = do
  runCommands gdb.stdinHandle ["set remotetimeout unlimited"]
setTimeout gdb (Just (show -> time)) = do
  runCommands gdb.stdinHandle ["set remotetimeout " <> time]

-- | Sets breakpoints on functions in gdb.
setBreakpoints :: ProcessHandles -> [String] -> IO ()
setBreakpoints gdb breakpoints = do
  let echoMsg = "breakpoints set"
  runCommands gdb.stdinHandle $ (fmap ("break " <>) breakpoints)
  echo gdb.stdinHandle echoMsg
  result <- timeout 10_000_000 $ readUntil gdb.stdoutHandle echoMsg
  case result of
    Just _ -> pure ()
    Nothing -> error "Setting breakpoints timed out"

{- | Sets a hook to run when a breakpoint is hit.
The hook will print the register values, backtrace, and quit gdb.
-}
setBreakpointHook :: ProcessHandles -> IO ()
setBreakpointHook gdb = do
  runCommands
    gdb.stdinHandle
    [ "define hook-stop"
    , "printf \"!!! program stopped executing !!!\\n\""
    , "i r"
    , "bt"
    , "quit 1"
    , "end"
    ]
