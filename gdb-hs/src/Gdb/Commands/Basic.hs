-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}

-- | Wrapper functions around common GDB commands
module Gdb.Commands.Basic where

import Prelude

import Control.Concurrent (withMVar)
import Data.String.Interpolate (i)
import GHC.Stack (HasCallStack)
import Numeric (showHex)
import Project.Handle (Error (Error, Ok))
import System.IO (hPutStrLn)
import System.Posix (sigINT, signalProcess)
import System.Process.Internals (
  ProcessHandle (ProcessHandle, phandle),
  ProcessHandle__ (ClosedHandle, OpenExtHandle, OpenHandle),
 )

import Gdb.Internal

import qualified Data.List as L

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

  if any ("Remote communication error." `L.isPrefixOf`) output
    then pure $ Error [i|GDB remote communication error: #{output}|]
    else compareSections gdb

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
