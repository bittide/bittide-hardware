-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Bittide.Instances.Hitl.Utils.Gdb where

import Bittide.Instances.Hitl.Utils.Program
import Clash.Prelude
import Control.Monad (forM_)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Maybe (fromJust)
import Project.Handle
import System.IO
import System.Process
import System.Timeout

import qualified Data.List as L

withGdb :: (MonadIO m, MonadMask m) => (ProcessStdIoHandles -> m a) -> m a
withGdb action = do
  (gdb, clean) <- liftIO startGdb
  finally (action gdb) (liftIO clean)

startGdb :: IO (ProcessStdIoHandles, IO ())
startGdb = (\(a, _, c) -> (a, c)) <$> startGdbH

startGdbH :: IO (ProcessStdIoHandles, ProcessHandle, IO ())
startGdbH = do
  let
    gdbProc = (proc "gdb" []){std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}

  gdbHandles@(gdbStdin, gdbStdout, gdbStderr, gdbPh) <-
    createProcess gdbProc

  let
    gdbHandles' =
      ProcessStdIoHandles
        { stdinHandle = fromJust gdbStdin
        , stdoutHandle = fromJust gdbStdout
        , stderrHandle = fromJust gdbStderr
        }

  pure (gdbHandles', gdbPh, cleanupProcess gdbHandles)

runCommands :: Handle -> [String] -> IO ()
runCommands h commands =
  forM_ commands $ \command -> do
    putStrLn $ "gdb-in: " <> command
    hPutStrLn h command

echo :: Handle -> String -> IO ()
echo h s = runCommands h ["echo \\n" <> s <> "\\n"]

continue :: ProcessStdIoHandles -> IO ()
continue gdb = runCommands (stdinHandle gdb) ["continue"]

loadBinary :: ProcessStdIoHandles -> IO Error
loadBinary gdb = do
  runCommands gdb.stdinHandle ["load"]
  let
    expectedResponse s
      | "Remote communication error." `L.isPrefixOf` s =
          Stop (Error ("GDB remote communication error: " <> s))
      | "Start address 0x80000000" `L.isPrefixOf` s = Stop Ok
      | otherwise = Continue
  result <- timeout 60_000_000 $ expectLine gdb.stdoutHandle expectedResponse
  pure $ case result of
    Just _ -> Ok
    Nothing -> Error "Loading binary timed out"

{- | Runs "compare-sections" in gdb and parses the resulting output.
If any of the hash values do not match, this function will throw an error.
-}
compareSections :: ProcessStdIoHandles -> IO Error
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
    $ if all (\l -> parseSection l == Just "matched") sectionLines
      then Ok
      else Error "Some sections did not match"
 where
  parseSection :: String -> Maybe String
  parseSection s =
    case words s of
      ["Section", _name, "range", _start, "--", _end, result] -> Just $ L.init result
      _ -> Nothing

-- | Enables logging to a file in gdb.
setLogging :: ProcessStdIoHandles -> FilePath -> IO ()
setLogging gdb logPath = do
  runCommands gdb.stdinHandle
    $ [ "set logging file " <> logPath
      , "set logging overwrite on"
      , "set logging enabled on"
      ]

-- | Sets the target to be debugged in gdb, must be a port number.
setTarget :: ProcessStdIoHandles -> Int -> IO ()
setTarget gdb port = do
  runCommands gdb.stdinHandle ["target extended-remote :" <> show port]

-- | Sets the file to be debugged in gdb.
setFile :: ProcessStdIoHandles -> FilePath -> IO ()
setFile gdb filePath = do
  runCommands gdb.stdinHandle ["file " <> filePath]

setTimeout :: ProcessStdIoHandles -> Maybe Int -> IO ()
setTimeout gdb Nothing = do
  runCommands gdb.stdinHandle ["set remotetimeout unlimited"]
setTimeout gdb (Just (show -> time)) = do
  runCommands gdb.stdinHandle ["set remotetimeout " <> time]

-- | Sets breakpoints on functions in gdb.
setBreakpoints :: ProcessStdIoHandles -> [String] -> IO ()
setBreakpoints gdb breakpoints = do
  let echoMsg = "breakpoints set"
  runCommands gdb.stdinHandle $ (fmap ("break " <>) breakpoints)
  echo gdb.stdinHandle echoMsg
  result <- timeout 10_000_000 $ readUntil (stdoutHandle gdb) echoMsg
  case result of
    Just _ -> pure ()
    Nothing -> error "Setting breakpoints timed out"

{- | Sets a hook to run when a breakpoint is hit.
The hook will print the register values, backtrace, and quit gdb.
-}
setBreakpointHook :: ProcessStdIoHandles -> IO ()
setBreakpointHook gdb = do
  runCommands
    gdb.stdinHandle
    [ "define hook-stop"
    , "printf '!!! program stopped executing !!!\\n'"
    , "i r"
    , "bt"
    , "quit 1"
    , "end"
    ]
