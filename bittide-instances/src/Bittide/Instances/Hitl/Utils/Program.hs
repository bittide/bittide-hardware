-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedRecordDot #-}

module Bittide.Instances.Hitl.Utils.Program where

import Prelude

import Project.Handle

import Paths_bittide_instances

import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import System.IO
import System.Posix.Env (getEnvironment)
import System.Process

getOpenOcdStartPath :: IO FilePath
getOpenOcdStartPath = getDataFileName "data/openocd/start.sh"

getPicocomStartPath :: IO FilePath
getPicocomStartPath = getDataFileName "data/picocom/start.sh"

getTcpSprayPath :: IO FilePath
getTcpSprayPath = getDataFileName "data/tcpspray/start.sh"

data ProcessStdIoHandles = ProcessStdIoHandles
  { stdinHandle :: Handle
  , stdoutHandle :: Handle
  , stderrHandle :: Handle
  }

startOpenOcd :: String -> Int -> IO (ProcessStdIoHandles, IO ())
startOpenOcd usbLoc gdbPort = do
  startOpenOcdPath <- getOpenOcdStartPath
  currentEnv <- getEnvironment
  let
    openOcdProc =
      (proc startOpenOcdPath [])
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        , env = Just (currentEnv <> [("USB_DEVICE", usbLoc), ("GDB_PORT", show gdbPort)])
        }

  ocdHandles@(openOcdStdin, openOcdStdout, openOcdStderr, _openOcdPh) <-
    createProcess openOcdProc

  let
    ocdHandles' =
      ProcessStdIoHandles
        { stdinHandle = fromJust openOcdStdin
        , stdoutHandle = fromJust openOcdStdout
        , stderrHandle = fromJust openOcdStderr
        }

  pure (ocdHandles', cleanupProcess ocdHandles)

startGdb :: IO (ProcessStdIoHandles, IO ())
startGdb = do
  let
    gdbProc = (proc "gdb" []){std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}

  gdbHandles@(gdbStdin, gdbStdout, gdbStderr, _gdbPh) <-
    createProcess gdbProc

  let
    gdbHandles' =
      ProcessStdIoHandles
        { stdinHandle = fromJust gdbStdin
        , stdoutHandle = fromJust gdbStdout
        , stderrHandle = fromJust gdbStderr
        }

  pure (gdbHandles', cleanupProcess gdbHandles)

startPicocom :: FilePath -> IO (ProcessStdIoHandles, IO ())
startPicocom devPath = do
  startPicocomPath <- getPicocomStartPath

  let
    picocomProc =
      (proc startPicocomPath [devPath])
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        , new_session = True
        }

  picoHandles@(picoStdin, picoStdout, picoStderr, _picoPh) <-
    createProcess picocomProc

  let
    picoHandles' =
      ProcessStdIoHandles
        { stdinHandle = fromJust picoStdin
        , stdoutHandle = fromJust picoStdout
        , stderrHandle = fromJust picoStderr
        }

  pure (picoHandles', cleanupProcess picoHandles)

startPicocomWithLogging ::
  FilePath -> FilePath -> FilePath -> IO (ProcessStdIoHandles, IO ())
startPicocomWithLogging devPath stdoutPath stderrPath = do
  startPicocomPath <- getPicocomStartPath
  currentEnv <- getEnvironment

  let
    picocomProc =
      (proc startPicocomPath [devPath])
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        , new_session = True
        , env =
            Just
              (currentEnv <> [("PICOCOM_STDOUT_LOG", stdoutPath), ("PICOCOM_STDERR_LOG", stderrPath)])
        }

  picoHandles@(picoStdin, picoStdout, picoStderr, _picoPh) <-
    createProcess picocomProc

  let
    picoHandles' =
      ProcessStdIoHandles
        { stdinHandle = fromJust picoStdin
        , stdoutHandle = fromJust picoStdout
        , stderrHandle = fromJust picoStderr
        }

  pure (picoHandles', cleanupProcess picoHandles)

-- | Wait until we see "Halting processor", fail if we see an error.
openOcdWaitForHalt :: String -> Filter
openOcdWaitForHalt s
  | "Error:" `isPrefixOf` s = Stop (Error ("Found error in OpenOCD output: " <> s))
  | "Halting processor" `isPrefixOf` s = Stop Ok
  | otherwise = Continue
