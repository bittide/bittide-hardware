-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedRecordDot #-}

module Bittide.Instances.Hitl.Utils.Program where

import Prelude

import Project.Handle

import Paths_bittide_instances

import Control.Exception
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import System.IO
import System.Posix.Env (getEnvironment)
import System.Process
import System.Timeout (timeout)

brackets :: [IO a] -> (a -> IO b) -> ([a] -> IO c) -> IO c
brackets acqs rel act = go [] acqs
 where
  go resL [] = act (reverse resL)
  go resL (acq : acqs1) = bracket acq rel $ \res -> go (res : resL) acqs1

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

awaitProcessTermination :: String -> ProcessHandle -> Maybe Int -> IO ()
awaitProcessTermination name h Nothing = do
  putStrLn $ "Waiting for process " <> name <> " to terminate"
  _ <- waitForProcess h
  return ()
awaitProcessTermination name h (Just t) = do
  putStrLn $ "Waiting for process " <> name <> " to terminate"
  result <- timeout t $ waitForProcess h
  case result of
    Just _ -> return ()
    Nothing -> error "Waiting for pocess termination timed out."

withOpenOcd :: String -> Int -> Int -> Int -> (ProcessStdIoHandles -> IO a) -> IO a
withOpenOcd usbLoc gdbPort tclPort telnetPort action = do
  (ocd, clean) <- startOpenOcd usbLoc gdbPort tclPort telnetPort
  finally (action ocd) clean

startOpenOcd :: String -> Int -> Int -> Int -> IO (ProcessStdIoHandles, IO ())
startOpenOcd usbLoc gdbPort tclPort telnetPort = do
  startOpenOcdPath <- getOpenOcdStartPath
  currentEnv <- getEnvironment
  let
    openOcdProc =
      (proc startOpenOcdPath [])
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        , env =
            Just
              ( currentEnv
                  <> [ ("USB_DEVICE", usbLoc)
                     , ("GDB_PORT", show gdbPort)
                     , ("TCL_PORT", show tclPort)
                     , ("TELNET_PORT", show telnetPort)
                     ]
              )
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

startOpenOcdWithEnv ::
  [(String, String)] ->
  String ->
  Int ->
  Int ->
  Int ->
  IO (ProcessStdIoHandles, ProcessHandle, IO ())
startOpenOcdWithEnv extraEnv usbLoc gdbPort tclPort telnetPort = do
  startOpenOcdPath <- getOpenOcdStartPath
  currentEnv <- getEnvironment
  let
    openOcdProc =
      (proc startOpenOcdPath [])
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        , env =
            Just
              ( currentEnv
                  <> extraEnv
                  <> [ ("USB_DEVICE", usbLoc)
                     , ("GDB_PORT", show gdbPort)
                     , ("TCL_PORT", show tclPort)
                     , ("TELNET_PORT", show telnetPort)
                     ]
              )
        }

  ocdHandles@(openOcdStdin, openOcdStdout, openOcdStderr, openOcdPh) <-
    createProcess openOcdProc

  let
    ocdHandles' =
      ProcessStdIoHandles
        { stdinHandle = fromJust openOcdStdin
        , stdoutHandle = fromJust openOcdStdout
        , stderrHandle = fromJust openOcdStderr
        }

  pure (ocdHandles', openOcdPh, cleanupProcess ocdHandles)

withGdb :: (ProcessStdIoHandles -> IO a) -> IO a
withGdb action = do
  (gdb, clean) <- startGdb
  finally (action gdb) clean

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

withPicocomWithLogging ::
  FilePath ->
  FilePath ->
  FilePath ->
  (ProcessStdIoHandles -> IO a) ->
  IO a
withPicocomWithLogging devPath stdoutPath stderrPath action = do
  (pico, clean) <- startPicocomWithLogging devPath stdoutPath stderrPath
  finally (action pico) clean

withPicocomWithLoggingAndEnv ::
  FilePath ->
  FilePath ->
  FilePath ->
  [(String, String)] ->
  (ProcessStdIoHandles -> IO a) ->
  IO a
withPicocomWithLoggingAndEnv devPath stdoutPath stderrPath extraEnv action = do
  (pico, clean) <- startPicocomWithLoggingAndEnv devPath stdoutPath stderrPath extraEnv
  finally (action pico) clean

startPicocomWithLogging ::
  FilePath -> FilePath -> FilePath -> IO (ProcessStdIoHandles, IO ())
startPicocomWithLogging devPath stdoutPath stderrPath =
  startPicocomWithLoggingAndEnv devPath stdoutPath stderrPath []

startPicocomWithLoggingAndEnv ::
  FilePath ->
  FilePath ->
  FilePath ->
  [(String, String)] ->
  IO (ProcessStdIoHandles, IO ())
startPicocomWithLoggingAndEnv devPath stdoutPath stderrPath extraEnv = do
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
              ( currentEnv
                  <> extraEnv
                  <> [("PICOCOM_STDOUT_LOG", stdoutPath), ("PICOCOM_STDERR_LOG", stderrPath)]
              )
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

gdbWaitForLoad :: String -> Filter
gdbWaitForLoad s
  | "Remote communication error." `isPrefixOf` s =
      Stop (Error ("GDB remote communication error: " <> s))
  | "Start address 0x80000000" `isPrefixOf` s = Stop Ok
  | otherwise = Continue
