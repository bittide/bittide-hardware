-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

-- {-# OPTIONS -fplugin-opt=Protocols.Plugin:debug #-}

module Bittide.Instances.Hitl.Driver.VexRiscv where

import Clash.Prelude

import Project.FilePath
import Project.Handle

import Vivado
import Vivado.Tcl

import Bittide.Hitl
import Bittide.Instances.Hitl.Setup (demoRigInfo)
import Bittide.Instances.Hitl.Utils.Gdb
import Bittide.Instances.Hitl.Utils.Program
import Bittide.Instances.Hitl.Utils.Vivado

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch, displayException)
import Control.Monad (forM)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import System.Exit
import System.FilePath
import System.IO
import System.Timeout (timeout)

preProcessFunc ::
  VivadoHandle ->
  String ->
  FilePath ->
  HwTarget ->
  DeviceInfo ->
  IO (TestStepResult (ProcessStdIoHandles, ProcessStdIoHandles, ProcessStdIoHandles, IO ()))
preProcessFunc v _name ilaPath hwT deviceInfo = do
  openHwT v hwT
  execCmd_ v "set_property" ["PROBES.FILE", embrace ilaPath, "[current_hw_device]"]
  refresh_hw_device v []

  -- even though this is just pre-process step, the CPU is reset until
  -- the test_start signal is asserted and cannot be accessed via GDB otherwise
  execCmd_ v "set_property" ["OUTPUT_VALUE", "1", getProbeTestStartTcl]
  commit_hw_vio v ["[get_hw_vios]"]

  let targetId = idFromHwT hwT
  let targetIndex = fromMaybe 9 $ L.findIndex (\d -> d.deviceId == targetId) demoRigInfo
  let gdbPort = 3333 + targetIndex

  (ocd, ocdClean) <- startOpenOcd deviceInfo.usbAdapterLocation gdbPort

  -- make sure OpenOCD is started properly

  hSetBuffering ocd.stderrHandle LineBuffering
  expectLine ocd.stderrHandle openOcdWaitForHalt

  -- make sure PicoCom is started properly

  projectDir <- findParentContaining "cabal.project"
  let
    hitlDir = projectDir </> "_build" </> "hitl"
    stdoutLog = hitlDir </> "picocom-stdout." <> show targetIndex <> ".log"
    stderrLog = hitlDir </> "picocom-stderr." <> show targetIndex <> ".log"
  putStrLn $ "logging stdout to `" <> stdoutLog <> "`"
  putStrLn $ "logging stderr to `" <> stderrLog <> "`"

  putStrLn "Starting Picocom..."
  (pico, picoClean) <- startPicocomWithLogging deviceInfo.serial stdoutLog stderrLog

  hSetBuffering pico.stdinHandle LineBuffering
  hSetBuffering pico.stdoutHandle LineBuffering

  let
    -- Create function to log the output of the processes
    loggingSequence = do
      threadDelay 1_000_000 -- Wait 1 second for data loggers to catch up
      putStrLn "Picocom stdout"
      picocomOut <- readRemainingChars pico.stdoutHandle

      putStrLn picocomOut

      putStrLn "Picocom StdErr"
      readRemainingChars pico.stderrHandle >>= putStrLn

    tryWithTimeout :: String -> Int -> IO a -> IO a
    tryWithTimeout actionName dur action = do
      result <- timeout dur action
      case result of
        Nothing -> do
          loggingSequence
          error $ "Timeout while performing action: " <> actionName
        Just r -> pure r

  tryWithTimeout "Waiting for \"Terminal ready\"" 10_000_000
    $ waitForLine pico.stdoutHandle "Terminal ready"

  -- program the FPGA
  (gdb, gdbClean) <- startGdb

  hSetBuffering gdb.stdinHandle LineBuffering

  runGdbCommands
    gdb.stdinHandle
    [ "file \"./_build/cargo/firmware-binaries/riscv32imc-unknown-none-elf/debug/hello\""
    , "target extended-remote :" <> show gdbPort
    , "load"
    , gdbEcho "load done"
    ]

  tryWithTimeout "Waiting for \"load dome\"" 120_000_000
    $ waitForLine gdb.stdoutHandle "load done"

  pure $ TestStepSuccess (ocd, pico, gdb, gdbClean >> picoClean >> ocdClean)

driverFunc ::
  VivadoHandle ->
  String ->
  FilePath ->
  [ ( HwTarget
    , DeviceInfo
    , (ProcessStdIoHandles, ProcessStdIoHandles, ProcessStdIoHandles, IO ())
    )
  ] ->
  IO ExitCode
driverFunc v _name ilaPath targets = do
  let
    catchError :: DeviceInfo -> SomeException -> IO ExitCode
    catchError d ex = do
      putStrLn $ "Test Failed on device " <> d.deviceId <> " with: " <> displayException ex
      pure $ ExitFailure 2

  exitCodes <- forM targets $ \(hwT, deviceInfo, (_ocd, pico, gdb, cleanup)) -> flip catch (catchError deviceInfo) $ do
    openHwT v hwT
    execCmd_ v "set_property" ["PROBES.FILE", embrace ilaPath, "[current_hw_device]"]
    refresh_hw_device v []

    execCmd_ v "set_property" ["OUTPUT_VALUE", "1", getProbeTestStartTcl]
    commit_hw_vio v ["[get_hw_vios]"]

    let
      -- Create function to log the output of the processes
      loggingSequence = do
        threadDelay 1_000_000 -- Wait 1 second for data loggers to catch up
        putStrLn "Picocom stdout"
        picocomOut <- readRemainingChars pico.stdoutHandle

        putStrLn picocomOut

        putStrLn "Picocom StdErr"
        readRemainingChars pico.stderrHandle >>= putStrLn

      tryWithTimeout :: String -> Int -> IO a -> IO a
      tryWithTimeout actionName dur action = do
        result <- timeout dur action
        case result of
          Nothing -> do
            loggingSequence
            error $ "Timeout while performing action: " <> actionName
          Just r -> pure r

    -- break test
    do
      putStrLn "Testing whether breakpoints work"

      runGdbCommands
        gdb.stdinHandle
        [ "break hello::test_success"
        , "jump _start"
        , gdbEcho "breakpoint reached"
        ]

      tryWithTimeout "Waiting for \"breakpoint reached\"" 10_000_000
        $ waitForLine gdb.stdoutHandle "breakpoint reached"

      runGdbCommands
        gdb.stdinHandle
        [ "disable 1"
        , gdbEcho "continuing"
        , "continue"
        ]

      tryWithTimeout "Waiting for \"continuing\"" 10_000_000
        $ waitForLine gdb.stdoutHandle "continuing"

    -- This is the last thing that will print when the FPGA has been programmed
    -- and starts entereing UART-echo mode.
    tryWithTimeout "Waiting for \"Going in echo mode!\"" 10_000_000
      $ waitForLine pico.stdoutHandle "Going in echo mode!"

    -- Test UART echo
    hPutStrLn pico.stdinHandle "Hello, UART!"
    tryWithTimeout "Waiting for \"Hello, UART!!\"" 10_000_000
      $ waitForLine pico.stdoutHandle "Hello, UART!"

    execCmd_ v "set_property" ["OUTPUT_VALUE", "0", getProbeTestStartTcl]
    commit_hw_vio v ["[get_hw_vios]"]

    cleanup

    pure $ ExitSuccess

  let exitCode =
        L.foldl
          ( \acc code ->
              case code of
                ExitSuccess -> acc
                _ -> code
          )
          ExitSuccess
          exitCodes

  pure $ exitCode
