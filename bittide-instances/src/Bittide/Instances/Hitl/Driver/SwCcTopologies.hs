-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Bittide.Instances.Hitl.Driver.SwCcTopologies where

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

import Control.Exception (SomeException, displayException, handle, throw)
import Control.Monad (zipWithM_)
import Data.Foldable (sequenceA_)
import Data.Maybe (fromMaybe)
import System.Clock (Clock (Monotonic), diffTimeSpec, getTime, toNanoSecs)
import System.Exit
import System.FilePath
import System.IO
import System.Timeout (timeout)

import qualified Data.List as L

getProbeProgEnTcl :: String
getProbeProgEnTcl = getTestProbeTcl "*vioHitlt/probe_prog_en"

data TestStatus = TestRunning | TestDone Bool deriving (Eq)

driverFunc ::
  VivadoHandle ->
  String ->
  FilePath ->
  [(HwTarget, DeviceInfo)] ->
  IO ExitCode
driverFunc v testName ilaPath targets = do
  putStrLn
    $ "Running Driver function for targets "
    <> show ((\(_, info) -> info.deviceId) <$> targets)

  startTime <- getTime Monotonic

  let
    calcTimeSpentMs = (`div` 1000000) . toNanoSecs . diffTimeSpec startTime <$> getTime Monotonic

    catchError :: DeviceInfo -> SomeException -> IO ExitCode
    catchError d ex = do
      putStrLn $ "Test failed on device " <> d.deviceId <> " with: " <> displayException ex
      pure $ ExitFailure 2

    tryWithTimeout :: String -> Int -> IO a -> IO a
    tryWithTimeout actionName dur action = do
      result <- timeout dur action
      case result of
        Nothing -> do
          error $ "Timeout while performing action: " <> actionName
        Just r -> pure r

    startPrograms ::
      (HwTarget, DeviceInfo) ->
      IO (ProcessStdIoHandles, ProcessStdIoHandles, IO ())
    startPrograms (hwT, d) = do
      putStrLn $ "Starting OpenOCD and GDB for target " <> show d.deviceId

      openHwTarget v hwT
      execCmd_ v "set_property" ["PROBES.FILE", embrace ilaPath, "[current_hw_device]"]
      refresh_hw_device v []
      execCmd_ v "set_property" ["OUTPUT_VALUE", "1", getProbeProgEnTcl]
      commit_hw_vio v ["[get_hw_vios]"]

      let targetId = idFromHwT hwT
      let targetIndex = fromMaybe 9 $ L.findIndex (\di -> di.deviceId == targetId) demoRigInfo
      let gdbPort = 3333 + targetIndex
      let tclPort = 6666 + targetIndex
      let telnetPort = 4444 + targetIndex

      projectDir <- findParentContaining "cabal.project"
      let
        ocdStdout =
          projectDir
            </> "_build"
            </> "hitl"
            </> testName
            </> "openocd-" <> show targetIndex <> "-stdout.log"
        ocdStderr =
          projectDir
            </> "_build"
            </> "hitl"
            </> testName
            </> "openocd-" <> show targetIndex <> "-stderr.log"
      putStrLn $ "logging OpenOCD stdout to `" <> ocdStdout <> "`"
      putStrLn $ "logging OpenOCD stderr to `" <> ocdStderr <> "`"

      putStrLn "Starting OpenOCD..."
      (ocd, ocdPh, ocdClean) <-
        startOpenOcdWithEnv
          [("OPENOCD_STDOUT_LOG", ocdStdout), ("OPENOCD_STDERR_LOG", ocdStderr)]
          d.usbAdapterLocation
          gdbPort
          tclPort
          telnetPort
      hSetBuffering ocd.stderrHandle LineBuffering
      tryWithTimeout "Waiting for OpenOCD to start" 15_000_000
        $ expectLine ocd.stderrHandle openOcdWaitForHalt
      let ocdProcName = "OpenOCD (" <> show d.deviceId <> ")"

      putStrLn "Starting GDB"
      (gdb, gdbPh, gdbClean) <- startGdbH
      hSetBuffering gdb.stdinHandle LineBuffering
      let gdbProcName = "GDB (" <> show d.deviceId <> ")"

      runGdbCommands
        gdb.stdinHandle
        [ "set logging file ./_build/hitl/" <> testName <> "/gdb-out-" <> show targetIndex <> ".log"
        , "set logging overwrite on"
        , "set logging enabled on"
        , "file \"./_build/cargo/firmware-binaries/riscv32imc-unknown-none-elf/release/clock-control\""
        , "target extended-remote :" <> show gdbPort
        ]

      return
        ( ocd
        , gdb
        , do
            gdbClean
            ocdClean
            awaitProcessTermination gdbProcName gdbPh (Just 5_000_000)
            awaitProcessTermination ocdProcName ocdPh (Just 5_000_000)
        )

    loadBinary ::
      (HwTarget, DeviceInfo) ->
      (ProcessStdIoHandles, ProcessStdIoHandles, IO ()) ->
      IO ()
    loadBinary (hwT, d) (_, gdb, cleanup) =
      handle (\e -> catchError d e >> cleanup >> throw e) $ do
        putStrLn $ "Loading binary onto target " <> show d.deviceId
        runGdbCommands gdb.stdinHandle ["load"]
        tryWithTimeout "Waiting for program load to finish" 120_000_000
          $ expectLine gdb.stdoutHandle gdbWaitForLoad
        openHwTarget v hwT
        execCmd_ v "set_property" ["PROBES.FILE", embrace ilaPath, "[current_hw_device]"]
        refresh_hw_device v []
        execCmd_ v "set_property" ["OUTPUT_VALUE", "0", getProbeProgEnTcl]
        commit_hw_vio v ["[get_hw_vios]"]

    startBinary ::
      (HwTarget, DeviceInfo) ->
      (ProcessStdIoHandles, ProcessStdIoHandles, IO ()) ->
      IO ()
    startBinary (hwT, d) (_, gdb, cleanup) =
      handle (\e -> catchError d e >> cleanup >> throw e) $ do
        putStrLn $ "Starting binary on target " <> show d.deviceId
        openHwTarget v hwT
        execCmd_ v "set_property" ["PROBES.FILE", embrace ilaPath, "[current_hw_device]"]
        refresh_hw_device v []
        execCmd_ v "set_property" ["OUTPUT_VALUE", "0", getProbeProgEnTcl]
        execCmd_ v "set_property" ["OUTPUT_VALUE", "1", getProbeTestStartTcl]
        commit_hw_vio v ["[get_hw_vios]"]
        runGdbCommands gdb.stdinHandle ["continue"]

    getTestsStatus ::
      [(HwTarget, DeviceInfo)] ->
      [(ProcessStdIoHandles, ProcessStdIoHandles, IO ())] ->
      [TestStatus] ->
      IO [Maybe TestStatus]
    getTestsStatus [] _ _ = return []
    getTestsStatus _ [] _ = return []
    getTestsStatus _ _ [] = return []
    getTestsStatus ((hwT, _) : hwtdRest) (_ : hdlsRest) (status : statusRest) = do
      case status of
        TestRunning -> do
          timeSpent <- calcTimeSpentMs
          rest <- getTestsStatus hwtdRest hdlsRest statusRest
          if timeSpent < testTimeoutMs
            then do
              openHwTarget v hwT
              execCmd_ v "set_property" ["PROBES.FILE", embrace ilaPath, "[current_hw_device]"]
              refresh_hw_device v []
              testDone <- execCmd v "get_property" ["INPUT_VALUE", getProbeTestDoneTcl]
              testSuccess <- execCmd v "get_property" ["INPUT_VALUE", getProbeTestSuccessTcl]
              let newStatus = if testDone == "1" then TestDone (testSuccess == "1") else TestRunning
              pure $ (Just newStatus) : rest
            else pure $ Nothing : rest
        other -> do
          rest <- getTestsStatus hwtdRest hdlsRest statusRest
          pure $ (Just other) : rest

    getTestResults ::
      [(HwTarget, DeviceInfo)] ->
      [(ProcessStdIoHandles, ProcessStdIoHandles, IO ())] ->
      Maybe [TestStatus] ->
      IO [ExitCode]
    getTestResults tgts hdls prevStatus0 = do
      let prevStatus1 = fromMaybe (L.replicate (L.length tgts) TestRunning) prevStatus0
      testsStatus <- getTestsStatus tgts hdls prevStatus1
      case sequence testsStatus of
        Just statuses ->
          if not (TestRunning `L.elem` statuses)
            then do
              let
                go (TestDone True) = ExitSuccess
                go _ = ExitFailure 2
                exitCodes = go <$> statuses
              return exitCodes
            else getTestResults tgts hdls (Just statuses)
        Nothing -> do
          let
            go ::
              (HwTarget, DeviceInfo) ->
              (ProcessStdIoHandles, ProcessStdIoHandles, IO ()) ->
              Maybe TestStatus ->
              IO ()
            go _ (_, _, cleanup) (Just _) = cleanup
            go (_, d) (_, _, cleanup) Nothing = do
              cleanup
              putStrLn $ "Test timed out on target " <> show d.deviceId
          sequenceA_ (L.zipWith3 go tgts hdls testsStatus)
          error "Test failed on previously listed timeouts."

    foldExitCodes ::
      IO (Int, ExitCode) ->
      ((HwTarget, DeviceInfo), ExitCode) ->
      IO (Int, ExitCode)
    foldExitCodes prev ((_, d), code) = do
      (count, acc) <- prev
      if code == ExitSuccess
        then do
          putStrLn $ "Test succeeded on target " <> show d.deviceId
          return (count + 1, acc)
        else do
          putStrLn $ "Test failed on target " <> show d.deviceId
          return (count, code)

    deassertStartTest ::
      (HwTarget, DeviceInfo) ->
      (ProcessStdIoHandles, ProcessStdIoHandles, IO ()) ->
      IO ()
    deassertStartTest (hwT, d) (_, _, cleanup) =
      handle (\e -> catchError d e >> cleanup >> throw e) $ do
        openHwTarget v hwT
        execCmd_ v "set_property" ["PROBES.FILE", embrace ilaPath, "[current_hw_device]"]
        refresh_hw_device v []
        execCmd_ v "set_property" ["OUTPUT_VALUE", "1", getProbeTestStartTcl]
        commit_hw_vio v ["[get_hw_vios]"]
        putStrLn $ "Running cleanup for target " <> d.deviceId
        cleanup

  targetProgs <- mapM startPrograms targets

  zipWithM_ loadBinary targets targetProgs

  zipWithM_ startBinary targets targetProgs

  testResults <- getTestResults targets targetProgs Nothing

  (count, exitCode) <-
    L.foldl foldExitCodes (pure (0, ExitSuccess)) $ L.zip targets testResults
  putStrLn
    $ "Test case '"
    <> testName
    <> "' passed on "
    <> show count
    <> " of "
    <> show (L.length targets)
    <> " targets."

  zipWithM_ deassertStartTest targets targetProgs

  return exitCode
 where
  testTimeoutMs = 60_000 :: Integer
