-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

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

import Control.Monad (forM_, zipWithM, zipWithM_)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import System.Clock (Clock (Monotonic), diffTimeSpec, getTime, toNanoSecs)
import System.Exit
import System.FilePath
import System.IO
import System.Timeout (timeout)

import qualified Data.List as L

getProbeProgEnTcl :: String
getProbeProgEnTcl = getTestProbeTcl "*vioHitlt/probe_prog_en"

data TestStatus = TestRunning | TestDone Bool | TestTimeout deriving (Eq)

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

    getTargetIndex :: HwTarget -> Int
    getTargetIndex hwT = fromMaybe 9 $ L.findIndex (\di -> di.deviceId == idFromHwT hwT) demoRigInfo

    tryWithTimeout :: String -> Int -> IO a -> IO a
    tryWithTimeout actionName dur action = do
      result <- timeout dur action
      case result of
        Nothing -> do
          error $ "Timeout while performing action: " <> actionName
        Just r -> pure r

    initHwTargets :: IO ()
    initHwTargets = forM_ targets $ \(hwT, d) -> do
      putStrLn $ "Preparing hardware target " <> show d.deviceId

      openHwTarget v hwT
      execCmd_ v "set_property" ["PROBES.FILE", embrace ilaPath, "[current_hw_device]"]
      refresh_hw_device v []
      execCmd_ v "set_property" ["OUTPUT_VALUE", "1", getProbeProgEnTcl]
      commit_hw_vio v ["[get_hw_vios]"]

    initOpenOcds :: [IO ((Int, ProcessStdIoHandles), IO ())]
    initOpenOcds = flip L.map targets $ \(hwT, d) -> do
      putStrLn $ "Starting OpenOCD for target " <> show d.deviceId

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
      (ocd, ocdPh, ocdClean1) <-
        startOpenOcdWithEnv
          [("OPENOCD_STDOUT_LOG", ocdStdout), ("OPENOCD_STDERR_LOG", ocdStderr)]
          d.usbAdapterLocation
          gdbPort
          tclPort
          telnetPort
      hSetBuffering ocd.stderrHandle LineBuffering
      tryWithTimeout "Waiting for OpenOCD to start" 15_000_000
        $ expectLine ocd.stderrHandle openOcdWaitForHalt

      let
        ocdProcName = "OpenOCD (" <> show d.deviceId <> ")"
        ocdClean2 = ocdClean1 >> awaitProcessTermination ocdProcName ocdPh (Just 5_000_000)

      return ((gdbPort, ocd), ocdClean2)

    initGdbs :: [Int] -> [IO (ProcessStdIoHandles, IO ())]
    initGdbs gdbPorts = L.zipWith go gdbPorts targets
     where
      go :: Int -> (HwTarget, DeviceInfo) -> IO (ProcessStdIoHandles, IO ())
      go gdbPort (hwT, d) = do
        putStrLn $ "Starting GDB for target " <> show d.deviceId

        (gdb, gdbPh, gdbClean1) <- startGdbH
        hSetBuffering gdb.stdinHandle LineBuffering

        runGdbCommands
          gdb.stdinHandle
          [ "set logging file ./_build/hitl/"
              <> testName
              <> "/gdb-out-"
              <> show (getTargetIndex hwT)
              <> ".log"
          , "set logging overwrite on"
          , "set logging enabled on"
          , "file \"./_build/cargo/firmware-binaries/riscv32imc-unknown-none-elf/release/clock-control\""
          , "target extended-remote :" <> show gdbPort
          ]

        let
          gdbProcName = "GDB (" <> show d.deviceId <> ")"
          gdbClean2 = gdbClean1 >> awaitProcessTermination gdbProcName gdbPh (Just 5_000_000)

        return (gdb, gdbClean2)

    loadBinary :: (HwTarget, DeviceInfo) -> ProcessStdIoHandles -> IO ()
    loadBinary (hwT, d) gdb = do
      putStrLn $ "Loading binary onto target " <> show d.deviceId
      runGdbCommands gdb.stdinHandle ["load"]
      tryWithTimeout "Waiting for program load to finish" 120_000_000
        $ expectLine gdb.stdoutHandle gdbWaitForLoad
      openHwTarget v hwT
      execCmd_ v "set_property" ["PROBES.FILE", embrace ilaPath, "[current_hw_device]"]
      refresh_hw_device v []
      execCmd_ v "set_property" ["OUTPUT_VALUE", "0", getProbeProgEnTcl]
      commit_hw_vio v ["[get_hw_vios]"]

    startBinary :: (HwTarget, DeviceInfo) -> ProcessStdIoHandles -> IO ()
    startBinary (hwT, d) gdb = do
      putStrLn $ "Starting binary on target " <> show d.deviceId
      openHwTarget v hwT
      execCmd_ v "set_property" ["PROBES.FILE", embrace ilaPath, "[current_hw_device]"]
      refresh_hw_device v []
      execCmd_ v "set_property" ["OUTPUT_VALUE", "0", getProbeProgEnTcl]
      execCmd_ v "set_property" ["OUTPUT_VALUE", "1", getProbeTestStartTcl]
      commit_hw_vio v ["[get_hw_vios]"]
      runGdbCommands gdb.stdinHandle ["continue"]

    getTestsStatus :: [(HwTarget, DeviceInfo)] -> [TestStatus] -> IO [TestStatus]
    getTestsStatus [] _ = return []
    getTestsStatus _ [] = return []
    getTestsStatus ((hwT, _) : hwtdRest) (status : statusRest) = do
      case status of
        TestRunning -> do
          timeSpent <- calcTimeSpentMs
          rest <- getTestsStatus hwtdRest statusRest
          if timeSpent < testTimeoutMs
            then do
              openHwTarget v hwT
              execCmd_ v "set_property" ["PROBES.FILE", embrace ilaPath, "[current_hw_device]"]
              refresh_hw_device v []
              testDone <- execCmd v "get_property" ["INPUT_VALUE", getProbeTestDoneTcl]
              testSuccess <- execCmd v "get_property" ["INPUT_VALUE", getProbeTestSuccessTcl]
              let newStatus = if testDone == "1" then TestDone (testSuccess == "1") else TestRunning
              pure $ newStatus : rest
            else pure $ TestTimeout : rest
        other -> do
          rest <- getTestsStatus hwtdRest statusRest
          pure $ other : rest

    getTestResults :: [(HwTarget, DeviceInfo)] -> [TestStatus] -> IO [ExitCode]
    getTestResults tgts prevStatuses = do
      newStatuses <- getTestsStatus tgts prevStatuses
      if not (TestRunning `L.elem` newStatuses)
        then do
          let
            go :: (HwTarget, DeviceInfo) -> TestStatus -> IO ExitCode
            go (_, d) status = case status of
              TestDone True -> do
                putStrLn $ "Test passed on target " <> show d.deviceId
                return ExitSuccess
              TestDone False -> do
                putStrLn $ "Test finished unsuccessfully on target " <> show d.deviceId
                return $ ExitFailure 2
              _ -> do
                putStrLn $ "Test timed out on target " <> show d.deviceId
                return $ ExitFailure 2
          zipWithM go tgts newStatuses
        else getTestResults tgts newStatuses

    foldExitCodes ::
      IO (Int, ExitCode) -> ExitCode -> IO (Int, ExitCode)
    foldExitCodes prev code = do
      (count, acc) <- prev
      return
        $ if code == ExitSuccess
          then (count + 1, acc)
          else (count, code)

    deassertStartTest :: (HwTarget, DeviceInfo) -> IO ()
    deassertStartTest (hwT, d) = do
      openHwTarget v hwT
      execCmd_ v "set_property" ["PROBES.FILE", embrace ilaPath, "[current_hw_device]"]
      refresh_hw_device v []
      execCmd_ v "set_property" ["OUTPUT_VALUE", "0", getProbeTestStartTcl]
      commit_hw_vio v ["[get_hw_vios]"]
      putStrLn $ "Running cleanup for target " <> d.deviceId

  initHwTargets
  brackets initOpenOcds snd $ \initOcdsData -> do
    let gdbPorts = fmap (fst . fst) initOcdsData
    brackets (initGdbs gdbPorts) snd $ \initGdbsData -> do
      let gdbs = fmap fst initGdbsData
      zipWithM_ loadBinary targets gdbs
      zipWithM_ startBinary targets gdbs

      testResults <- getTestResults targets (L.replicate (L.length targets) TestRunning)
      (count, exitCode) <-
        L.foldl foldExitCodes (pure (0, ExitSuccess)) testResults
      putStrLn [i|Test case #{testName} passed on #{count} of #{L.length targets} targets|]

      forM_ targets deassertStartTest

      return exitCode
 where
  testTimeoutMs = 60_000 :: Integer
