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

import Vivado.Tcl (HwTarget)
import Vivado.VivadoM

import Bittide.Hitl
import Bittide.Instances.Hitl.Setup (demoRigInfo)
import qualified Bittide.Instances.Hitl.Utils.Gdb as Gdb
import Bittide.Instances.Hitl.Utils.Program
import Bittide.Instances.Hitl.Utils.Vivado

import Control.Monad (forM_, zipWithM)
import Control.Monad.IO.Class
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
  String ->
  [(HwTarget, DeviceInfo)] ->
  VivadoM ExitCode
driverFunc testName targets = do
  liftIO
    . putStrLn
    $ "Running Driver function for targets "
    <> show ((\(_, info) -> info.deviceId) <$> targets)

  startTime <- liftIO $ getTime Monotonic

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

    initHwTargets :: VivadoM ()
    initHwTargets = forM_ targets $ \(hwT, d) -> do
      liftIO $ putStrLn $ "Preparing hardware target " <> show d.deviceId

      openHardwareTarget hwT
      updateVio "vioHitlt" [("probe_prog_en", "1")]

    initOpenOcds :: [IO ((Int, ProcessStdIoHandles), IO ())]
    initOpenOcds = flip L.map (L.zip [0 ..] targets) $ \(targetIndex, (_, d)) -> do
      putStrLn $ "Starting OpenOCD for target " <> show d.deviceId

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
        ocdClean2 = ocdClean1 >> awaitProcessTermination ocdProcName ocdPh (Just 15_000_000)

      return ((gdbPort, ocd), ocdClean2)

    initGdbs :: [Int] -> [IO (ProcessStdIoHandles, IO ())]
    initGdbs gdbPorts = L.zipWith go gdbPorts targets
     where
      go :: Int -> (HwTarget, DeviceInfo) -> IO (ProcessStdIoHandles, IO ())
      go gdbPort (hwT, d) = do
        putStrLn $ "Starting GDB for target " <> show d.deviceId

        (gdb, gdbPh, gdbClean1) <- Gdb.startGdbH
        hSetBuffering gdb.stdinHandle LineBuffering
        Gdb.setLogging gdb
          $ "./_build/hitl/"
              <> testName
              <> "/gdb-out-"
              <> show (getTargetIndex hwT)
              <> ".log"
        Gdb.setFile gdb $ firmwareBinariesDir "riscv32imc" Release </> "clock-control"
        Gdb.setTarget gdb gdbPort
        let
          gdbProcName = "GDB (" <> show d.deviceId <> ")"
          gdbClean2 = gdbClean1 >> awaitProcessTermination gdbProcName gdbPh (Just 5_000_000)

        return (gdb, gdbClean2)

    startTest :: (HwTarget, DeviceInfo) -> VivadoM ()
    startTest (hwT, d) = do
      liftIO $ putStrLn $ "Asserting test probe on " <> show d.deviceId

      openHardwareTarget hwT
      updateVio
        "vioHitlt"
        [ ("probe_prog_en", "0")
        , ("probe_test_start", "1")
        ]

    getTestsStatus :: [(HwTarget, DeviceInfo)] -> [TestStatus] -> VivadoM [TestStatus]
    getTestsStatus [] _ = return []
    getTestsStatus _ [] = return []
    getTestsStatus ((hwT, _) : hwtdRest) (status : statusRest) = do
      case status of
        TestRunning -> do
          timeSpent <- liftIO $ calcTimeSpentMs
          rest <- getTestsStatus hwtdRest statusRest
          if timeSpent < testTimeoutMs
            then do
              openHardwareTarget hwT

              vals <- readVio "vioHitlt" ["probe_test_done", "probe_test_success"]
              case vals of
                [("probe_test_done", "1"), ("probe_test_success", success)] ->
                  pure $ TestDone (success == "1") : rest
                _ -> pure $ TestRunning : rest
            else pure $ TestTimeout : rest
        other -> do
          rest <- getTestsStatus hwtdRest statusRest
          pure $ other : rest

    getTestResults :: [(HwTarget, DeviceInfo)] -> [TestStatus] -> VivadoM [ExitCode]
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
          liftIO $ zipWithM go tgts newStatuses
        else getTestResults tgts newStatuses

    foldExitCodes ::
      VivadoM (Int, ExitCode) -> ExitCode -> VivadoM (Int, ExitCode)
    foldExitCodes prev code = do
      (count, acc) <- prev
      return
        $ if code == ExitSuccess
          then (count + 1, acc)
          else (count, code)

    deassertStartTest :: (HwTarget, DeviceInfo) -> VivadoM ()
    deassertStartTest (hwT, d) = do
      openHardwareTarget hwT
      updateVio "vioHitlt" [("probe_test_start", "0")]

      liftIO $ putStrLn $ "Running cleanup for target " <> d.deviceId

  initHwTargets
  brackets (liftIO <$> initOpenOcds) (liftIO . snd) $ \initOcdsData -> do
    let gdbPorts = fmap (fst . fst) initOcdsData
    brackets (liftIO <$> initGdbs gdbPorts) (liftIO . snd) $ \initGdbsData -> do
      let gdbs = fmap fst initGdbsData
      liftIO $ mapM_ ((errorToException =<<) . Gdb.loadBinary) gdbs
      liftIO $ mapM_ ((errorToException =<<) . Gdb.compareSections) gdbs
      liftIO $ mapM_ Gdb.continue gdbs
      mapM_ startTest targets

      testResults <- getTestResults targets (L.replicate (L.length targets) TestRunning)
      (count, exitCode) <-
        L.foldl foldExitCodes (pure (0, ExitSuccess)) testResults
      liftIO
        $ putStrLn [i|Test case #{testName} passed on #{count} of #{L.length targets} targets|]

      forM_ targets deassertStartTest

      return exitCode
 where
  testTimeoutMs = 60_000 :: Integer
