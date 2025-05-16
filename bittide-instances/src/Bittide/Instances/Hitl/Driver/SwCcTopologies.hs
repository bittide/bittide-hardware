-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DuplicateRecordFields #-}
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
import Bittide.Instances.Hitl.Utils.Driver
import Bittide.Instances.Hitl.Utils.Program
import Bittide.Instances.Hitl.Utils.Vivado

import Clash.Sized.Extra (extendLsb0s)
import Control.Monad (forM_, zipWithM, (<=<))
import Control.Monad.IO.Class
import Data.String.Interpolate (i)
import System.Clock (Clock (Monotonic), diffTimeSpec, getTime, toNanoSecs)
import System.Exit
import System.FilePath
import System.IO

import qualified Bittide.Instances.Hitl.Utils.Gdb as Gdb
import qualified Data.List as L

whoAmID :: BitVector 32
whoAmID = 0x3075_7063

whoAmIPfx :: Unsigned 3
whoAmIPfx = 0b111

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

  projectDir <- liftIO $ findParentContaining "cabal.project"
  let hitlDir = projectDir </> "_build/hitl/" <> testName
  startTime <- liftIO $ getTime Monotonic
  let
    calcTimeSpentMs = (`div` 1_000_000) . toNanoSecs . diffTimeSpec startTime <$> getTime Monotonic

    initHwTargets :: VivadoM ()
    initHwTargets = forM_ targets (assertProbe "probe_prog_en")

    initGdbAdapters :: (a, DeviceInfo) -> Int -> IO ((Int, ProcessStdIoHandles), IO ())
    initGdbAdapters (_, d) targetIndex = do
      putStrLn $ "Starting gdb-adapters for target " <> show d.deviceId
      putStrLn $ "Logs will be saved in the hitl directory: " <> hitlDir
      let
        gdbPort = 3333 + targetIndex
        adaptersStdout = hitlDir </> "gdb-adapters-" <> show targetIndex <> "-stdout.log"
        adaptersStderr = hitlDir </> "gdb-adapters-" <> show targetIndex <> "-stderr.log"
        adaptersConfig =
          GdbAdaptersConfig
            { usbDev = d.usbAdapterLocation
            , memMapAddress = extendLsb0s whoAmIPfx
            , cpuMap = Build [(whoAmID, gdbPort)]
            , stdoutPath = Just adaptersStdout
            , stderrPath = Just adaptersStderr
            }

      putStrLn "Starting gdb-adapters..."
      (adapters, adaptersPh, adaptersClean1) <- startGdbAdapters adaptersConfig
      hSetBuffering adapters.stderrHandle LineBuffering
      tryWithTimeout "Waiting for gdb-adapters to start" 15_000_000
        $ expectLine adapters.stderrHandle adaptersWaitForHalt

      let
        adaptersProcName = "gdb-adapters (" <> show d.deviceId <> ")"
        adaptersClean2 = adaptersClean1 >> awaitProcessTermination adaptersProcName adaptersPh (Just 5_000_000)

      return ((gdbPort, adapters), adaptersClean2)

    initGdb :: Int -> (HwTarget, DeviceInfo) -> IO (ProcessStdIoHandles, IO ())
    initGdb gdbPort (hwT, d) = do
      putStrLn $ "Starting GDB for target " <> show d.deviceId

      (gdb, gdbPh, gdbClean1) <- Gdb.startGdbH
      hSetBuffering gdb.stdinHandle LineBuffering
      Gdb.setLogging gdb $ hitlDir </> "gdb-" <> show (getTargetIndex hwT) <> "-stdout.log"
      Gdb.setFile gdb $ firmwareBinariesDir RiscV Release </> "clock-control"
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
          timeSpent <- liftIO calcTimeSpentMs
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
      if TestRunning `notElem` newStatuses
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

  initHwTargets
  let gdbPorts = L.take (L.length targets) [3333 ..]
  brackets (liftIO <$> L.zipWith initGdbAdapters targets [0 ..]) (liftIO . snd) $ \_initAdaptersData -> do
    brackets (liftIO <$> L.zipWith initGdb gdbPorts targets) (liftIO . snd) $ \initGdbsData -> do
      let gdbs = fmap fst initGdbsData
      liftIO $ mapM_ (errorToException <=< Gdb.loadBinary) gdbs

      -- TODO: Replace `prog_en` vio with `enable_sync_gen` vio
      forM_ targets (deassertProbe "probe_prog_en")
      -- liftIO $ mapM_ ((errorToException =<<) . Gdb.compareSections) gdbs
      liftIO $ mapM_ Gdb.continue gdbs
      forM_ targets startTest

      testResults <- getTestResults targets (L.replicate (L.length targets) TestRunning)
      (count, exitCode) <-
        L.foldl foldExitCodes (pure (0, ExitSuccess)) testResults
      liftIO
        $ putStrLn [i|Test case #{testName} passed on #{count} of #{L.length targets} targets|]

      return exitCode
 where
  testTimeoutMs = 60_000 :: Integer
