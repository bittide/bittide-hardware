-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}

module Bittide.Instances.Hitl.SoftUgnDemo.Driver where

import Clash.Prelude

import Bittide.ClockControl.Config (defCcConf)
import Bittide.Hitl
import Bittide.Instances.Hitl.Setup (FpgaCount)
import Bittide.Instances.Hitl.Utils.Driver
import Bittide.Instances.Hitl.Utils.Gdb (initGdb)
import Bittide.Instances.Hitl.Utils.OpenOcd (parseBootTapInfo, parseTapInfo)
import Bittide.Instances.Hitl.Utils.Serial (initSerial)
import Bittide.Instances.Hitl.Utils.Ugn
import Bittide.Instances.Hitl.Utils.Usb (resetUsbDeviceByLocation)
import Bittide.Instances.Hitl.Utils.Utils (dumpCcSamples)
import Control.Concurrent.Async (forConcurrently_, mapConcurrently, mapConcurrently_)
import Control.Concurrent.Async.Extra (zipWithConcurrently3_)
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class
import Data.Vector.Internal.Check (HasCallStack)
import Project.Chan
import Project.FilePath
import Project.Handle (assertEither)
import System.Exit
import System.FilePath
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM (VivadoM)
import "bittide-extra" Control.Exception.Extra (brackets)

import qualified Bittide.Instances.Hitl.SoftUgnDemo.MemoryMaps as MemoryMaps
import qualified Bittide.Instances.Hitl.Utils.OpenOcd as Ocd
import qualified Data.List as L
import qualified Gdb
import qualified System.Timeout.Extra as T

driver ::
  (HasCallStack) =>
  String ->
  [(HwTarget, DeviceInfo)] ->
  VivadoM ExitCode
driver testName targets = do
  liftIO
    . putStrLn
    $ "Running driver function for targets "
    <> show ((\(_, info) -> info.deviceId) <$> targets)

  projectDir <- liftIO $ findParentContaining "cabal.project"
  let hitlDir = projectDir </> "_build/hitl" </> testName

  forM_ targets (assertProbe "probe_test_start")

  -- Reset USB adapter, see documentation of "Bittide.Instances.Hitl.Utils.Usb"
  liftIO $ forM_ targets $ \(_, d) -> resetUsbDeviceByLocation d.usbAdapterLocation

  let
    -- BOOT / MU / CC IDs
    expectedJtagIds = [0x0514C001, 0x1514C001, 0x2514C001]
    toInitArgs (_, deviceInfo) targetIndex =
      Ocd.InitOpenOcdArgs{deviceInfo, expectedJtagIds, hitlDir, targetIndex}
    initArgs = L.zipWith toInitArgs targets [0 ..]
    optionalBootInitArgs = L.repeat def{Ocd.logPrefix = "boot-", Ocd.initTcl = "vexriscv_boot_init.tcl"}
    openOcdBootStarts = liftIO <$> L.zipWith Ocd.initOpenOcd initArgs optionalBootInitArgs

  let serialStarts = liftIO <$> L.zipWith (initSerial hitlDir) targets [0 ..]
  brackets serialStarts (liftIO . snd) $ \(L.map fst -> serials) -> do
    -- Start OpenOCD that will program the boot CPU
    brackets openOcdBootStarts (liftIO . (.cleanup)) $ \initOcdsData -> do
      let bootTapInfos = parseBootTapInfo <$> initOcdsData

      Gdb.withGdbs (L.length targets) $ \bootGdbs -> do
        liftIO
          $ zipWithConcurrently3_ (initGdb hitlDir "soft-ugn-demo-boot") bootGdbs bootTapInfos targets
        liftIO $ mapConcurrently_ ((assertEither =<<) . Gdb.loadBinary) bootGdbs
        liftIO $ mapConcurrently_ Gdb.continue bootGdbs
        liftIO
          $ T.tryWithTimeout T.PrintActionTime "Waiting for done" 60_000_000
          $ forConcurrently_ serials
          $ \serial ->
            waitForLine serial "[BT] Going into infinite loop.."

  let
    optionalInitArgs = L.repeat def
    openOcdStarts = liftIO <$> L.zipWith Ocd.initOpenOcd initArgs optionalInitArgs

  -- Start OpenOCD instances for all CPUs
  brackets openOcdStarts (liftIO . (.cleanup)) $ \initOcdsData -> do
    let
      allTapInfos = parseTapInfo expectedJtagIds <$> initOcdsData

      _bootTapInfos, managementUnitTapInfos, clockControlTapInfos :: [Ocd.TapInfo]
      (_bootTapInfos, managementUnitTapInfos, clockControlTapInfos)
        | all (== L.length expectedJtagIds) (L.length <$> allTapInfos)
        , [boots, mus, ccs] <- L.transpose allTapInfos =
            (boots, mus, ccs)
        | otherwise =
            error
              $ "Unexpected number of OpenOCD taps initialized. Expected: "
              <> show (L.length expectedJtagIds)
              <> ", but got: "
              <> show (L.length <$> allTapInfos)

    Gdb.withGdbs (L.length targets) $ \clockControlGdbs -> do
      liftIO
        $ zipWithConcurrently3_
          (initGdb hitlDir "soft-ugn-demo-clock-control")
          clockControlGdbs
          clockControlTapInfos
          targets
      liftIO $ mapConcurrently_ ((assertEither =<<) . Gdb.loadBinary) clockControlGdbs

      Gdb.withGdbs (L.length targets) $ \managementUnitGdbs -> do
        liftIO
          $ zipWithConcurrently3_
            (initGdb hitlDir "soft-ugn-demo-management-unit")
            managementUnitGdbs
            managementUnitTapInfos
            targets
        liftIO $ mapConcurrently_ ((assertEither =<<) . Gdb.loadBinary) managementUnitGdbs

        brackets serialStarts (liftIO . snd) $ \(L.map fst -> serials) -> do
          let goDumpCcSamples = dumpCcSamples MemoryMaps.clockControl hitlDir (defCcConf (natToNum @FpgaCount)) clockControlGdbs
          liftIO $ mapConcurrently_ Gdb.continue clockControlGdbs
          liftIO $ mapConcurrently_ Gdb.continue managementUnitGdbs

          hardwareCaptureCounters <-
            liftIO
              $ T.tryWithTimeoutOn
                T.PrintActionTime
                "Waiting for hardware UGNs"
                (3 * 60_000_000)
                goDumpCcSamples
              $ mapConcurrently parseCaptureCounters serials
          let
            hardwareUgns =
              L.zipWith
                (\i ccs -> (timingOracleToUgnEdge . counterCaptureToTimingOracle i) <$> ccs)
                [0 ..]
                hardwareCaptureCounters
            hardwareRoundtrips = calculateRoundtripLatencies $ L.concat hardwareUgns
          _ <- liftIO $ do
            putStrLn "\n=== Hardware UGN Roundtrip Latencies ==="
            mapM print hardwareRoundtrips

          softwareUgnsPerNode <-
            liftIO
              $ T.tryWithTimeoutOn
                T.PrintActionTime
                "Waiting for software UGNs"
                (60_000_000)
                goDumpCcSamples
              $ mapConcurrently parseSoftwareUgns serials

          liftIO $ do
            putStrLn "\n=== Hardware UGNs ==="
            forM_ (L.zip hardwareUgns [0 :: Int ..]) $ \(hw, idx) ->
              putStrLn $ "Node " <> show idx <> ": " <> show (L.length hw) <> " edges"

            putStrLn "\n=== Software UGNs ==="
            forM_ (L.zip softwareUgnsPerNode [0 :: Int ..]) $ \((swIn, swOut), idx) ->
              putStrLn
                $ "Node "
                <> show idx
                <> ": "
                <> show (L.length swIn + L.length swOut)
                <> " edges"

            -- Process UGN edges and calculate roundtrip latencies
            let hardwareUgnsFlat = postProcessHardwareUgns hardwareUgns
            softwareUgnsFlat <- postProcessSoftwareUgns softwareUgnsPerNode

            let
              swExtraLatency = 1 -- ring buffer read latency
              mismatchedUgns =
                findMismatchedUgnEdges
                  (fmap (addLatencyEdge swExtraLatency) hardwareUgnsFlat)
                  softwareUgnsFlat
              softwareRoundtrips = calculateRoundtripLatencies softwareUgnsFlat
            unless (L.null mismatchedUgns) $ do
              putStrLn "\n=== Mismatched UGN Edges ==="
              forM_ mismatchedUgns $ \(hw, sw) -> do
                putStrLn $ "Hardware: " <> show hw
                putStrLn $ "Software: " <> show sw

            putStrLn "\n=== Software Roundtrip Latencies ==="
            mapM_ print softwareRoundtrips

            -- Compare roundtrip latencies
            matched <-
              compareRoundtripLatencies
                (fmap (adjustLatencyRoundTrip (2 * swExtraLatency)) hardwareRoundtrips)
                softwareRoundtrips

            unless matched $ do
              putStrLn "\n=== Per-Node Details ==="
              forM_ (L.zip3 hardwareUgns softwareUgnsPerNode [0 :: Int ..]) $ \(hw, (swIn, swOut), idx) -> do
                let sw = swIn L.++ swOut
                when (L.length hw /= L.length sw) $ do
                  putStrLn $ "\nNode " <> show idx <> " edge count differs:"
                  putStrLn $ "  Hardware: " <> show (L.length hw) <> " edges"
                  putStrLn
                    $ "  Software: "
                    <> show (L.length sw)
                    <> " edges ("
                    <> show (L.length swIn)
                    <> " in + "
                    <> show (L.length swOut)
                    <> " out)"
              error "Roundtrip latencies did not match between hardware and software"
            when (not $ L.null mismatchedUgns) $ error "Some UGN edges did not match!"

            liftIO
              $ T.tryWithTimeoutOn
                T.PrintActionTime
                "Waiting for CPU test status"
                (1_000_000)
                goDumpCcSamples
              $ forConcurrently_ serials
              $ \serial ->
                waitForLine serial "[MU] Test status: Success"

          liftIO goDumpCcSamples

          pure ExitSuccess
