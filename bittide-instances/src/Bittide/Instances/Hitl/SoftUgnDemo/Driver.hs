-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}

module Bittide.Instances.Hitl.SoftUgnDemo.Driver where

import Clash.Prelude

import Bittide.ClockControl.Config (defCcConf)
import Bittide.Hitl
import Bittide.Instances.Hitl.Setup (FpgaCount)
import Bittide.Instances.Hitl.SwitchDemo.Driver (
  dumpCcSamples,
  initGdb,
  initPicocom,
  parseBootTapInfo,
  parseTapInfo,
 )
import Bittide.Instances.Hitl.Utils.Driver
import Bittide.Instances.Hitl.Utils.Program
import Control.Concurrent.Async (forConcurrently_, mapConcurrently_)
import Control.Concurrent.Async.Extra (zipWithConcurrently3_)
import Control.Monad (forM_)
import Control.Monad.IO.Class
import Data.Vector.Internal.Check (HasCallStack)
import Project.FilePath
import Project.Handle
import System.Exit
import System.FilePath
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM (VivadoM)
import "bittide-extra" Control.Exception.Extra (brackets)

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

  let
    -- BOOT / MU / CC / GPPE IDs
    expectedJtagIds = [0x0514C001, 0x1514C001, 0x2514C001, 0x3514C001]
    toInitArgs (_, deviceInfo) targetIndex =
      Ocd.InitOpenOcdArgs{deviceInfo, expectedJtagIds, hitlDir, targetIndex}
    initArgs = L.zipWith toInitArgs targets [0 ..]
    optionalBootInitArgs = L.repeat def{Ocd.logPrefix = "boot-", Ocd.initTcl = "vexriscv_boot_init.tcl"}
    openOcdBootStarts = liftIO <$> L.zipWith Ocd.initOpenOcd initArgs optionalBootInitArgs

  let picocomStarts = liftIO <$> L.zipWith (initPicocom hitlDir) targets [0 ..]
  brackets picocomStarts (liftIO . snd) $ \(L.map fst -> picocoms) -> do
    -- Start OpenOCD that will program the boot CPU
    brackets openOcdBootStarts (liftIO . (.cleanup)) $ \initOcdsData -> do
      let bootTapInfos = parseBootTapInfo <$> initOcdsData

      Gdb.withGdbs (L.length targets) $ \bootGdbs -> do
        liftIO
          $ zipWithConcurrently3_ (initGdb hitlDir "switch-demo1-boot") bootGdbs bootTapInfos targets
        liftIO $ mapConcurrently_ ((assertEither =<<) . Gdb.loadBinary) bootGdbs
        liftIO $ mapConcurrently_ Gdb.continue bootGdbs
        liftIO
          $ T.tryWithTimeout T.PrintActionTime "Waiting for done" 60_000_000
          $ forConcurrently_ picocoms
          $ \pico ->
            waitForLine pico.stdoutHandle "[BT] Going into infinite loop.."

  let
    optionalInitArgs = L.repeat def
    openOcdStarts = liftIO <$> L.zipWith Ocd.initOpenOcd initArgs optionalInitArgs

  -- Start OpenOCD instances for all CPUs
  brackets openOcdStarts (liftIO . (.cleanup)) $ \initOcdsData -> do
    let
      allTapInfos = parseTapInfo expectedJtagIds <$> initOcdsData

      _bootTapInfos, muTapInfos, ccTapInfos, gppeTapInfos :: [Ocd.TapInfo]
      (_bootTapInfos, muTapInfos, ccTapInfos, gppeTapInfos)
        | all (== L.length expectedJtagIds) (L.length <$> allTapInfos)
        , [boots, mus, ccs, gppes] <- L.transpose allTapInfos =
            (boots, mus, ccs, gppes)
        | otherwise =
            error
              $ "Unexpected number of OpenOCD taps initialized. Expected: "
              <> show (L.length expectedJtagIds)
              <> ", but got: "
              <> show (L.length <$> allTapInfos)

    Gdb.withGdbs (L.length targets) $ \ccGdbs -> do
      liftIO $ zipWithConcurrently3_ (initGdb hitlDir "clock-control") ccGdbs ccTapInfos targets
      liftIO $ mapConcurrently_ ((assertEither =<<) . Gdb.loadBinary) ccGdbs

      Gdb.withGdbs (L.length targets) $ \muGdbs -> do
        liftIO $ zipWithConcurrently3_ (initGdb hitlDir "soft-ugn-mu") muGdbs muTapInfos targets
        liftIO $ mapConcurrently_ ((assertEither =<<) . Gdb.loadBinary) muGdbs

        Gdb.withGdbs (L.length targets) $ \gppeGdbs -> do
          liftIO
            $ zipWithConcurrently3_ (initGdb hitlDir "soft-ugn-gppe") gppeGdbs gppeTapInfos targets
          liftIO $ mapConcurrently_ ((assertEither =<<) . Gdb.loadBinary) gppeGdbs

          brackets picocomStarts (liftIO . snd) $ \(L.map fst -> picocoms) -> do
            let goDumpCcSamples = dumpCcSamples hitlDir (defCcConf (natToNum @FpgaCount)) ccGdbs
            liftIO $ mapConcurrently_ Gdb.continue ccGdbs
            liftIO $ mapConcurrently_ Gdb.continue muGdbs

            liftIO
              $ T.tryWithTimeoutOn
                T.PrintActionTime
                "Waiting for captured UGNs"
                60_000_000
                goDumpCcSamples
              $ forConcurrently_ picocoms
              $ \pico ->
                waitForLine pico.stdoutHandle "[MU] All UGNs captured"

            -- From here the actual test should be done, but for now it's just going to be
            -- waiting for the devices to print out over UART.
            liftIO $ mapConcurrently_ Gdb.continue gppeGdbs
            liftIO
              $ T.tryWithTimeoutOn
                T.PrintActionTime
                "Waiting for GPPE hello"
                (5_000_000)
                goDumpCcSamples
              $ forConcurrently_ picocoms
              $ \pico ->
                waitForLine pico.stdoutHandle "[PE] Hello from C!"

            liftIO goDumpCcSamples

            pure ExitSuccess
