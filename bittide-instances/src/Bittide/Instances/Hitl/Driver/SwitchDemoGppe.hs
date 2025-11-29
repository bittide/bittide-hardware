-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}

module Bittide.Instances.Hitl.Driver.SwitchDemoGppe where

import Clash.Prelude

import Bittide.ClockControl.Config (defCcConf)
import Bittide.Hitl
import Bittide.Instances.Hitl.Driver.SwitchDemo (
  dumpCcSamples,
  initGdb,
  initPicocom,
 )
import Bittide.Instances.Hitl.Dut.SwitchDemoGppe ()
import Bittide.Instances.Hitl.Setup (FpgaCount)
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

data OcdInitData = OcdInitData
  { muPort :: Int
  -- ^ Management unit GDB port
  , ccPort :: Int
  -- ^ Clock control GDB port
  , gppePort :: Int
  -- ^ GPPE GDB port
  , handles :: ProcessHandles
  -- ^ OpenOCD stdio handles
  , cleanup :: IO ()
  -- ^ Cleanup function
  }

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

  let
    hitlDir = projectDir </> "_build/hitl" </> testName

  forM_ targets (assertProbe "probe_test_start")
  T.tryWithTimeout
    T.PrintActionTime
    "Wait for handshakes successes from all boards"
    30_000_000
    $ awaitHandshakes targets
  let
    -- Expected JTAG IDs for MU, CC and GPPE TAPs in that order
    expectedJtagIds = [0x0514C001, 0x1514C001, 0x2514C001]
    openOcdStarts = liftIO <$> L.zipWith (Ocd.initOpenOcd expectedJtagIds hitlDir) targets [0 ..]
  brackets openOcdStarts (liftIO . (.cleanup)) $ \initOcdsData -> do
    let
      allTapInfos = (.tapInfos) <$> initOcdsData

      muTapInfos, ccTapInfos, gppeTapInfos :: [Ocd.TapInfo]
      (muTapInfos, ccTapInfos, gppeTapInfos)
        | all (== L.length expectedJtagIds) (L.length <$> allTapInfos)
        , [mus, ccs, gppes] <- L.transpose allTapInfos =
            (mus, ccs, gppes)
        | otherwise =
            error
              $ "Unexpected number of OpenOCD taps initialized. Expected: "
              <> show (L.length expectedJtagIds)
              <> ", but got: "
              <> show (L.length <$> allTapInfos)

      muPorts = (.gdbPort) <$> muTapInfos
      ccPorts = (.gdbPort) <$> ccTapInfos
      gppePorts = (.gdbPort) <$> gppeTapInfos

    Gdb.withGdbs (L.length targets) $ \ccGdbs -> do
      liftIO $ zipWithConcurrently3_ (initGdb hitlDir "clock-control") ccGdbs ccPorts targets
      liftIO $ mapConcurrently_ ((errorToException =<<) . Gdb.loadBinary) ccGdbs

      Gdb.withGdbs (L.length targets) $ \muGdbs -> do
        liftIO $ zipWithConcurrently3_ (initGdb hitlDir "switch-demo2-mu") muGdbs muPorts targets
        liftIO $ mapConcurrently_ ((errorToException =<<) . Gdb.loadBinary) muGdbs

        Gdb.withGdbs (L.length targets) $ \gppeGdbs -> do
          liftIO
            $ zipWithConcurrently3_ (initGdb hitlDir "switch-demo2-gppe") gppeGdbs gppePorts targets
          liftIO $ mapConcurrently_ ((errorToException =<<) . Gdb.loadBinary) gppeGdbs

          let picocomStarts = liftIO <$> L.zipWith (initPicocom hitlDir) targets [0 ..]
          brackets picocomStarts (liftIO . snd) $ \(L.map fst -> picocoms) -> do
            let goDumpCcSamples = dumpCcSamples hitlDir (defCcConf (natToNum @FpgaCount)) ccGdbs
            liftIO $ mapConcurrently_ Gdb.continue ccGdbs
            liftIO
              $ T.tryWithTimeoutOn T.PrintActionTime "Waiting for stable links" 60_000_000 goDumpCcSamples
              $ forConcurrently_ picocoms
              $ \pico ->
                waitForLine pico.stdoutHandle "[CC] All links stable"

            liftIO $ mapConcurrently_ Gdb.continue muGdbs
            liftIO
              $ T.tryWithTimeoutOn
                T.PrintActionTime
                "Wait for elastic buffers to be centered"
                60_000_000
                goDumpCcSamples
              $ forConcurrently_ picocoms
              $ \pico ->
                waitForLine pico.stdoutHandle "[MU] All elastic buffers centered"

            liftIO
              $ T.tryWithTimeoutOn
                T.PrintActionTime
                "Waiting for captured UGNs"
                (3 * 60_000_000)
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
                waitForLine pico.stdoutHandle "[PE] Hello!"

            liftIO goDumpCcSamples

            pure ExitSuccess
