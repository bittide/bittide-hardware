-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.Driver.SwCcTopologies where

import Clash.Prelude

import Bittide.ClockControl.Topology (complete)
import Bittide.Hitl (DeviceInfo (deviceId))
import Bittide.Instances.Hitl.Driver.ClockControl.Config (CcConf (CcConf, topology))
import Bittide.Instances.Hitl.Driver.SwitchDemo (
  OcdInitData (ccPort, cleanup),
  dumpCcSamples,
  initGdb,
  initOpenOcd,
  initPicocom,
 )
import Bittide.Instances.Hitl.Utils.Driver (
  assertProbe,
  awaitHandshakes,
  tryWithTimeout,
  tryWithTimeoutFinally,
 )
import Bittide.Instances.Hitl.Utils.Program (ProcessHandles (stdoutHandle), brackets)
import Control.Concurrent.Async (forConcurrently_)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Project.FilePath (findParentContaining)
import Project.Handle (errorToException, waitForLine)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM (VivadoM)

import qualified Bittide.Instances.Hitl.Utils.Gdb as Gdb
import qualified Data.List as L

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

  forM_ targets $ \target ->
    assertProbe "probe_test_start" target

  tryWithTimeout "Wait for handshakes" 30_000_000
    $ awaitHandshakes targets

  let openOcdStarts = liftIO <$> L.zipWith (initOpenOcd hitlDir) targets [0 ..]
  brackets openOcdStarts (liftIO . (.cleanup)) $ \initOcdsData -> do
    let
      ccPorts = (.ccPort) <$> initOcdsData
      ccGdbStarts = liftIO <$> L.zipWith (initGdb hitlDir "clock-control") ccPorts targets

    brackets ccGdbStarts (liftIO . snd) $ \initCCGdbsData -> do
      let
        ccGdbs = fst <$> initCCGdbsData
        ccConf = CcConf{topology = complete 8}
        goDumpCcSamples = dumpCcSamples hitlDir ccConf ccGdbs
        picocomStarts = liftIO <$> L.zipWith (initPicocom hitlDir) targets [0 ..]

      liftIO $ mapM_ ((errorToException =<<) . Gdb.loadBinary) ccGdbs

      brackets picocomStarts (liftIO . snd) $ \(L.map fst -> picocoms) -> do
        liftIO $ mapM_ Gdb.continue ccGdbs
        liftIO
          $ tryWithTimeoutFinally "Waiting for stable links" 60_000_000 goDumpCcSamples
          $ forConcurrently_ picocoms
          $ \pico ->
            waitForLine pico.stdoutHandle "[CC] All links stable"

        pure ExitSuccess
