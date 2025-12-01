-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Bittide.Instances.Hitl.Driver.Si539xConfiguration where

import Clash.Prelude

import Project.FilePath
import Project.Handle

import Vivado.Tcl (HwTarget)
import Vivado.VivadoM

import Bittide.Hitl
import Bittide.Instances.Hitl.SwitchDemo.Driver (initGdb, initPicocom)
import Bittide.Instances.Hitl.Utils.Program
import "bittide-extra" Control.Exception.Extra (brackets)

import Control.Concurrent.Async (forConcurrently_, mapConcurrently_)
import Control.Concurrent.Async.Extra (zipWithConcurrently3_)
import Control.Monad (forM_)
import Control.Monad.IO.Class
import System.Exit
import System.FilePath

import qualified Bittide.Instances.Hitl.Utils.OpenOcd as Ocd
import qualified Data.List as L
import qualified Gdb
import qualified System.Timeout.Extra as T

driverFunc ::
  String ->
  [(HwTarget, DeviceInfo)] ->
  VivadoM ExitCode
driverFunc _name targets = do
  liftIO
    $ putStrLn
    $ "Running driver function for targets "
    <> show ((\(_, info) -> info.deviceId) <$> targets)

  projectDir <- liftIO $ findParentContaining "cabal.project"

  forM_ targets $ \(hwT, deviceInfo) -> do
    liftIO $ putStrLn $ "Running driver for " <> deviceInfo.deviceId

    let hitlDir = projectDir </> "_build" </> "hitl"

    openHardwareTarget hwT

    let
      expectedJtagIds = [0x0514C001]
      openOcdStarts = liftIO <$> L.zipWith (Ocd.initOpenOcd expectedJtagIds hitlDir) targets [0 ..]
    brackets openOcdStarts (liftIO . (.cleanup)) $ \initOcdsData -> do
      let
        allTapInfos = (.tapInfos) <$> initOcdsData

        peTapInfos :: [Ocd.TapInfo]
        peTapInfos
          | all (== L.length expectedJtagIds) (L.length <$> allTapInfos)
          , [pes] <- L.transpose allTapInfos =
              pes
          | otherwise =
              error
                $ "Unexpected number of OpenOCD taps initialized. Expected: "
                <> show (L.length expectedJtagIds)
                <> ", but got: "
                <> show (L.length <$> allTapInfos)

        gdbPorts = (.gdbPort) <$> peTapInfos

      Gdb.withGdbs (L.length targets) $ \gdbs -> do
        liftIO $ zipWithConcurrently3_ (initGdb hitlDir "clock-board") gdbs peTapInfos targets
        liftIO $ mapConcurrently_ ((errorToException =<<) . Gdb.loadBinary) gdbs

        let picocomStarts = liftIO <$> L.zipWith (initPicocom hitlDir) targets [0 ..]
        brackets picocomStarts (liftIO . snd) $ \(L.map fst -> picocoms) -> do
          liftIO $ mapConcurrently_ Gdb.continue gdbs

          liftIO
            $ T.tryWithTimeout T.PrintActionTime "Waiting for test success" 15_000_000
            $ forConcurrently_ picocoms
            $ \pico ->
              waitForLine pico.stdoutHandle "All tests passed"

  pure ExitSuccess
