-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Bittide.Instances.Hitl.Driver.FincFdec where

import Clash.Prelude

import Control.Monad (forM_)
import Project.Chan
import Project.FilePath
import Project.Handle (assertEither)

import Vivado.Tcl (HwTarget)
import Vivado.VivadoM

import Bittide.Hitl
import Bittide.Instances.Hitl.Utils.Driver (assertProbe)
import Bittide.Instances.Hitl.Utils.Gdb (initGdb)
import Bittide.Instances.Hitl.Utils.OpenOcd (parseTapInfo)
import Bittide.Instances.Hitl.Utils.Serial (initSerial)
import Bittide.Instances.Hitl.Utils.Usb (resetUsbDeviceByLocation)
import "bittide-extra" Control.Exception.Extra (brackets)

import Control.Concurrent.Async (forConcurrently_, mapConcurrently_)
import Control.Concurrent.Async.Extra (zipWithConcurrently3_)
import Control.Monad.IO.Class
import System.Exit
import System.FilePath

import qualified Bittide.Instances.Hitl.Utils.OpenOcd as Ocd
import qualified Data.List as L
import qualified Gdb
import qualified System.Timeout.Extra as T

driver ::
  String ->
  [(HwTarget, DeviceInfo)] ->
  VivadoM ExitCode
driver _name targets = do
  liftIO
    $ putStrLn
    $ "Running driver function for targets "
    <> show ((\(_, info) -> info.deviceId) <$> targets)

  projectDir <- liftIO $ findParentContaining "cabal.project"
  let hitlDir = projectDir </> "_build" </> "hitl"

  forM_ targets (assertProbe "probe_test_start")

  -- Reset USB adapter, see documentation of "Bittide.Instances.Hitl.Utils.Usb"
  liftIO $ mapM_ (\(_, d) -> resetUsbDeviceByLocation d.usbAdapterLocation) targets

  let
    expectedJtagIds = [0x0514C001]
    toInitArgs (_, deviceInfo) targetIndex =
      Ocd.InitOpenOcdArgs{deviceInfo, expectedJtagIds, hitlDir, targetIndex}
    initArgs = L.zipWith toInitArgs targets [0 ..]
    optionalInitArgs = L.repeat def
    openOcdStarts = liftIO <$> L.zipWith Ocd.initOpenOcd initArgs optionalInitArgs

  brackets openOcdStarts (liftIO . (.cleanup)) $ \initOcdsData -> do
    let
      allTapInfos = parseTapInfo expectedJtagIds <$> initOcdsData

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

    Gdb.withGdbs (L.length targets) $ \gdbs -> do
      liftIO $ zipWithConcurrently3_ (initGdb hitlDir "finc-fdec") gdbs peTapInfos targets
      liftIO $ mapConcurrently_ ((assertEither =<<) . Gdb.loadBinary) gdbs

      let serialStarts = liftIO <$> L.zipWith (initSerial hitlDir) targets [0 ..]
      brackets serialStarts (liftIO . snd) $ \(L.map fst -> serials) -> do
        liftIO $ mapConcurrently_ Gdb.continue gdbs

        liftIO
          $ T.tryWithTimeout T.PrintActionTime "Waiting for test success" 60_000_000
          $ forConcurrently_ serials
          $ \serial ->
            waitForLine serial "All tests passed"

  pure ExitSuccess
