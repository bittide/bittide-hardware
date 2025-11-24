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
import Bittide.Instances.Hitl.Driver.SwitchDemo (initGdb, initPicocom)
import Bittide.Instances.Hitl.Utils.Program
import "bittide-extra" Control.Exception.Extra (brackets)

import Control.Concurrent.Async (forConcurrently_, mapConcurrently_)
import Control.Concurrent.Async.Extra (zipWithConcurrently3_)
import Control.Monad (forM_)
import Control.Monad.IO.Class
import System.Exit
import System.FilePath
import System.IO

import qualified Bittide.Instances.Hitl.Utils.OpenOcd as Ocd
import qualified Data.List as L
import qualified Gdb
import qualified System.Timeout.Extra as T

data OcdInitData = OcdInitData
  { gdbPort :: Int
  -- ^ GDB port
  , handles :: ProcessHandles
  -- ^ OpenOCD stdio handles
  , cleanup :: IO ()
  -- ^ Cleanup function
  }

initOpenOcd :: FilePath -> (HwTarget, DeviceInfo) -> Int -> IO OcdInitData
initOpenOcd hitlDir (_, d) targetIndex = do
  putStrLn $ "Starting OpenOCD for target " <> d.deviceId

  let
    gdbPort = 3333 + targetIndex
    ocdStdout = hitlDir </> "openocd-" <> show targetIndex <> "-stdout.log"
    ocdStderr = hitlDir </> "openocd-" <> show targetIndex <> "-stderr.log"
    openocdEnv = [("OPENOCD_STDOUT_LOG", ocdStdout), ("OPENOCD_STDERR_LOG", ocdStderr)]
  putStrLn $ "logging OpenOCD stdout to `" <> ocdStdout <> "`"
  putStrLn $ "logging OpenOCD stderr to `" <> ocdStderr <> "`"

  putStrLn "Starting OpenOCD..."
  (ocd, ocdPh, ocdClean0) <-
    Ocd.startOpenOcdWithEnv openocdEnv d.usbAdapterLocation gdbPort 6666 4444
  hSetBuffering ocd.stderrHandle LineBuffering
  T.tryWithTimeout T.PrintActionTime "Waiting for OpenOCD to start" 15_000_000
    $ expectLine ocd.stderrHandle Ocd.waitForInitComplete

  let
    ocdProcName = "OpenOCD (" <> d.deviceId <> ")"
    ocdClean1 = ocdClean0 >> awaitProcessTermination ocdProcName ocdPh (Just 10_000_000)

  return $ OcdInitData gdbPort ocd ocdClean1

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

    let openOcdStarts = liftIO <$> L.zipWith (initOpenOcd hitlDir) targets [0 ..]
    brackets openOcdStarts (liftIO . (.cleanup)) $ \initOcdsData -> do
      -- make sure OpenOCD is started properly
      let gdbPorts = (.gdbPort) <$> initOcdsData

      Gdb.withGdbs (L.length targets) $ \gdbs -> do
        liftIO $ zipWithConcurrently3_ (initGdb hitlDir "clock-board") gdbs gdbPorts targets
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
