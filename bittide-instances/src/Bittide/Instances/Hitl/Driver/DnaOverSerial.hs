-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Bittide.Instances.Hitl.Driver.DnaOverSerial where

import Clash.Prelude
import Prelude ()

import Bittide.Hitl
import Bittide.Instances.Hitl.Setup
import Bittide.Instances.Hitl.Utils.Program
import Bittide.Instances.Hitl.Utils.Vivado
import Control.Exception
import Control.Monad.Extra
import qualified Data.List as L
import Data.Maybe
import Numeric
import Project.FilePath
import Project.Handle
import System.Exit
import System.FilePath
import System.IO
import System.Timeout
import Test.Tasty.HUnit
import Vivado
import Vivado.Tcl

dnaOverSerialPreProcess ::
  VivadoHandle ->
  String ->
  FilePath ->
  HwTarget ->
  DeviceInfo ->
  IO (TestStepResult (ProcessStdIoHandles, IO ()))
dnaOverSerialPreProcess _v _name _ilaPath hwT deviceInfo = do
  let targetId = idFromHwT hwT
  let targetIndex = fromMaybe 9 $ L.findIndex (\d -> d.deviceId == targetId) demoRigInfo

  -- make sure PicoCom is started properly

  projectDir <- findParentContaining "cabal.project"
  let
    hitlDir = projectDir </> "_build" </> "hitl"
    stdoutLog = hitlDir </> "picocom-stdout." <> show targetIndex <> ".log"
    stderrLog = hitlDir </> "picocom-stderr." <> show targetIndex <> ".log"
  putStrLn $ "logging stdout to `" <> stdoutLog <> "`"
  putStrLn $ "logging stderr to `" <> stderrLog <> "`"
  (pico, picoClean) <- startPicocomWithLogging deviceInfo.serial stdoutLog stderrLog

  hSetBuffering pico.stdinHandle LineBuffering
  hSetBuffering pico.stdoutHandle LineBuffering

  pure $ TestStepSuccess (pico, picoClean)

{- | Test that all FPGAs that are programmed with `dnaOverSerial` transmit the
DNA that we expect based on the DeviceInfo.
-}
dnaOverSerialDriver ::
  VivadoHandle ->
  String ->
  FilePath ->
  [(HwTarget, DeviceInfo, (ProcessStdIoHandles, IO ()))] ->
  IO ExitCode
dnaOverSerialDriver v _name ilaPath targets = do
  flip finally (forM targets $ \(_, _, (_, cleanup)) -> cleanup) $ do
    putStrLn "Starting all targets to read DNA values"

    -- start all targets
    forM_ targets $ \(hwT, _, _) -> do
      openHwT v hwT
      execCmd_ v "set_property" ["PROBES.FILE", embrace ilaPath, "[current_hw_device]"]
      refresh_hw_device v []

      execCmd_ v "set_property" ["OUTPUT_VALUE", "1", getProbeTestStartTcl]
      commit_hw_vio v ["[get_hw_vios]"]

    putStrLn "Expecting specific DNAs for all serial ports"
    putStrLn "Serial ports:"
    mapM_ putStrLn [d.serial | (_, d, _) <- targets]

    results <- forM targets $ \(_, d, (picoCom, picoClean)) -> do
      res <- checkDna d picoCom
      picoClean
      pure $ res

    print results
    if (and results)
      then pure $ ExitSuccess
      else do
        assertFailure "Not all FPGAs transmitted the expected DNA"
        pure $ ExitFailure 2
 where
  checkDna :: DeviceInfo -> ProcessStdIoHandles -> IO Bool
  checkDna d pico = do
    terminalReadyResult <-
      timeout 10_000_000 $ waitForLine pico.stdoutHandle "Terminal ready"
    when (isNothing terminalReadyResult) $ do
      assertFailure "Timeout waiting for \"Terminal ready\""

    _ <- hGetLine pico.stdoutHandle -- Discard a potentially incomplete line
    receivedDna <- hGetLine pico.stdoutHandle
    let
      expected = showHex d.dna ""
      differences = L.zipWith (\e a -> if e == a then ' ' else '^') expected receivedDna
      match = read ("0x" <> receivedDna) == (read ("0x" <> expected) :: Int)
    putStrLn $ "Serial path: " <> d.serial
    putStrLn $ "Expected DNA: " <> expected
    putStrLn $ "Received DNA: " <> receivedDna
    putStrLn $ "Differences:  " <> differences
    pure match
