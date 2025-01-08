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
import qualified Data.ByteString as BS
import Data.ByteString.Internal (w2c)
import Data.Either (partitionEithers)
import qualified Data.List as L
import Data.Maybe
import Data.Word8 (isHexDigit)
import Numeric
import Project.FilePath
import Project.Handle
import System.Exit
import System.FilePath
import System.IO (BufferMode (..), hSetBuffering)
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
  (pico, picoClean) <-
    startPicocomWithLoggingAndEnv
      deviceInfo.serial
      stdoutLog
      stderrLog
      [("PICOCOM_BAUD", "9600")]

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
  [(HwTarget, DeviceInfo)] ->
  IO ExitCode
dnaOverSerialDriver v _name ilaPath targets = do
  preProcessResults <- forM targets $ \(hwT, dI) -> do
    dnaOverSerialPreProcess v _name ilaPath hwT dI >>= \case
      TestStepSuccess out -> pure $ Right out
      TestStepFailure reason -> pure $ Left reason

  putStrLn "Attempted to open Picocom for all target devices"

  let (preProcessFails, preProcessPasses) = partitionEithers preProcessResults

  unless (null preProcessFails) $ do
    let failReason = "Some preprocess steps failed. reasons:\n - " <> L.intercalate "\n - " preProcessFails
    forM_ preProcessPasses snd
    assertFailure failReason

  putStrLn "No failures for Picocom opening"

  flip finally (forM preProcessPasses snd) $ do
    putStrLn "Starting all targets to read DNA values"

    -- start all targets
    forM_ targets $ \(hwT, _) -> do
      openHwTarget v hwT
      execCmd_ v "set_property" ["PROBES.FILE", embrace ilaPath, "[current_hw_device]"]
      refresh_hw_device v []

      execCmd_ v "set_property" ["OUTPUT_VALUE", "1", getProbeTestStartTcl]
      commit_hw_vio v ["[get_hw_vios]"]

    putStrLn "Expecting specific DNAs for all serial ports"
    putStrLn "Serial ports:"
    mapM_ putStrLn [d.serial | (_, d) <- targets]

    results <- forM (L.zip targets preProcessPasses) $ \((_, d), (picoCom, picoClean)) -> do
      putStrLn $ "Waiting for output on port: " <> d.serial
      res <- checkDna d picoCom
      picoClean
      pure res

    print results
    if and results
      then pure ExitSuccess
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

    putStrLn "Terminal is ready!"

    receivedDna0 <- timeout 10_000_000 $ findDna pico ""
    receivedDna <- case receivedDna0 of
      Just rDna -> return rDna
      Nothing -> assertFailure "Timeout waiting for DNA"
    let
      expected = showHex d.dna ""
      differences = L.zipWith (\e a -> if e == a then ' ' else '^') expected receivedDna
      match = read ("0x" <> receivedDna) == (read ("0x" <> expected) :: Int)
    putStrLn $ "Serial path: " <> d.serial
    putStrLn $ "Expected DNA: " <> expected
    putStrLn $ "Received DNA: " <> receivedDna
    putStrLn $ "Differences:  " <> differences
    pure match
  findDna :: ProcessStdIoHandles -> String -> IO String
  findDna pico prev = do
    get <- BS.hGet pico.stdoutHandle 1
    let nC = L.head $ BS.unpack get
    if isHexDigit nC
      then findDna pico (prev <> [w2c nC])
      else
        if null prev
          then findDna pico prev
          else return prev
