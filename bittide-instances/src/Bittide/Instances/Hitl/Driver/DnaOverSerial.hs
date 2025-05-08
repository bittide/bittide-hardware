-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DuplicateRecordFields #-}
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
import Control.Monad.Extra
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.ByteString.Internal (w2c)
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
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM

{- | Test that all FPGAs that are programmed with `dnaOverSerial` transmit the
DNA that we expect based on the DeviceInfo.
-}
dnaOverSerialDriver ::
  String ->
  [(HwTarget, DeviceInfo)] ->
  VivadoM ExitCode
dnaOverSerialDriver _name targets = do
  results <- brackets (liftIO <$> initPicocoms) (liftIO . snd) $ \initPicocomsData -> do
    let targetPicocoms = fst <$> initPicocomsData

    liftIO $ putStrLn "Starting all targets to read DNA values"
    -- start all targets
    forM_ targets $ \(hwT, _) -> do
      openHardwareTarget hwT
      updateVio "vioHitlt" [("probe_test_start", "1")]

    liftIO $ putStrLn "Expecting specific DNAs for all serial ports"
    liftIO $ putStrLn "Serial ports:"
    mapM_ (liftIO . putStrLn) [d.serial | (_, d) <- targets]

    forM (L.zip targets targetPicocoms) $ \((_, d), picoCom) -> do
      liftIO $ putStrLn $ "Waiting for output on port: " <> d.serial
      res <- liftIO $ checkDna d picoCom
      pure res

  liftIO $ print results
  if and results
    then pure ExitSuccess
    else do
      liftIO $ assertFailure "Not all FPGAs transmitted the expected DNA"
      pure $ ExitFailure 2
 where
  initPicocoms :: [IO (ProcessStdIoHandles, IO ())]
  initPicocoms = flip L.map targets $ \(hwT, dI) -> do
    let targetId = idFromHwT hwT
    let targetIndex = fromMaybe 9 $ L.findIndex (\d -> d.deviceId == targetId) demoRigInfo

    projectDir <- findParentContaining "cabal.project"
    let
      hitlDir = projectDir </> "_build" </> "hitl"
      stdoutLog = hitlDir </> "picocom-stdout." <> show targetIndex <> ".log"
      stderrLog = hitlDir </> "picocom-stderr." <> show targetIndex <> ".log"
      picocomConfig =
        PicocomConfig
          { devPath = dI.serial
          , baudRate = Just 9600
          , stdoutPath = Just stdoutLog
          , stderrPath = Just stderrLog
          }
    putStrLn $ "logging stdout to `" <> stdoutLog <> "`"
    putStrLn $ "logging stderr to `" <> stderrLog <> "`"

    (pico, picoClean) <- startPicocom picocomConfig

    hSetBuffering pico.stdinHandle LineBuffering
    hSetBuffering pico.stdoutHandle LineBuffering

    pure (pico, picoClean)

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
