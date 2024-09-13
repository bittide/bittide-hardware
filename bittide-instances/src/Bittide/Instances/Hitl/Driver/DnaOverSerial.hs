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
import Bittide.Instances.Hitl.Utils.Vivado
import Control.Monad.Extra
import qualified Data.List as L
import Data.Maybe
import Numeric
import Project.Handle
import System.Exit
import System.IO
import System.Process
import System.Timeout
import Test.Tasty.HUnit
import Vivado
import Vivado.Tcl

{- | Test that all FPGAs that are programmed with `dnaOverSerial` transmit the
DNA that we expect based on the DeviceInfo.
-}
dnaOverSerialDriver ::
  VivadoHandle ->
  String ->
  FilePath ->
  [(HwTarget, DeviceInfo, ())] ->
  IO ExitCode
dnaOverSerialDriver v _name ilaPath targets = do
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
  results <- mapM checkDna [d | (_, d, _) <- targets]
  print results
  assertBool "Not all FPGAs transmitted the expected DNA" (and results)
  pure $ ExitSuccess
 where
  checkDna :: DeviceInfo -> IO Bool
  checkDna d = do
    let
      picocomProc =
        (proc "picocom" ["--baud", "9600", "--imap", "lfcrlf", "--omap", "lfcrlf", d.serial])
          { std_out = CreatePipe
          , std_in = CreatePipe
          , std_err = CreatePipe
          , new_session = True -- Seems to be required for picocom to work
          }
    withCreateProcess picocomProc $ \_ maybePicocomStdOut _maybePicocomStdErr _ -> do
      let
        picocomStdOutHandle = fromJust maybePicocomStdOut

      terminalReadyResult <-
        timeout 10_000_000 $ waitForLine picocomStdOutHandle "Terminal ready"
      when (isNothing terminalReadyResult) $ do
        assertFailure "Timeout waiting for \"Terminal ready\""

      _ <- hGetLine picocomStdOutHandle -- Discard a potentially incomplete line
      receivedDna <- hGetLine picocomStdOutHandle
      let
        expected = showHex d.dna ""
        differences = L.zipWith (\e a -> if e == a then ' ' else '^') expected receivedDna
        match = read ("0x" <> receivedDna) == (read ("0x" <> expected) :: Int)
      putStrLn $ "Serial path: " <> d.serial
      putStrLn $ "Expected DNA: " <> expected
      putStrLn $ "Received DNA: " <> receivedDna
      putStrLn $ "Differences:  " <> differences
      pure match
