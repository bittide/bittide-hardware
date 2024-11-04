-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main where

import Clash.Prelude
import Prelude ()

import Control.Monad.Extra
import qualified Data.List as L
import Data.Maybe
import Numeric
import System.Environment (withArgs)
import System.IO
import System.Process
import System.Timeout
import Test.Tasty.HUnit
import Test.Tasty.TH

import Bittide.Instances.Hitl.Setup
import Project.Handle

{- | Test that all FPGAs that are programmed with `dnaOverSerial` transmit the
DNA that we expect based on the DeviceInfo.
-}
case_dnaOverSerial :: Assertion
case_dnaOverSerial = do
  putStrLn "Expecting specific DNAs for all serial ports"
  putStrLn "Serial ports:"
  mapM_ putStrLn [d.serial | d <- demoRigInfo]
  results <- mapM checkDna demoRigInfo
  print results
  assertBool "Not all FPGAs transmitted the expected DNA" (and results)
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

main :: IO ()
main = withArgs [] $defaultMainGenerator
