-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Don't warn about partial functions: this is a test, so we'll see it fail.
{-# OPTIONS_GHC -Wno-x-partial #-}

module Wishbone.ScatterGather where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.Instances.Common (PeConfigElfSource (NameOnly), peConfigFromElf)
import Bittide.ProcessingElement (PeConfig)
import Data.Char (chr)
import Data.List (isInfixOf)
import Data.Maybe (catMaybes)
import Project.FilePath (CargoBuildType (Release))
import Protocols
import Protocols.MemoryMap
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Bittide.Instances.Tests.ScatterGather (DMemWords, IMemWords, dutWithPeConfig)

import qualified Prelude as P

peConfigSim :: String -> IO (PeConfig 7)
peConfigSim binName =
  peConfigFromElf
    (SNat @IMemWords)
    (SNat @DMemWords)
    (NameOnly binName)
    Release
    d0
    d0
    False
    vexRiscv0

sim :: IO ()
sim = do
  peConfig <- peConfigSim "scatter_gather_test"
  putStr $ simResult peConfig

simResult :: (HasCallStack) => PeConfig 7 -> String
simResult peConfig = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream =
    sampleC def{timeoutAfter = 100_000}
      $ withClockResetEnable @System clockGen (resetGenN d2) enableGen
      $ unMemmap
      $ dutWithPeConfig peConfig

case_scatter_gather_echo_test :: Assertion
case_scatter_gather_echo_test = do
  peConfig <- peConfigSim "scatter_gather_test"
  let
    result = simResult peConfig
    msg = "Received the following from the CPU over UART:\n" <> result
  assertBool
    msg
    (P.head (lines $ simResult peConfig) == "Written data was read back correctly")

-- C test simulation
simC :: IO ()
simC = do
  peConfig <- peConfigSim "c_scatter_gather_test"
  putStr $ simResult peConfig

case_scatter_gather_c_test :: Assertion
case_scatter_gather_c_test = do
  peConfig <- peConfigSim "c_scatter_gather_test"
  let
    result = simResult peConfig
    msg = "Received the following from the CPU over UART:\n" <> result
  assertBool
    msg
    ("Scatter/Gather HAL tests PASSED" `isInfixOf` result)
 where

tests :: TestTree
tests = $(testGroupGenerator)
