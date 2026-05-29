-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Don't warn about partial functions: this is a test, so we'll see it fail.
{-# OPTIONS_GHC -Wno-x-partial #-}

module Wishbone.DnaPortE2 where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

import Clash.Cores.Xilinx.Unisim.DnaPortE2
import Data.Char
import Data.Maybe
import Numeric
import Protocols.Experimental.Simulate (sampleC)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Bittide.Instances.Tests.DnaPortE2 (dut, peConfigSim)
import Bittide.ProcessingElement

import qualified Prelude as P

sim :: IO ()
sim = do
  peConfig <- peConfigSim
  putStr $ simResult peConfig

simResult :: PeConfig 4 -> String
simResult peConfig = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream =
    sampleC def
      $ withClockResetEnable clockGen (resetGenN d2) enableGen
      $ dut @System peConfig

-- | Test whether we can read the DNA from the DNA port peripheral.
case_dna_port_self_test :: Assertion
case_dna_port_self_test = do
  peConfig <- peConfigSim
  let
    msg =
      "Received dna "
        <> showHex receivedDna ""
        <> " not equal to expected dna "
        <> showHex simDna2 ""
    receivedDna = parseResult $ simResult peConfig
  assertBool msg (receivedDna == simDna2)

parseResult :: String -> BitVector 96
parseResult = pack . (read :: String -> Unsigned 96) . P.head . lines

tests :: TestTree
tests = $(testGroupGenerator)
