-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Tests.Clash.Cores.Xilinx.Dna where

import Clash.Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe

import Clash.Cores.Xilinx.Dna

import qualified Data.List as L
-- Simulate deviceDna and check if it produces the simulation DNA within 1000 cycles
testDeviceDna :: TestTree
testDeviceDna = testCase "deviceDna produces simulation DNA" $ do
  let
    result = catMaybes $ sampleN @System 1000 (deviceDna defaultSimDNA)
  assertBool "No results produced" (not $ null result)
  assertEqual "Unexpected result" result (L.replicate (L.length result) defaultSimDNA)

-- Add this test to your existing test group
dnaTestGroup :: TestTree
dnaTestGroup = testGroup "Xilinx DNA tests" [testDeviceDna]
