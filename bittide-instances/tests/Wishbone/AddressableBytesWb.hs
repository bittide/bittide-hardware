-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Wishbone.AddressableBytesWb where

import Clash.Prelude

import Data.List (isInfixOf)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Test.Tasty.TH (testGroupGenerator)

import Bittide.Instances.Tests.AddressableBytesWb (dutUartStream, getDumpVcd, peConfigSim)

case_sim :: Assertion
case_sim = do
  dumpVcd <- getDumpVcd
  peConfig <- peConfigSim
  let
    simResult = dutUartStream dumpVcd peConfig
    msg = "Received the following from the CPU over UART:\n" <> simResult
  assertBool msg ("RESULT: OK" `isInfixOf` simResult)

tests :: TestTree
tests = $(testGroupGenerator)
