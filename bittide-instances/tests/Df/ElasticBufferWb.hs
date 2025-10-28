-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Don't warn about partial functions: this is a test, so we'll see it fail.
{-# OPTIONS_GHC -Wno-x-partial #-}

module Df.ElasticBufferWb where

import Bittide.Instances.Tests.ElasticBufferWb
import Clash.Explicit.Prelude
import Data.Char (chr)
import Data.Maybe (catMaybes, mapMaybe)
import Protocols
import Protocols.Idle
import Protocols.MemoryMap
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

-- | Simulate the UART output of the elastic buffer test
sim :: IO ()
sim = putStr simResult

simResult :: (HasCallStack) => String
simResult = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream = sampleC def{timeoutAfter = 300_000} testCircuit

  testCircuit :: Circuit () (Df XilinxSystem (BitVector 8))
  testCircuit = idleSource |> ignoreMM |> dut

{- | Test whether the elastic buffer can be controlled via Wishbone.
The firmware runs multiple tests and outputs a result for each and ends with
"All elastic buffer tests passed" if all tests succeed.
-}
case_elastic_buffer_wb_test :: Assertion
case_elastic_buffer_wb_test = do
  assertBool
    msg
    (firstTrue $ mapMaybe checkLine (lines simResult))
 where
  firstTrue (True : _) = True
  firstTrue _ = False

  msg = "Received the following from the CPU over UART:\n" <> simResult
  checkLine :: String -> Maybe Bool
  checkLine line
    | line == "All elastic buffer tests passed" = Just True
    | line == "Some elastic buffer tests failed" = Just False
    | otherwise = Nothing

tests :: TestTree
tests = $(testGroupGenerator)
