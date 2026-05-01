-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Don't warn about partial functions: this is a test, so we'll see it fail.
{-# OPTIONS_GHC -Wno-x-partial #-}

module Wishbone.Watchdog where

-- Preludes
import Clash.Prelude

-- Local
import Bittide.Instances.Tests.Watchdog (dut, peConfigSim)
import Bittide.ProcessingElement

-- Other
import Data.Char
import Data.Maybe
import Protocols
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

-- Qualified
import qualified Data.List as L

sim :: IO ()
sim = do
  peConfig <- peConfigSim
  putStrLn $ simResult peConfig

simResult :: PeConfig 6 -> String
simResult peConfig = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream = sampleC def $ dut peConfig

{- | Run the timing module self test with processingElement and inspect it's uart output.
The test returns names of tests and a boolean indicating if the test passed.
-}
case_time_rust_self_test :: Assertion
case_time_rust_self_test = do
  peConfig <- peConfigSim
  let result = L.head $ lines $ simResult peConfig
  assertEqual "Measured timeout wrong " "Timeout took 50 microseconds" result

tests :: TestTree
tests = $(testGroupGenerator)
