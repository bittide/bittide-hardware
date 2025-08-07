-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Don't warn about partial functions: this is a test, so we'll see it fail.
{-# OPTIONS_GHC -Wno-x-partial #-}

module Wishbone.ScatterGather where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

import Data.Char (chr)
import Data.Maybe (catMaybes)
import Protocols
import Protocols.MemoryMap
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Bittide.Instances.Tests.ScatterGather (dut)

import qualified Prelude as P

sim :: IO ()
sim = putStr simResult

simResult :: (HasCallStack) => String
simResult = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream = sampleC def{timeoutAfter = 100_000} dutNoMM

  dutNoMM :: (HasCallStack) => Circuit () (Df System (BitVector 8))
  dutNoMM = circuit $ do
    mm <- ignoreMM
    uartTx <-
      withClockResetEnable clockGen (resetGenN d2) enableGen
        $ dut
        -< mm
    idC -< uartTx

case_scatter_gather_echo_test :: Assertion
case_scatter_gather_echo_test = do
  assertBool
    msg
    (P.head (lines simResult) == "Written data was read back correctly")
 where
  msg = "Received the following from the CPU over UART:\n" <> simResult

tests :: TestTree
tests = $(testGroupGenerator)
