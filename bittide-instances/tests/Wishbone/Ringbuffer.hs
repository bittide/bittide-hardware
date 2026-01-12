-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Don't warn about orphan instances, caused by `createDomain`.
{-# OPTIONS_GHC -Wno-orphans #-}
-- Don't warn about partial functions: this is a test, so we'll see it fail.
{-# OPTIONS_GHC -Wno-x-partial #-}

module Wishbone.Ringbuffer where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

import Data.Char (chr)
import Data.List (isInfixOf)
import Data.Maybe (catMaybes)
import Protocols
import Protocols.MemoryMap
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Bittide.Instances.Tests.Ringbuffer (dutWithBinary)

createDomain vSystem{vName = "Slow", vPeriod = hzToPeriod 1000000}

-- Ringbuffer test simulation
simRingbuffer :: IO ()
simRingbuffer = putStr simResultRingbuffer

simResultRingbuffer :: (HasCallStack) => String
simResultRingbuffer = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream = sampleC def{timeoutAfter = 200_000} dutNoMM

  dutNoMM :: (HasCallStack) => Circuit () (Df System (BitVector 8))
  dutNoMM = circuit $ do
    mm <- ignoreMM
    uartTx <-
      withClockResetEnable clockGen (resetGenN d2) enableGen
        $ (dutWithBinary "ringbuffer_test")
        -< mm
    idC -< uartTx

case_ringbuffer_test :: Assertion
case_ringbuffer_test = do
  assertBool
    msg
    ("*** ALL TESTS PASSED ***" `isInfixOf` simResultRingbuffer)
 where
  msg = "Received the following from the CPU over UART:\n" <> simResultRingbuffer

tests :: TestTree
tests = $(testGroupGenerator)
