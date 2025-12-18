-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Don't warn about partial functions: this is a test, so we'll see it fail.
{-# OPTIONS_GHC -Wno-x-partial #-}

module Wishbone.ScatterGather where

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

import Bittide.Instances.Tests.ScatterGather (dutWithBinary)

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
        $ (dutWithBinary "scatter_gather_test")
        -< mm
    idC -< uartTx

case_scatter_gather_echo_test :: Assertion
case_scatter_gather_echo_test = do
  assertBool
    msg
    (P.head (lines simResult) == "Written data was read back correctly")
 where
  msg = "Received the following from the CPU over UART:\n" <> simResult

-- C test simulation
simC :: IO ()
simC = putStr simResultC

simResultC :: (HasCallStack) => String
simResultC = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream = sampleC def{timeoutAfter = 200_000} dutNoMM

  dutNoMM :: (HasCallStack) => Circuit () (Df System (BitVector 8))
  dutNoMM = circuit $ do
    mm <- ignoreMM
    uartTx <-
      withClockResetEnable clockGen (resetGenN d2) enableGen
        $ (dutWithBinary "c_scatter_gather_test")
        -< mm
    idC -< uartTx

case_scatter_gather_c_test :: Assertion
case_scatter_gather_c_test = do
  assertBool
    msg
    ("Scatter/Gather HAL tests PASSED" `isInfixOf` simResultC)
 where
  msg = "Received the following from the CPU over UART:\n" <> simResultC

-- Aligned ringbuffer test simulation
simAlignedRingbuffer :: IO ()
simAlignedRingbuffer = putStr simResultAlignedRingbuffer

simResultAlignedRingbuffer :: (HasCallStack) => String
simResultAlignedRingbuffer = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream = sampleC def{timeoutAfter = 200_000} dutNoMM

  dutNoMM :: (HasCallStack) => Circuit () (Df System (BitVector 8))
  dutNoMM = circuit $ do
    mm <- ignoreMM
    uartTx <-
      withClockResetEnable clockGen (resetGenN d2) enableGen
        $ (dutWithBinary "aligned_ringbuffer_test")
        -< mm
    idC -< uartTx

case_aligned_ringbuffer_test :: Assertion
case_aligned_ringbuffer_test = do
  assertBool
    msg
    ("*** ALL TESTS PASSED ***" `isInfixOf` simResultAlignedRingbuffer)
 where
  msg = "Received the following from the CPU over UART:\n" <> simResultAlignedRingbuffer

tests :: TestTree
tests = $(testGroupGenerator)
