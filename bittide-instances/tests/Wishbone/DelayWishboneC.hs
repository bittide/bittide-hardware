-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Wishbone.DelayWishboneC where

import Clash.Prelude

import Bittide.Instances.Tests.DelayWishboneC

import Data.Char
import Data.Maybe
import Protocols
import Protocols.MemoryMap (ignoreMM)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

simCpu :: IO ()
simCpu = putStr simResult

simResult :: String
simResult = takeWhile (/= '\x04') $ chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream = sampleC def dutFn

dutFn :: Circuit () (Df System (BitVector 8))
dutFn = circuit $ \_unit -> do
  mm <- ignoreMM
  uartTx <- withClockResetEnable clockGen (resetGenN d2) enableGen dutCpu -< mm
  idC -< uartTx

case_test_wishboneDelayC :: Assertion
case_test_wishboneDelayC = assertEqual msg simResult expected
 where
  expected = "WhoAmID: helo\n"
  msg = "Simulation result " <> simResult <> " not equal to expected string "

tests :: TestTree
tests = $(testGroupGenerator)
