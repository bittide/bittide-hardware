-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Don't warn about orphan instances, caused by `createDomain`.
{-# OPTIONS_GHC -Wno-orphans #-}
-- Don't warn about partial functions: this is a test, so we'll see it fail.
{-# OPTIONS_GHC -Wno-x-partial #-}

module Wishbone.Ringbuffer where

import Clash.Explicit.Prelude

import Data.List (isInfixOf)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Bittide.Instances.Tests.Ringbuffer (simResultRingbuffer)

case_ringbuffer_test :: Assertion
case_ringbuffer_test = do
  assertBool
    msg
    ("*** ALL TESTS PASSED ***" `isInfixOf` simResultRingbuffer)
 where
  msg = "Received the following from the CPU over UART:\n" <> simResultRingbuffer

tests :: TestTree
tests = $(testGroupGenerator)
