-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Test.Tasty
import Test.Tasty.Hedgehog

import Tests.Calendar
import Tests.DoubleBufferedRAM
import Tests.ScatterGather

tests :: TestTree
tests = testGroup "Unittests"
  [calGroup, sgGroup, ramGroup]

setDefaultHedgehogTestLimit :: HedgehogTestLimit -> HedgehogTestLimit
setDefaultHedgehogTestLimit (HedgehogTestLimit Nothing) = HedgehogTestLimit (Just 10000)
setDefaultHedgehogTestLimit opt = opt

main :: IO ()
main = defaultMain $
  adjustOption setDefaultHedgehogTestLimit
  tests
