-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Prelude

import Test.Tasty
import Test.Tasty.Hedgehog

import Tests.Calendar
import Tests.DoubleBufferedRam
import Tests.Link
import Tests.ScatterGather
import Tests.Switch
import Tests.Wishbone

tests :: TestTree
tests = testGroup "Unittests"
  [calGroup, sgGroup, ramGroup, memMapGroup, linkGroup, switchGroup]

setDefaultHedgehogTestLimit :: HedgehogTestLimit -> HedgehogTestLimit
setDefaultHedgehogTestLimit (HedgehogTestLimit Nothing) = HedgehogTestLimit (Just 10000)
setDefaultHedgehogTestLimit opt = opt

main :: IO ()
main = defaultMain $
  adjustOption setDefaultHedgehogTestLimit
  tests
