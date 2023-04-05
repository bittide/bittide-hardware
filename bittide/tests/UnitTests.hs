-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Prelude

import Test.Tasty
import Test.Tasty.Hedgehog

import Tests.Axi4
import Tests.Calendar
import Tests.ClockControl.Si539xSpi
import Tests.DoubleBufferedRam
import Tests.ElasticBuffer
import Tests.Haxioms
import Tests.Link
import Tests.ProcessingElement.ReadElf
import Tests.ScatterGather
import Tests.StabilityChecker
import Tests.Switch
import Tests.Wishbone

tests :: TestTree
tests = testGroup "Unittests"
  [ axi4Group
  , calGroup
  , clockGenGroup
  , ebGroup
  , haxiomsGroup
  , linkGroup
  , wbGroup
  , ramGroup
  , readElfTestGroup
  , sgGroup
  , stabilityGroup
  , switchGroup
  ]

setDefaultHedgehogTestLimit :: HedgehogTestLimit -> HedgehogTestLimit
setDefaultHedgehogTestLimit (HedgehogTestLimit Nothing) = HedgehogTestLimit (Just 1000)
setDefaultHedgehogTestLimit opt = opt

main :: IO ()
main = defaultMain $
  adjustOption setDefaultHedgehogTestLimit
  tests
