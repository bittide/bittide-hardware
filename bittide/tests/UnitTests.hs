-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Prelude

import Test.Tasty
import Test.Tasty.Hedgehog

import Tests.Axi4
import Tests.Calendar
import Tests.ClockControl.Si539xSpi
import Tests.Clash.Cores.Xilinx.Dna
import Tests.DoubleBufferedRam
import Tests.ElasticBuffer
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
  , dnaTestGroup
  , ebGroup
  , linkGroup
  , ramGroup
  , readElfTestGroup
  , sgGroup
  , stabilityGroup
  , switchGroup
  , wbGroup
  ]

setDefaultHedgehogTestLimit :: HedgehogTestLimit -> HedgehogTestLimit
setDefaultHedgehogTestLimit (HedgehogTestLimit Nothing) = HedgehogTestLimit (Just 1000)
setDefaultHedgehogTestLimit opt = opt

main :: IO ()
main = defaultMain $
  adjustOption setDefaultHedgehogTestLimit
  tests
