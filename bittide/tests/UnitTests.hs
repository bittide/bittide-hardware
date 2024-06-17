-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Prelude

import Test.Tasty
import Test.Tasty.Hedgehog

import qualified Tests.Axi4
import qualified Tests.Calendar
import qualified Tests.ClockControl.Si539xSpi
import qualified Tests.DoubleBufferedRam
import qualified Tests.ElasticBuffer
import qualified Tests.Link
import qualified Tests.ProcessingElement.ReadElf
import qualified Tests.ScatterGather
import qualified Tests.StabilityChecker
import qualified Tests.Switch
import qualified Tests.Transceiver
import qualified Tests.Transceiver.Prbs
import qualified Tests.Transceiver.WordAlign
import qualified Tests.Wishbone

tests :: TestTree
tests = testGroup "Unittests"
  [ Tests.Axi4.tests
  , Tests.Calendar.tests
  , Tests.ClockControl.Si539xSpi.tests
  , Tests.DoubleBufferedRam.tests
  , Tests.ElasticBuffer.tests
  , Tests.Link.tests
  , Tests.ProcessingElement.ReadElf.tests
  , Tests.ScatterGather.tests
  , Tests.StabilityChecker.tests
  , Tests.Switch.tests
  , Tests.Transceiver.tests
  , Tests.Transceiver.Prbs.tests
  , Tests.Transceiver.WordAlign.tests
  , Tests.Wishbone.tests
  ]

setDefaultHedgehogTestLimit :: HedgehogTestLimit -> HedgehogTestLimit
setDefaultHedgehogTestLimit (HedgehogTestLimit Nothing) = HedgehogTestLimit (Just 1000)
setDefaultHedgehogTestLimit opt = opt

main :: IO ()
main = defaultMain $
  adjustOption setDefaultHedgehogTestLimit
  tests
