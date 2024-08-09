-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Prelude

import Test.Tasty
import Test.Tasty.Hedgehog

import qualified Tests.Axi4
import qualified Tests.Axi4.Generators
import qualified Tests.Axi4.Properties
import qualified Tests.Calendar
import qualified Tests.ClockControl.Si539xSpi
import qualified Tests.DoubleBufferedRam
import qualified Tests.ElasticBuffer
import qualified Tests.Haxioms
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
tests =
  testGroup
    "Unittests"
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
    , Tests.Axi4.Generators.tests
    , Tests.Axi4.Properties.tests
    , Tests.Haxioms.tests
    ]

{- | Default number of tests is 100, which is too low for our (complicated)
state machinery.
-}
setDefaultHedgehogTestLimit :: HedgehogTestLimit -> HedgehogTestLimit
setDefaultHedgehogTestLimit (HedgehogTestLimit Nothing) = HedgehogTestLimit (Just 1000)
setDefaultHedgehogTestLimit opt = opt

{- | Hedgehog seemingly gets stuck in an infinite loop when shrinking - probably
due to the way our generators depend on each other. We limit the number of
shrinks to 100.
-}
setDefaultHedgehogShrinkLimit :: HedgehogShrinkLimit -> HedgehogShrinkLimit
setDefaultHedgehogShrinkLimit (HedgehogShrinkLimit Nothing) = HedgehogShrinkLimit (Just 100)
setDefaultHedgehogShrinkLimit opt = opt

main :: IO ()
main =
  defaultMain $
    adjustOption setDefaultHedgehogTestLimit $
      adjustOption setDefaultHedgehogShrinkLimit $
        tests
