-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Prelude

import Test.Tasty

import qualified Tests.ClockControlWb as ClockControlWb
import qualified Tests.OverflowResistantDiff as Ord
import qualified Wishbone.Axi as Axi
import qualified Wishbone.CaptureUgn as CaptureUgn
import qualified Wishbone.DnaPortE2 as DnaPortE2
import qualified Wishbone.ScatterGather as ScatterGather
import qualified Wishbone.SwitchDemoProcessingElement as SwitchDemoProcessingElement
import qualified Wishbone.Time as Time
import qualified Wishbone.Watchdog as Watchdog

tests :: TestTree
tests =
  testGroup
    "Unittests"
    [ Axi.tests
    , CaptureUgn.tests
    , ClockControlWb.tests
    , DnaPortE2.tests
    , Ord.tests
    , ScatterGather.tests
    , SwitchDemoProcessingElement.tests
    , Time.tests
    , Watchdog.tests
    ]

main :: IO ()
main = defaultMain tests
