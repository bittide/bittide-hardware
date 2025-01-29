-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Prelude

import Test.Tasty

import qualified Tests.ClockControlWb as ClockControlWb
import qualified Tests.OverflowResistantDiff as Ord
import qualified Tests.PanicBacktraces as Pb
import qualified Wishbone.Axi as Axi
import qualified Wishbone.CaptureUgn as CaptureUgn
import qualified Wishbone.DnaPortE2 as DnaPortE2
import qualified Wishbone.Time as Time
import qualified Wishbone.Watchdog as Watchdog

tests :: TestTree
tests =
  testGroup
    "Unittests"
    [ CaptureUgn.tests
    , ClockControlWb.tests
    , DnaPortE2.tests
    , Ord.tests
    , Time.tests
    , Axi.tests
    , Watchdog.tests
    , Pb.tests
    ]

main :: IO ()
main = defaultMain tests
