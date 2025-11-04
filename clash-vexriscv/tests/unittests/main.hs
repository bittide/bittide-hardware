-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Test.Tasty
import Test.Tasty.Hedgehog
import Prelude

import qualified Tests.VexRiscv.ClockTicks
import qualified Tests.VexRiscv.Random
import qualified Tests.VexRiscv.Reset

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ Tests.VexRiscv.ClockTicks.tests
    , Tests.VexRiscv.Random.tests
    , Tests.VexRiscv.Reset.tests
    ]

setDefaultHedgehogTestLimit :: HedgehogTestLimit -> HedgehogTestLimit
setDefaultHedgehogTestLimit (HedgehogTestLimit Nothing) = HedgehogTestLimit (Just 1000)
setDefaultHedgehogTestLimit opt = opt

main :: IO ()
main =
  defaultMain $
    adjustOption
      setDefaultHedgehogTestLimit
      tests
