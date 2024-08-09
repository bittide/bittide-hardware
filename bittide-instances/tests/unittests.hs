-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Prelude

import Test.Tasty

import qualified Tests.OverflowResistantDiff as Ord
import qualified Wishbone.Axi as Axi
import qualified Wishbone.DnaPortE2 as DnaPortE2
import qualified Wishbone.Time as Time

tests :: TestTree
tests =
  testGroup
    "Unittests"
    [ DnaPortE2.tests
    , Ord.tests
    , Time.tests
    , Axi.tests
    ]

main :: IO ()
main = defaultMain tests
