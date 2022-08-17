-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Prelude
import Test.Tasty

import Tests.Bittide.Simulate qualified

tests :: TestTree
tests =
  testGroup "Tests"
    [ testGroup "Bittide"
      [ Tests.Bittide.Simulate.tests
      ]
    ]

main :: IO ()
main = defaultMain tests
