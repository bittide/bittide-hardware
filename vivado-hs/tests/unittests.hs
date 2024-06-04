-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Test.Tasty

import Tests.Vivado qualified

tests :: TestTree
tests =
  testGroup
    "unittests"
    [ Tests.Vivado.tests
    ]

main :: IO ()
main = defaultMain tests
