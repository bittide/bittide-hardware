-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Test.Tasty
import Prelude

tests :: TestTree
tests =
  testGroup
    "Unittests"
    []

main :: IO ()
main = defaultMain tests
