-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Test.Tasty
import Prelude

import qualified Tests.Gdb

tests :: TestTree
tests =
  testGroup
    "Unittests"
    [Tests.Gdb.tests]

main :: IO ()
main = defaultMain tests
