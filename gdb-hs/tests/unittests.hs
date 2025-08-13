-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Test.Tasty
import Prelude

import qualified Tests.Gdb.Commands.Read
import qualified Tests.Gdb.Internal

tests :: TestTree
tests =
  testGroup
    "Unittests"
    [ Tests.Gdb.Commands.Read.tests
    , Tests.Gdb.Internal.tests
    ]

main :: IO ()
main = defaultMain tests
