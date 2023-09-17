-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Prelude
import Test.Tasty

import Tests.OverflowResistantDiff

tests :: TestTree
tests = testGroup "Unittests"
  [ ordGroup
  ]

main :: IO ()
main = defaultMain $
  tests
