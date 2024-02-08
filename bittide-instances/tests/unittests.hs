-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Prelude
import Test.Tasty
import qualified Wishbone.Time as Time

import Tests.OverflowResistantDiff

tests :: TestTree
tests = testGroup "Unittests"
  [ ordGroup
  , Time.tests
  ]

main :: IO ()
main = defaultMain $
  tests
