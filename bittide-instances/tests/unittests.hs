-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Prelude
import Test.Tasty

tests :: TestTree
tests = testGroup "Unittests"
  [
  ]

main :: IO ()
main = defaultMain $
  tests
