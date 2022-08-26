-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Prelude
import Test.Tasty

import qualified Tests.Clash.Shake.Vivado.ParseTimingSummary

tests :: TestTree
tests = testGroup "Unittests"
  [ Tests.Clash.Shake.Vivado.ParseTimingSummary.tests
  ]

main :: IO ()
main = defaultMain $
  tests
