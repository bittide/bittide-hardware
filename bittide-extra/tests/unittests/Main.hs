-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Prelude

import Test.Tasty

import qualified Tests.Numeric.Extra
import Test.Tasty.Hedgehog (HedgehogTestLimit(..))

setDefaultHedgehogTestLimit :: HedgehogTestLimit -> HedgehogTestLimit
setDefaultHedgehogTestLimit (HedgehogTestLimit Nothing) = HedgehogTestLimit (Just 10000)
setDefaultHedgehogTestLimit opt = opt

tests :: TestTree
tests = testGroup "tests"
  [ Tests.Numeric.Extra.tests
  ]

main :: IO ()
main = defaultMain $
  adjustOption setDefaultHedgehogTestLimit
  tests