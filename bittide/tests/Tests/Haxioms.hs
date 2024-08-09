-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}

module Tests.Haxioms where

import Clash.Prelude
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

prop_leMult :: Property
prop_leMult = property $ do
  a <- forAll $ Gen.int (Range.constant 1 1_000_000)
  b <- forAll $ Gen.int (Range.constant 1 1_000_000)
  assert (1 <= a * b)

tests :: TestTree
tests =
  testGroup
    "Haxioms"
    [ testProperty "prop_leMult" prop_leMult
    ]
