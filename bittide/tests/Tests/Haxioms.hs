-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE  OverloadedStrings #-}

module Tests.Haxioms where

import Prelude
import Numeric.Natural

import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

haxiomsGroup :: TestTree
haxiomsGroup = testGroup "Haxioms"
  [ testPropertyNamed "timesDivRU holds" "prop_timesDivRU"  prop_timesDivRU
  ]

-- | Given that naturals in this module are used in proofs, we don't bother
-- generating very large ones.
--
genNatural :: Natural -> Gen Natural
genNatural min_ = Gen.integral (Range.linear min_ 1000)

-- | Test whether the following equation holds:
--
--      b <= Div (b + (a - 1)) a * a
--
-- Given:
--
--      1 <= a
--
prop_timesDivRU :: Property
prop_timesDivRU = property $ do
  a <- forAll (genNatural 1)
  b <- forAll (genNatural 0)
  assert (b <= (b + (a - 1) `div` a) * a)
