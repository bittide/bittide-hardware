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
  , testPropertyNamed "clog2axiom holds" "prop_clog2axiom"  prop_clog2axiom
  , testPropertyNamed "timesNDivRU holds" "prop_timesNDivRU" prop_timesNDivRU
  , testPropertyNamed "oneLTdivRU holds" "prop_oneLTdivRU" prop_oneLTdivRU
  , testPropertyNamed "leMaxLeft holds" "prop_leMaxLeft" prop_leMaxLeft
  , testPropertyNamed "leMaxRight holds" "prop_leMaxRight" prop_leMaxRight
  ]

-- | Given that naturals in this module are used in proofs, we don't bother
-- generating very large ones.
--
genNatural :: Natural -> Gen Natural
genNatural min_ = Gen.integral (Range.linear min_ 1000)

clog :: Natural -> Natural -> Natural
clog (fromIntegral -> base) (fromIntegral -> value) =
  ceiling (logBase @Double base value)

divRU :: Natural -> Natural -> Natural
divRU dividend divider =
  case dividend `divMod` divider of
    (n, 0) -> n
    (n, _) -> n + 1

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

-- | Test whether the following equation holds:
--
--     CLog 2 (n * 2) <=> CLog 2 n + 1
--
-- Given:
--
--     1 <= n
--
prop_clog2axiom :: Property
prop_clog2axiom = property $ do
  n <- forAll (genNatural 1)
  clog 2 (n * 2) === clog 2 n + 1

-- | Test whether the following equation holds:
--
--     DivRU (a * b) b ~ a
--
-- Given:
--
--     1 <= b
--
prop_timesNDivRU :: Property
prop_timesNDivRU = property $ do
  a <- forAll (genNatural 0)
  b <- forAll (genNatural 1)
  divRU (a * b) b === a

-- | Test whether the following equation holds:
--
--     Div ((a * b) + (b - 1)) b ~ a
--
-- Given:
--
--     1 <= b
--
-- XXX: FAILS
--
-- prop_timesNDivRU'' :: Property
-- prop_timesNDivRU'' = property $ do
--   a <- forAll (genNatural 0)
--   b <- forAll (genNatural 1)
--   divRU ((a * b) + (b - 1)) b === a

-- | Test whether the following equation holds:
--
--     1 <= DivRU a b
--
-- Given:
--
--     1 <= a, 1 <= b
--
prop_oneLTdivRU :: Property
prop_oneLTdivRU = property $ do
  a <- forAll (genNatural 1)
  b <- forAll (genNatural 1)
  assert (1 <= divRU a b)

-- | Test whether the following equation holds:
--
--     a <= Max (a + c) b
--
prop_leMaxLeft :: Property
prop_leMaxLeft = property $ do
  a <- forAll (genNatural 0)
  b <- forAll (genNatural 0)
  c <- forAll (genNatural 0)
  assert (a <= max (a + c) b)

-- | Test whether the following equation holds:
--
--     b <= Max a (b + c)
--
prop_leMaxRight :: Property
prop_leMaxRight = property $ do
  a <- forAll (genNatural 0)
  b <- forAll (genNatural 0)
  c <- forAll (genNatural 0)
  assert (b <= max a (b + c))
