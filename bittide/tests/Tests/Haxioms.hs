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
  , testPropertyNamed "clogProductRule holds" "prop_clogProductRule"  prop_clogProductRule
  , testPropertyNamed "cancelMulDiv holds" "prop_cancelMulDiv" prop_cancelMulDiv
  , testPropertyNamed "strictlyPositiveDivRu holds" "prop_strictlyPositiveDivRu" prop_strictlyPositiveDivRu
  , testPropertyNamed "leMaxLeft holds" "prop_leMaxLeft" prop_leMaxLeft
  , testPropertyNamed "leMaxRight holds" "prop_leMaxRight" prop_leMaxRight
  , testPropertyNamed "divWithRemainder holds" "prop_divWithRemainder" prop_divWithRemainder
  , testPropertyNamed "euclid3 holds" "prop_euclid3" prop_euclid3
  , testPropertyNamed "prop_oneLeCLog2n holds" "prop_oneLeCLog2n" prop_oneLeCLog2n
  , testPropertyNamed "prop_lessThanMax holds" "prop_lessThanMax" prop_lessThanMax
  , testPropertyNamed "prop_useLowerLimit holds" "prop_useLowerLimit" prop_useLowerLimit
  , testPropertyNamed "prop_oneMore holds" "prop_oneMore" prop_oneMore
  , testPropertyNamed "prop_isOne holds" "prop_isOne" prop_isOne
  ]

-- | Generate a 'Natural' greater than or equal to /n/. Can generate 'Natural's
-- up to /n+1000/. This should be enough, given that naturals in this module are
-- used in proofs.
genNatural :: Natural -> Gen Natural
genNatural min_ = Gen.integral (Range.linear min_ (1000 + min_))

-- | Like 'CLog', but at term-level. Operates on 'Double's internally, so it will
-- probably fail on very large naturals. This is irrelevant for this module though,
-- see 'genNatural'.
--
clog :: Natural -> Natural -> Natural
clog (fromIntegral -> base) (fromIntegral -> value) =
  ceiling (logBase @Double base value)

-- | Like 'DivRU', but at term-level.
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
-- Tests: 'Data.Constraint.Nat.Extra.timesDivRU'.
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
-- Tests: 'Data.Constraint.Nat.Extra.clogProductRule'.
--
prop_clogProductRule :: Property
prop_clogProductRule = property $ do
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
-- Tests: 'Data.Constraint.Nat.Extra.cancelMulDiv'.
--
prop_cancelMulDiv :: Property
prop_cancelMulDiv = property $ do
  a <- forAll (genNatural 0)
  b <- forAll (genNatural 1)
  divRU (a * b) b === a

-- | Test whether the following equation holds:
--
--     1 <= DivRU a b
--
-- Given:
--
--     1 <= a, 1 <= b
--
-- Tests: 'Data.Constraint.Nat.Extra.strictlyPositiveDivRu'.
--
prop_strictlyPositiveDivRu :: Property
prop_strictlyPositiveDivRu = property $ do
  a <- forAll (genNatural 1)
  b <- forAll (genNatural 1)
  assert (1 <= divRU a b)

-- | Test whether the following equation holds:
--
--     a <= Max (a + c) b
--
-- Tests: 'Data.Constraint.Nat.Extra.leMaxLeft'.
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
-- Tests: 'Data.Constraint.Nat.Extra.leMaxRight'.
--
prop_leMaxRight :: Property
prop_leMaxRight = property $ do
  a <- forAll (genNatural 0)
  b <- forAll (genNatural 0)
  c <- forAll (genNatural 0)
  assert (b <= max a (b + c))

-- | Test whether the following equation holds:
--
--     Div ((a * b) + c) b ~ a
--
-- Given:
--
--     1 <= b, c <= (b - 1)
--
-- Tests: 'Data.Constraint.Nat.Extra.divWithRemainder'.
--
prop_divWithRemainder :: Property
prop_divWithRemainder = property $ do
  a <- forAll (genNatural 0)
  b <- forAll (genNatural 1)
  c <- forAll (Gen.integral (Range.linear 0 (b - 1)))
  ((a * b) + c) `div` b === a

-- | Test whether the following equation holds:
--
--     a <= c - b
--
-- Given:
--
--     a + b <= c
--
prop_euclid3 :: Property
prop_euclid3 = property $ do
  a <- forAll (genNatural 0)
  b <- forAll (genNatural 0)
  c <- forAll (genNatural (a + b))
  (a + b <= c) === (a <= c - b)

-- | Test whether the following equation (supplied by 'oneLeCLog2n') holds:
--
--     1 <= CLog 2 n
--
-- Given:
--
--     2 <= n
--
prop_oneLeCLog2n :: Property
prop_oneLeCLog2n = property $ do
  n <- forAll (genNatural 2)
  assert (1 <= clog 2 n)

-- | Test whether the following equation (supplied by 'lessThanMax') holds:
--
--     c <= Max a b
--
-- Given:
--
--     c <= a, c <= b
--
prop_lessThanMax :: Property
prop_lessThanMax = property $ do
  c <- forAll (genNatural 0)
  a <- forAll (genNatural c)
  b <- forAll (genNatural c)
  assert (c <= max a b)

-- | Test whether the following equation (supplied by 'useLowerLimit') holds:
--
--     1 + n <= u
--
-- Given:
--
--     1 <= m, n + m <= u
--
prop_useLowerLimit :: Property
prop_useLowerLimit = property $ do
  m <- forAll (genNatural 1)     -- 1 <= m
  n <- forAll (genNatural 0)     -- no constraints
  u <- forAll (genNatural (n+m)) -- n + m <= u
  assert (1 + n <= u)

-- | Test whether the following equation (supplied by 'oneMore') holds:
--
--     1 <= Div n m + OneMore(Mod n m)
--
-- Given:
--
--     1 <= n, 1 <= m
--
prop_oneMore :: Property
prop_oneMore = property $ do
  n <- forAll (genNatural 1)     -- 1 <= n
  m <- forAll (genNatural 1)     -- 1 <= m
  assert (1 <= (n `div` m) + oneMore(n `mod` m))

-- | Test whether the following equation (supplied by 'isOne') holds:
--
--     1 ~ Div n m + OneMore(Mod n m)
--
-- Given:
--
--     1 <= n, n <= m
--
prop_isOne :: Property
prop_isOne = property $ do
  n <- forAll (genNatural 1)     -- 1 <= n
  m <- forAll (genNatural n)     -- n <= m
  assert (1 == (n `div` m) + oneMore(n `mod` m))

-- | See 'Data.Constraint.Nat.Extra'.
oneMore :: Natural -> Natural
oneMore = \case
  0 -> 0
  _ -> 1
