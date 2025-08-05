-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Tests.VexRiscv.Random where

import Clash.Hedgehog.Sized.BitVector
import Clash.Prelude
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import VexRiscv.Random

tests :: TestTree
tests =
  testGroup
    "VexRiscv.Random"
    [ testProperty "genNatural" prop_genNatural
    , testProperty "makeDefinedRandomBitVector" prop_makeDefinedRandomBitVector
    , testProperty "makeDefinedRandomBit" prop_makeDefinedRandomBit
    ]

prop_genNatural :: Property
prop_genNatural = property $ do
  lo <- forAll $ Gen.integral (Range.linear 0 (shiftL 1 1024))
  hi <- forAll $ Gen.integral (Range.linear lo (shiftL 1 1024))
  n <- evalIO $ genNatural (lo, hi)
  assert ((n >= lo) && (n <= hi))

prop_makeDefinedRandomBitVector :: Property
prop_makeDefinedRandomBitVector = property $ do
  someBv <- forAll $ (genSomeBitVector @0) (Range.linear 0 1024) genBitVector
  case someBv of
    SomeBitVector SNat bv -> do
      definedBv <- evalIO $ makeDefinedRandom bv
      assert (not $ hasUndefined definedBv)
      assert (definedBv <= maxBound)

prop_makeDefinedRandomBit :: Property
prop_makeDefinedRandomBit = property $ do
  b <- forAll $ genBit
  definedB <- evalIO $ makeDefinedRandom b
  assert (not $ hasUndefined definedB)
