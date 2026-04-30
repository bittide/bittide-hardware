-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Tests.Protocols.MemoryMap.Mask where

import Clash.Explicit.Prelude

import Clash.Class.BitPackC (ByteOrder (..), maybeUnpackC, packC)
import Hedgehog (Property, forAll, property, (===))
import Test.Tasty
import Test.Tasty.Hedgehog (testPropertyNamed)

import Protocols.MemoryMap.Mask (Mask, fromBitVector, fromVec, toBitVector, toVec)

import qualified Clash.Hedgehog.Sized.Vector as HV
import qualified Hedgehog.Gen as Gen

prop_vecRoundTrip :: Property
prop_vecRoundTrip = property $ do
  bs <- forAll (HV.genVec @17 Gen.bool)
  toVec (fromVec bs) === bs

prop_bitVectorRoundTrip :: Property
prop_bitVectorRoundTrip = property $ do
  bv <- forAll (Gen.enumBounded @_ @(BitVector 17))
  toBitVector (fromBitVector bv) === bv

-- | Index 0 of the input 'Vec' becomes bit 0 (LSB) of the resulting 'BitVector'.
prop_indexConvention :: Property
prop_indexConvention = property $ do
  let v0 = True :> repeat False :: Vec 8 Bool
  let bv = toBitVector (fromVec v0)
  bv === 0b0000_0001

  let v1 = repeat False :< True :: Vec 8 Bool
  let bv1 = toBitVector (fromVec v1)
  bv1 === 0b1000_0000

prop_packCRoundTrip :: Property
prop_packCRoundTrip = property $ do
  bv <- forAll (Gen.enumBounded @_ @(BitVector 17))
  let m = fromBitVector bv :: Mask 17
  let packed = packC LittleEndian m
  case maybeUnpackC LittleEndian packed of
    Nothing -> fail "maybeUnpackC returned Nothing"
    Just (m' :: Mask 17) -> toBitVector m' === bv

tests :: TestTree
tests =
  testGroup
    "Mask"
    [ testPropertyNamed
        "fromVec . toVec ≡ id (Vec 17 Bool)"
        "prop_vecRoundTrip"
        prop_vecRoundTrip
    , testPropertyNamed
        "fromBitVector . toBitVector ≡ id (BitVector 17)"
        "prop_bitVectorRoundTrip"
        prop_bitVectorRoundTrip
    , testPropertyNamed
        "Index 0 maps to LSB"
        "prop_indexConvention"
        prop_indexConvention
    , testPropertyNamed
        "BitPackC round-trip via packC/maybeUnpackC"
        "prop_packCRoundTrip"
        prop_packCRoundTrip
    ]
