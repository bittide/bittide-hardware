-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Tests.Words where

import Clash.Prelude hiding (someNatVal, withSomeSNat)

import Clash.Class.BitPackC.Words (pad, unpad)
import Clash.Hedgehog.Sized.BitVector (genDefinedBitVector)
import Clash.Hedgehog.Sized.Vector (genVec)
import Data.Data (Proxy)
import GHC.TypeNats (someNatVal)
import Hedgehog (Property, annotateShow, forAll, property, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testPropertyNamed)

import qualified Data.List.Extra as L
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

type Bytes n = BitVector (n * 8)

-- | Test that the lowest index byte ends up in the LSB of the first word
test_pad_byte_ordering_4bytes_2word :: TestTree
test_pad_byte_ordering_4bytes_2word = testCase "pad: 4 bytes into 2-byte words" $ do
  let input = 0xAA :> 0xBB :> 0xCC :> 0xDD :> Nil :: Vec 4 (BitVector 8)
      result = pad @4 @2 input
      expected = 0xBBAA :> 0xDDCC :> Nil :: Vec 2 (BitVector 16)
  result @?= expected

-- | Test that unpad correctly reverses the packing
test_unpad_byte_ordering_4bytes_2word :: TestTree
test_unpad_byte_ordering_4bytes_2word = testCase "unpad: 2-byte words into 4 bytes" $ do
  let input = 0xBBAA :> 0xDDCC :> Nil :: Vec 2 (BitVector 16)
      result = unpad @4 input
      expected = 0xAA :> 0xBB :> 0xCC :> 0xDD :> Nil :: Vec 4 (BitVector 8)
  result @?= expected

-- | Test pad with 3 bytes into 4-byte words (requires padding)
test_pad_with_padding :: TestTree
test_pad_with_padding = testCase "pad: 3 bytes into 4-byte words (with padding)" $ do
  let input = 0x11 :> 0x22 :> 0x33 :> Nil :: Vec 3 (BitVector 8)
      result = pad @3 @4 input
      -- Bytes: [0x11, 0x22, 0x33] + padding byte 0x00
      -- Packed: 0x00332211 (byte 0 at LSB, padding at MSB)
      expected = 0x00332211 :> Nil :: Vec 1 (BitVector 32)
  result @?= expected

-- | Test unpad ignores padding bytes
test_unpad_removes_padding :: TestTree
test_unpad_removes_padding = testCase "unpad: removes padding bytes" $ do
  let input = 0x00332211 :> Nil :: Vec 1 (BitVector 32)
      result = unpad @3 input
      expected = 0x11 :> 0x22 :> 0x33 :> Nil :: Vec 3 (BitVector 8)
  result @?= expected

withSomeSNat :: Natural -> (forall (n :: Nat). SNat n -> r) -> r
withSomeSNat n f = case someNatVal n of
  SomeNat (_ :: Proxy n) -> f (SNat @n)

-- | Property: @unpad . pad === id@
prop_unpadPadId :: Property
prop_unpadPadId = property $ do
  nBytesNat <- forAll $ Gen.integral (Range.linear 0 64)
  wordSizeMinusOne <- forAll $ Gen.integral (Range.linear 0 8)

  withSomeSNat nBytesNat $ \(SNat :: SNat nBytes) ->
    withSomeSNat wordSizeMinusOne $ \(wsMinusOne :: SNat wordSizeMinusOne) -> do
      case succSNat wsMinusOne of
        (SNat :: SNat wordSize) -> do
          input <- forAll $ genVec @nBytes genDefinedBitVector

          let
            padded = pad @nBytes @wordSize input
            unpadded = unpad @nBytes padded

          annotateShow input
          annotateShow padded
          annotateShow unpadded

          unpadded === input

-- | Property: @pad . unpad === id@ modulo data inserted by padding.
prop_padUnpadId :: Property
prop_padUnpadId = property $ do
  nBytesNat <- forAll $ Gen.integral (Range.linear 0 64)
  wordSizeMinusOne <- forAll $ Gen.integral (Range.linear 0 8)

  withSomeSNat nBytesNat $ \(SNat :: SNat nBytes) ->
    withSomeSNat wordSizeMinusOne $ \(wsMinusOne :: SNat wordSizeMinusOne) -> do
      case succSNat wsMinusOne of
        (SNat :: SNat wordSize) -> do
          input <- forAll $ genVec (genDefinedBitVector @(wordSize * 8))

          let
            unpadded = unpad @nBytes input
            padded = pad @nBytes unpadded

          annotateShow (toList input)
          annotateShow (toList unpadded)
          annotateShow (toList padded)

          -- XXX: Poor man's way of removing the data with the padding. Ideally
          --      we would generate all zeros for the padding, but I don't see
          --      any easy way of doing that.
          L.dropEnd 1 (toList padded) === L.dropEnd 1 (toList input)

tests :: TestTree
tests =
  testGroup
    "Padding"
    [ testPropertyNamed "prop_unpadPadId" "prop_unpadPadId" prop_unpadPadId
    , testPropertyNamed "prop_padUnpadId" "prop_padUnpadId" prop_padUnpadId
    , test_pad_byte_ordering_4bytes_2word
    , test_unpad_byte_ordering_4bytes_2word
    , test_pad_with_padding
    , test_unpad_removes_padding
    ]
