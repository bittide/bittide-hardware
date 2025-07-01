-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Tests.Padding where

import Clash.Prelude hiding (someNatVal, withSomeSNat)

import Clash.Class.BitPackC.Padding (pad, unpad)
import Clash.Hedgehog.Sized.BitVector (genDefinedBitVector)
import Clash.Hedgehog.Sized.Vector (genVec)
import Data.Data (Proxy)
import GHC.TypeNats (someNatVal)
import Hedgehog (Property, annotateShow, forAll, property, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import qualified Data.List.Extra as L
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

type Bytes n = BitVector (n * 8)

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
    ]
