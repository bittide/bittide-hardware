-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Tests.BitView where

import Clash.Prelude hiding (map)

import Clash.Hedgehog.Sized.BitVector (genDefinedBitVector)
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog
import Tests.Shared (SomeSNat (..), someSNat)

import qualified Bittide.BitView as BitView
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

{- | Property test for 'bitFromBytes'. Generates:
1. Vec n (BitVector msbs) - the upper bits
2. Vec n Bit - the target bit we want to extract
3. Vec n (BitVector lsbs) - the lower bits

Then packs them into bytes and verifies that the getter extracts the original bits correctly.
-}
prop_bitFromBytes_roundtrip :: Property
prop_bitFromBytes_roundtrip = property $ do
  nBytes <- forAll $ Gen.integral (Range.linear 1 10)
  bitPos <- forAll $ Gen.integral (Range.linear 0 7)

  case (someSNat nBytes, someSNat bitPos) of
    (SomeSNat (_sn@SNat :: SNat nBytes), SomeSNat (si@SNat :: SNat bitPos)) ->
      case compareSNat si d7 of
        SNatLE -> do
          fullData <- forAll (genDefinedBitVector @(nBytes * 8))
          testBits <- forAll (genDefinedBitVector @nBytes)

          let
            view = BitView.bitFromBytes @nBytes si
            modifiedData = BitView.set view testBits fullData
            retrievedBits = BitView.get view modifiedData

          retrievedBits === testBits
        _ -> failure

-- | Property test that verifies 'over' correctly applies a function.
prop_bitFromBytes_over :: Property
prop_bitFromBytes_over = property $ do
  nBytes <- forAll $ Gen.integral (Range.linear 1 100)
  bitPos <- forAll $ Gen.integral (Range.linear 0 7)

  case (someSNat nBytes, someSNat bitPos) of
    (SomeSNat (_sn@SNat :: SNat nBytes), SomeSNat (si@SNat :: SNat bitPos)) -> do
      case compareSNat si d7 of
        SNatLE -> do
          original <- forAll (genDefinedBitVector @(nBytes * 8))

          let
            view = BitView.bitFromBytes @nBytes si
            modified = BitView.over view complement original
            extracted = BitView.get view modified
            originalBits = BitView.get view original
            expectedBits = complement originalBits

          extracted === expectedBits
        _ -> failure

{- | Property test for 'aBitFromBytes'. Similar to 'prop_bitFromBytes_roundtrip'
but tests the version that works with types that implement 'BitPack'.
-}
prop_aBitFromBytes_roundtrip :: Property
prop_aBitFromBytes_roundtrip = property $ do
  -- Use a fixed number of bytes (8) to satisfy type constraints
  -- Generate which bit position within each byte
  bitPos <- forAll $ Gen.integral (Range.linear 0 7)

  case someSNat bitPos of
    SomeSNat (si@SNat :: SNat bitPos) -> do
      case compareSNat si d7 of
        SNatLE -> do
          fullData <- forAll (genDefinedBitVector @(8 * 8))
          testValue <- forAll Gen.bool

          let
            view = BitView.aBitFromBytes @Bool @8 si
            modifiedData = BitView.set view testValue fullData
            retrievedValue = BitView.get view modifiedData

          retrievedValue === testValue
        _ -> failure

tests :: TestTree
tests =
  testGroup
    "BitView"
    [ testProperty "bitFromBytes roundtrip" prop_bitFromBytes_roundtrip
    , testProperty "bitFromBytes over" prop_bitFromBytes_over
    , testProperty "aBitFromBytes roundtrip" prop_aBitFromBytes_roundtrip
    ]
