-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}

module Tests.Transceiver.WordAlign where

import Clash.Prelude hiding (someNatVal)

import Clash.Explicit.Reset (noReset)
import Clash.Hedgehog.Sized.BitVector (genDefinedBitVector)
import Clash.Hedgehog.Sized.Index (genIndex)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy(..))
import GHC.TypeNats (someNatVal)
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit

import qualified Bittide.Transceiver.WordAlign as WordAlign
import qualified Clash.Explicit.Prelude as E
import qualified Data.List as L
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

withSomeSNat :: Natural -> (forall (n :: Nat). SNat n -> r) -> r
withSomeSNat n f = case someNatVal n of
  SomeNat (_ :: Proxy n) -> f (SNat @n)

-- | Very simply sanity check for the 'dealign' function. Is non-exhaustive, but
-- is used extensively in 'prop_wordAlignFromMsb'.
case_dealign :: Assertion
case_dealign = do
  [0xCDEF, 0x0000] @=? WordAlign.dealignBytes (0 :: Index 2) [0xCDEF]
  [0xDEFC, 0x0000] @=? WordAlign.dealignBytes (0 :: Index 2) [0xDEFC]
  [0xEFCD, 0x0000] @=? WordAlign.dealignBytes (0 :: Index 2) [0xEFCD]
  [0xFCDE, 0x0000] @=? WordAlign.dealignBytes (0 :: Index 2) [0xFCDE]

  [0x00CD, 0xEF00] @=? WordAlign.dealignBytes (1 :: Index 2) [0xCDEF]
  [0x00DE, 0xFC00] @=? WordAlign.dealignBytes (1 :: Index 2) [0xDEFC]
  [0x00EF, 0xCD00] @=? WordAlign.dealignBytes (1 :: Index 2) [0xEFCD]
  [0x00FC, 0xDE00] @=? WordAlign.dealignBytes (1 :: Index 2) [0xFCDE]

prop_wordAlignFromMsb :: Property
prop_wordAlignFromMsb = property $ do
  -- Generate type level constructs for worker function
  nBytesMinusOne <- forAll $ Gen.integral (Range.linear 0 15)
  withSomeSNat nBytesMinusOne (go . succSNat)
 where
  -- Worker function that only deals with term level naturals
  go :: forall nBytes . (1 <= nBytes) => SNat nBytes -> PropertyT IO ()
  go SNat = leToPlus @1 @nBytes $ do
    -- How much offset is "inserted" by the "transceiver"s
    byteOffset <- forAll $ genIndex Range.constantBounded

    -- Number of cycles to keep all valid bits deasserted
    nInvalidCycles <- forAll $ Gen.integral (Range.linear 0 64)

    -- Number of cycles where we expect to transfer valid data. Note that we only
    -- set the valid bits once - once detected the align functions should ignore
    -- the valid bits. We always generate at least one valid cycle, to simplify
    -- the test implementation.
    nValidCycles <- forAll $ Gen.integral (Range.linear 1 64)

    -- First value of valid word. Subsequent values are incremented by one.
    startWord <- forAll $ genDefinedBitVector @_ @(7 * nBytes)

    let
      invalidWord = WordAlign.joinMsbs @nBytes @8 0 (errorX "go: invalid")
      validWord n =
        -- All words after the first valid word have their valid bits set
        -- "chaotically". That is, the word written is reduced using XOR to
        -- determine the valid bits. These should be ignored by the align functions.
        WordAlign.joinMsbs @nBytes (toValids (startWord + n)) (startWord + n)

      invalidCycles = L.replicate nInvalidCycles invalidWord
      firstValidCycle = WordAlign.joinMsbs @nBytes WordAlign.alignSymbol startWord
      restOfValidCycles = [validWord n | n <- [1..satPred SatBound nValidCycles]]
      validCycles = firstValidCycle : restOfValidCycles
      cycles = WordAlign.dealignBytes byteOffset (invalidCycles <> validCycles)

      -- How quickly the align function can respond to inputs
      pipelineDepth = 1

      actual =
          L.map (snd . WordAlign.splitMsbs @nBytes @8)
        $ L.take (L.length validCycles)
        $ catMaybes
        $ E.sampleN (L.length cycles + pipelineDepth)
        $ withClockResetEnable @XilinxSystem clockGen noReset enableGen
        $ WordAlign.alignBytesFromMsbs @nBytes
        $ E.fromList $ cycles <> L.repeat 0

      expected = [startWord + n | n <- [0..satPred SatBound nValidCycles]]

    footnote $ "invalidCycles: " <> show invalidCycles
    footnote $ "validCycles: " <> show validCycles
    footnote $ "cycles: " <> show cycles

    L.length expected === fromIntegral nValidCycles -- sanity check
    expected === actual

-- | \"Chaotic\" valid bits, based on the input word.
toValids :: forall n . KnownNat n => BitVector (7 * n) -> BitVector n
toValids = pack . map reduceXor . unpack @(Vec n (BitVector 7))

tests :: TestTree
tests = testGroup "WordAlign"
  [ testPropertyNamed "prop_wordAlignFromMsb" "prop_wordAlignFromMsb" prop_wordAlignFromMsb
  , testCase "case_dealign" case_dealign
  ]
