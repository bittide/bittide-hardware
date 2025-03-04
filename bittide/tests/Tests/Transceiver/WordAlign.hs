-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Tests.Transceiver.WordAlign where

import Clash.Prelude hiding (someNatVal, withSomeSNat, words)

import Bittide.SharedTypes (Byte)
import Clash.Hedgehog.Sized.BitVector (genDefinedBitVector)
import Clash.Hedgehog.Sized.Index (genIndex)
import Data.Proxy (Proxy (..))
import GHC.TypeNats (someNatVal)
import Hedgehog
import Numeric (showHex)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import qualified Bittide.Transceiver.WordAlign as WordAlign
import qualified Clash.Explicit.Prelude as E
import qualified Data.Char as Char
import qualified Data.List as L
import qualified Data.List.Extra as L
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

newtype HexByte = HexByte Byte
  deriving (Eq, Ord, Generic)
  deriving newtype (Num, BitPack, NFDataX)

instance Show HexByte where
  show (HexByte b) = "0x" <> L.map Char.toUpper (showHex b "")

withSomeSNat :: Natural -> (forall (n :: Nat). SNat n -> r) -> r
withSomeSNat n f = case someNatVal n of
  SomeNat (_ :: Proxy n) -> f (SNat @n)

word1 :: (HexByte, HexByte, HexByte, HexByte)
word1 = (HexByte 0x0, HexByte 0x1, HexByte 0x2, HexByte 0x3)

word2 :: (HexByte, HexByte, HexByte, HexByte)
word2 = (HexByte 0xA, HexByte 0xB, HexByte 0xC, HexByte 0xD)

case_dealignLsbFirst :: Assertion
case_dealignLsbFirst = do
  (0x0, 0x1, 0x2, 0x3) @=? go 0
  (0xD, 0x0, 0x1, 0x2) @=? go 1
  (0xC, 0xD, 0x0, 0x1) @=? go 2
  (0xB, 0xC, 0xD, 0x0) @=? go 3
 where
  go :: Index 4 -> (HexByte, HexByte, HexByte, HexByte)
  go offset = unpack (WordAlign.dealignLsbFirst offset (pack word1) (pack word2))

case_alignLsbFirst :: Assertion
case_alignLsbFirst = do
  (0xA, 0xB, 0xC, 0xD) @=? go 0
  (0xB, 0xC, 0xD, 0x0) @=? go 1
  (0xC, 0xD, 0x0, 0x1) @=? go 2
  (0xD, 0x0, 0x1, 0x2) @=? go 3
 where
  go :: Index 4 -> (HexByte, HexByte, HexByte, HexByte)
  go offset = unpack (WordAlign.alignLsbFirst offset (pack word1) (pack word2))

case_dealignMsbFirst :: Assertion
case_dealignMsbFirst = do
  (0xA, 0xB, 0xC, 0xD) @=? go 0
  (0x3, 0xA, 0xB, 0xC) @=? go 1
  (0x2, 0x3, 0xA, 0xB) @=? go 2
  (0x1, 0x2, 0x3, 0xA) @=? go 3
 where
  go :: Index 4 -> (HexByte, HexByte, HexByte, HexByte)
  go offset = unpack (WordAlign.dealignMsbFirst offset (pack word1) (pack word2))

case_alignMsbFirst :: Assertion
case_alignMsbFirst = do
  (0x0, 0x1, 0x2, 0x3) @=? go 0
  (0x1, 0x2, 0x3, 0xA) @=? go 1
  (0x2, 0x3, 0xA, 0xB) @=? go 2
  (0x3, 0xA, 0xB, 0xC) @=? go 3
 where
  go :: Index 4 -> (HexByte, HexByte, HexByte, HexByte)
  go offset = unpack (WordAlign.alignMsbFirst offset (pack word1) (pack word2))

prop_alignDealign ::
  (forall n. WordAlign.AlignmentFn n) ->
  (forall n. WordAlign.AlignmentFn n) ->
  Property
prop_alignDealign alignFn dealignFn = property $ do
  nMinusOne <- forAll $ Gen.integral (Range.linear 0 16)
  withSomeSNat nMinusOne $ \(succSNat -> SNat :: SNat n) -> do
    offset <- forAll $ genIndex @n Range.constantBounded
    nCycles <- forAll $ Gen.integral (Range.linear 0 64)
    input <- forAll $ Gen.list (Range.singleton nCycles) (genDefinedBitVector @(8 * n))

    withClock @XilinxSystem clockGen $ do
      let
        inputSignal = E.fromList (input <> L.repeat 0)
        dealigned = WordAlign.aligner dealignFn (pure False) (pure offset) inputSignal
        aligned = WordAlign.aligner alignFn (pure False) (pure offset) dealigned
        output = E.sampleN nCycles aligned

        pipelineCycles = 1
        startUpCycles = 1

        expected = L.dropEnd pipelineCycles (L.drop startUpCycles input)
        actual = L.drop (startUpCycles + pipelineCycles) output

      expected === actual

-- | 'prop_alignDealign' with LSB-first transmission
prop_alignDealignLsb :: Property
prop_alignDealignLsb = prop_alignDealign WordAlign.alignLsbFirst WordAlign.dealignLsbFirst

-- | 'prop_alignDealign' with MSB-first transmission
prop_alignDealignMsb :: Property
prop_alignDealignMsb = prop_alignDealign WordAlign.alignMsbFirst WordAlign.dealignMsbFirst

-- Should fail:
-- prop_alignDealignMsbLsb :: Property
-- prop_alignDealignMsbLsb = prop_alignDealign WordAlign.alignMsbFirst WordAlign.dealignLsbFirst

-- prop_alignDealignLsbMsb :: Property
-- prop_alignDealignLsbMsb = prop_alignDealign WordAlign.alignLsbFirst WordAlign.dealignMsbFirst

-- | 'prop_wordAlignFromMsbs' with LSB-first transmission
prop_wordAlignFromMsbsLsbFirst :: Property
prop_wordAlignFromMsbsLsbFirst = prop_wordAlignFromMsbs WordAlign.alignLsbFirst WordAlign.dealignLsbFirst

-- | 'prop_wordAlignFromMsbs' with MSB-first transmission
prop_wordAlignFromMsbsMsbFirst :: Property
prop_wordAlignFromMsbsMsbFirst = prop_wordAlignFromMsbs WordAlign.alignMsbFirst WordAlign.dealignMsbFirst

{- | Make sure we can recover "user data" after sending alignment bits and freezing
the offset/alignment.
-}
prop_wordAlignFromMsbs ::
  (forall n. WordAlign.AlignmentFn n) ->
  (forall n. WordAlign.AlignmentFn n) ->
  Property
prop_wordAlignFromMsbs alignFn dealignFn = property $ do
  -- Generate type level constructs for worker function
  nBytesMinusOne <- forAll $ Gen.integral (Range.linear 0 16)
  withSomeSNat nBytesMinusOne (go . succSNat)
 where
  -- Worker function that only deals with term level naturals
  go :: forall nBytes. (1 <= nBytes) => SNat nBytes -> PropertyT IO ()
  go SNat = leToPlus @1 @nBytes $ do
    -- How much offset is "inserted" by the "transceiver"s
    byteOffset <- forAll $ genIndex Range.constantBounded

    -- Number of cycles before sending alignment bits
    nPreAlignCycles <- forAll $ Gen.integral (Range.linear 0 16)
    preAlignWords <- forAll $ Gen.list (Range.singleton nPreAlignCycles) genDefinedBitVector

    -- Number of cycles where we transmit words with alignment bits. We will
    -- always generate valid alignments in each test case to simplify the
    -- implemenation. As noted in the documentation of 'WordAlign.aligner', we
    -- generate at least 2 cycles with alignment bits, to ensure proper decoding
    -- of the words thereafter.
    nAlignCycles <- forAll $ Gen.integral (Range.linear 2 16)
    alignWords <-
      fmap (WordAlign.joinMsbs @nBytes @8 WordAlign.alignSymbol)
        <$> forAll (Gen.list (Range.singleton nAlignCycles) genDefinedBitVector)

    -- Number of cycles to produce data with (potentially) invalid alignment
    -- bits. We expect to be able to recover these words, because the aligner
    -- should have used the last valid alignment bits.
    nPostAlignCycles <- forAll $ Gen.integral (Range.linear 0 16)
    postAlignWords <- forAll $ Gen.list (Range.singleton nPostAlignCycles) genDefinedBitVector

    withClock @XilinxSystem clockGen $ do
      let
        nCycles = nPreAlignCycles + nAlignCycles + nPostAlignCycles
        allWords = E.fromList (preAlignWords <> alignWords <> postAlignWords <> L.repeat 0)
        allDealignedWords = WordAlign.aligner dealignFn (pure False) (pure byteOffset) allWords

        pipelineDepth = 1

        freeze =
          L.replicate nPreAlignCycles False
            <> L.replicate nAlignCycles False
            <> L.replicate nPostAlignCycles True

        sampled =
          E.sampleN (nCycles + pipelineDepth)
            $ WordAlign.alignBytesFromMsbs @nBytes alignFn (E.fromList (freeze <> L.repeat True))
            $ allDealignedWords

        actual =
          L.take nPostAlignCycles
            $ L.drop (nPreAlignCycles + nAlignCycles + pipelineDepth)
            $ sampled

      footnote $ "preAlignWords: " <> show preAlignWords
      footnote $ "alignWords: " <> show alignWords
      footnote $ "postAlignWords: " <> show postAlignWords
      footnote $ "sampled: " <> show sampled
      footnote
        $ "allDealignedWords: "
        <> show (sampleN (nCycles + pipelineDepth) allDealignedWords)
      footnote $ "freeze: " <> show freeze

      postAlignWords === actual

tests :: TestTree
tests =
  testGroup
    "WordAlign"
    [ testCase "case_dealignLsbFirst" case_dealignLsbFirst
    , testCase "case_dealignMsbFirst" case_dealignMsbFirst
    , testCase "case_alignLsbFirst" case_alignLsbFirst
    , testCase "case_alignMsbFirst" case_alignMsbFirst
    , testPropertyNamed "prop_alignDealignLsb" "prop_alignDealignLsb" prop_alignDealignLsb
    , testPropertyNamed "prop_alignDealignMsb" "prop_alignDealignMsb" prop_alignDealignMsb
    , testPropertyNamed
        "prop_wordAlignFromMsbsLsbFirst"
        "prop_wordAlignFromMsbsLsbFirst"
        prop_wordAlignFromMsbsLsbFirst
    , testPropertyNamed
        "prop_wordAlignFromMsbsMsbFirst"
        "prop_wordAlignFromMsbsMsbFirst"
        prop_wordAlignFromMsbsMsbFirst
        -- While this works, it also prints the error which is very confusing :-(
        -- , expectFail $ testPropertyNamed "prop_alignDealignMsbLsb" "prop_alignDealignMsbLsb" prop_alignDealignMsbLsb
        -- , expectFail $ testPropertyNamed "prop_alignDealignLsbMsb" "prop_alignDealignLsbMsb" prop_alignDealignLsbMsb
    ]

main :: IO ()
main =
  defaultMain
    $ adjustOption (const (HedgehogTestLimit (Just 1000))) tests
