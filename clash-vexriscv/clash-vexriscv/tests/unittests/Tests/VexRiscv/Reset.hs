-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.VexRiscv.Reset where

import Clash.Explicit.Prelude hiding (or, unsafeOrReset)
import Hedgehog ((===))
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH
import VexRiscv.Reset as MinReset

import qualified Clash.Explicit.Prelude as CE
import qualified Data.List as L
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Test domain with a known period and synchronous reset
createDomain vSystem{vName = "TestDom", vResetKind = Synchronous}

testClk :: Clock TestDom
testClk = clockGen @TestDom

{- | Generator for a random reset signal that is randomly asserted or deasserted
during the first 50 cycles. After that the reset will be deasserted.
-}
genReset :: forall dom. (KnownDomain dom) => H.Gen (Reset dom)
genReset = do
  resetValues <- Gen.list (Range.linear 1 50) Gen.bool
  pure $ unsafeFromActiveHigh $ fromList (resetValues <> L.repeat False)

-- | `MinCyclesReset` version of `genReset`
genMinCyclesReset :: forall dom n. (KnownDomain dom, KnownNat n) => SNat n -> H.Gen (MinCyclesReset dom n)
genMinCyclesReset snat = do
  resetValues <- genReset
  let extendedReset = holdReset clockGen enableGen snat resetValues
  pure $ unsafeToMinCycles @n extendedReset

{- | Check if a list of boolean samples represents a reset where ALL assertion sequences
are at least n consecutive cycles. Returns False if any assertion sequence is shorter than n cycles.
-}
isResetAtLeastN :: Int -> [Bool] -> Bool
isResetAtLeastN n samples = not (null durations) && all (>= n) durations
 where
  durations = getResetDurations samples

-- | Extract the lengths of all consecutive True sequences from a list of booleans
getResetDurations :: [Bool] -> [Int]
getResetDurations samples = L.reverse $ fst $ L.foldl' go ([], 0) samples
 where
  go (acc, 0) False = (acc, 0)
  go (acc, current) False = (current : acc, 0)
  go (acc, current) True = (acc, current + 1)

-- | Sample a reset signal to a finite list of booleans
sampleReset :: (KnownDomain dom) => Reset dom -> [Bool]
sampleReset rst = sampleN 100 (unsafeToActiveHigh rst)

-- | Helper to use forAll with Reset, showing only the asserted portion of the sampled reset signal
forAllReset ::
  forall dom m.
  (Monad m, KnownDomain dom) =>
  H.Gen (Reset dom) ->
  H.PropertyT m (Reset dom)
forAllReset gen = H.forAllWith showReset gen
 where
  showReset rst = "Reset" <> show (L.reverse $ L.dropWhile not $ L.reverse $ sampleReset rst)

-- | Helper to use forAll with MinCyclesReset, showing only the asserted portion of the sampled reset signal
forAllMinReset ::
  forall dom n m.
  (Monad m, KnownDomain dom, KnownNat n) =>
  H.Gen (MinCyclesReset dom n) ->
  H.PropertyT m (MinCyclesReset dom n)
forAllMinReset gen = H.forAllWith showMinReset gen
 where
  showMinReset minRst =
    let rst = fromMinCycles minRst
     in "MinCyclesReset" <> show (L.reverse $ L.dropWhile not $ L.reverse $ sampleReset rst)

-- ** Properties for MinCyclesReset functions **

-- | Property: genMinCyclesReset should generate resets where all assertion sequences are at least n cycles
prop_genMinCyclesReset_valid :: H.Property
prop_genMinCyclesReset_valid = H.property $ do
  minReset <- forAllMinReset (genMinCyclesReset @TestDom d5)
  let samples = sampleReset $ fromMinCycles minReset
  -- All assertion sequences should be at least 5 cycles
  H.footnote $ "Reset durations: " <> show (getResetDurations samples)
  H.assert $ isResetAtLeastN 5 samples

-- | Property: Coercing a single-cycle reset to n cycles should extend all assertions to exactly n cycles
prop_convert_exactness :: H.Property
prop_convert_exactness = H.property $ do
  let reset1 = unsafeFromActiveHigh $ riseEvery @TestDom testClk resetGen enableGen d10
      minReset7 = convert @1 @7 testClk (toMinCycles testClk reset1)
      samples = sampleReset $ fromMinCycles minReset7
      durations = getResetDurations samples

  H.footnote $ "Reset durations: " <> show durations
  H.assert $ all (== 7) durations

-- | Property: fromMinCycles . toMinCycles should preserve the reset signal
prop_roundtrip_to_from :: H.Property
prop_roundtrip_to_from = H.property $ do
  reset0 <- forAllReset (genReset @TestDom)
  let reset1 = fromMinCycles $ toMinCycles @1 testClk reset0
      samples0 = sampleReset reset0
      samples1 = sampleReset reset1

  samples1 === samples0

-- | Property: Coercing from lower to higher minimum cycles should extend all assertion sequences
prop_convert_extends_reset :: H.Property
prop_convert_extends_reset = H.property $ do
  minReset2 <- forAllMinReset (genMinCyclesReset @TestDom d2)
  let minReset5 = convert @2 @5 testClk minReset2
      reset5 = fromMinCycles minReset5
      samples = sampleReset reset5

  -- All assertion sequences should be at least 5 cycles
  H.assert $ isResetAtLeastN 5 samples

-- | Property: Coercing twice should extend all assertions to the final target cycle count
prop_convert_extends_exactness = H.property $ do
  let reset1 = CE.orReset resetGen $ unsafeFromActiveHigh $ riseEvery @TestDom testClk resetGen enableGen d10
      minReset3 = convert @1 @3 testClk (toMinCycles testClk reset1)
      minReset6 = convert @3 @6 testClk minReset3
      samples = sampleReset $ fromMinCycles minReset6
      durations = getResetDurations samples

  H.footnote $ "Reset1" <> show (L.take 20 $ sampleReset reset1)
  H.footnote $ "Reset3" <> show (L.take 20 $ sampleReset $ fromMinCycles minReset3)
  H.footnote $ "Reset6" <> show (L.take 20 $ sampleReset $ fromMinCycles minReset6)
  H.footnote $ "Reset durations: " <> show durations
  H.assert $ all (== 6) $ L.drop 1 durations -- Ignore startup behavior

-- | Property: Coercing from higher to lower minimum cycles should not shorten assertion sequences
prop_convert_preserves_longer_reset :: H.Property
prop_convert_preserves_longer_reset = H.property $ do
  minReset10 <- forAllMinReset (genMinCyclesReset @TestDom d10)
  let minReset5 = convert @10 @5 testClk minReset10
      reset5 = fromMinCycles minReset5
      samples = sampleReset reset5

  -- All assertion sequences should still be at least 10 cycles (not shortened)
  H.assert $ isResetAtLeastN 10 samples

-- | Property: Coercing to the same minimum cycles should be identity
prop_convert_identity :: H.Property
prop_convert_identity = H.property $ do
  minReset <- forAllMinReset (genMinCyclesReset @TestDom d5)
  let convertd = convert @5 @5 testClk minReset
      samples0 = sampleReset $ fromMinCycles minReset
      samples1 = sampleReset $ fromMinCycles convertd

  samples0 === samples1

-- | Property: extend should extend all assertion sequences to the target cycle count
prop_extend_extends :: H.Property
prop_extend_extends = H.property $ do
  minReset2 <- forAllMinReset (genMinCyclesReset @TestDom @2 d2)
  let minReset8 = MinReset.extend @2 @8 testClk minReset2
      samples = sampleReset $ fromMinCycles minReset8

  -- All assertion sequences should be at least 8 cycles
  H.assert $ isResetAtLeastN 8 samples

-- | Property: fromExtended should preserve the underlying reset signal unchanged
prop_fromExtended_preserves :: H.Property
prop_fromExtended_preserves = H.property $ do
  minReset10 <- forAllMinReset (genMinCyclesReset @TestDom d10)
  let minReset5 = fromExtended @5 minReset10
      samples5 = sampleReset $ fromMinCycles minReset5
      samples10 = sampleReset $ fromMinCycles minReset10

  -- Both should have the same underlying signal
  samples5 === samples10

-- | Property: or should combine two resets using OR logic
prop_or_combines :: H.Property
prop_or_combines = H.property $ do
  minReset5 <- forAllMinReset (genMinCyclesReset @TestDom d5)
  minReset3 <- forAllMinReset (genMinCyclesReset d3)
  let combined = or testClk minReset5 minReset3
      samples5 = sampleReset $ fromMinCycles minReset5
      samples3 = sampleReset $ fromMinCycles $ convert @3 @5 testClk minReset3
      samplesOr = sampleReset $ fromMinCycles combined

  samplesOr === L.zipWith (||) samples5 samples3

-- | Property: or with noReset should preserve the active reset
prop_or_left_identity :: H.Property
prop_or_left_identity = H.property $ do
  minReset5 <- forAllMinReset (genMinCyclesReset @TestDom d5)
  let combined = or testClk minReset5 (unsafeToMinCycles @3 $ CE.noReset)
      samples = sampleReset $ fromMinCycles minReset5
      samplesOr = sampleReset $ fromMinCycles combined

  samplesOr === samples

-- | Property: or with noReset on the left should preserve the active reset
prop_or_right_identity :: H.Property
prop_or_right_identity = H.property $ do
  minReset5 <- forAllMinReset (genMinCyclesReset @TestDom d5)
  let combined = or testClk (unsafeToMinCycles @3 $ CE.noReset) minReset5
      samples = sampleReset $ fromMinCycles minReset5
      samplesOr = sampleReset $ fromMinCycles combined

  samplesOr === samples

tests :: TestTree
tests = $(testGroupGenerator)
