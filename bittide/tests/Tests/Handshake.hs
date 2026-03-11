-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Tests.Handshake where

import Clash.Prelude
import Hedgehog

import Bittide.Handshake hiding (stickyTrue)
import qualified Data.List as List
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Test.Tasty.Hedgehog (testProperty)
import Tests.Shared (SomeSNat (..), someSNat)

-- Generate a finite list that starts False, switches to True after a random
-- number of cycles in range `[minK, maxK]`, and stays True.
--
-- The list is finite so that hedgehog does not try to print/shrink an infinite
-- data structure.
genStickyTrue :: Int -> Int -> Int -> Gen [Bool]
genStickyTrue minK maxK len = do
  k <- Gen.int (Range.linear minK maxK)
  let l = List.replicate k False List.++ List.replicate (len - k) True
  pure l

-- Generate a finite list that starts False, switches to True for once cycle after
-- a random number of cycles in range `[minK, maxK]`, and then stays False.
--
-- The list is finite so that hedgehog does not try to print/shrink an infinite
-- data structure.
genOneshotTrue :: Int -> Int -> Int -> Gen [Bool]
genOneshotTrue minK maxK len = do
  k <- Gen.int (Range.linear minK maxK)
  let l = List.replicate k False List.++ List.replicate 1 True List.++ List.replicate (len - k - 1) False
  pure l

-- Output a list that is always true as soon as the input list has a True valie.
stickyTrue :: [Bool] -> [Bool]
stickyTrue = List.drop 1 . List.scanl (||) False

-- Create n registers in a row, each with initial value `a`.
registerN ::
  forall n a dom.
  (KnownNat n) =>
  (NFDataX a) =>
  (HiddenClockResetEnable dom) =>
  SNat n ->
  a ->
  Signal dom a ->
  Signal dom a
registerN _ initVal s = register initVal $ last $ iterateI @(n + 1) (register initVal) s

-- Connect two handshake state machines together, with a variable number of cycle delay
-- between the two state machines. The cycle delays need not be symmetrical.
handshakesWithDelays ::
  forall dom m n.
  (HiddenClockResetEnable dom) =>
  (KnownNat m, KnownNat n) =>
  SNat m ->
  SNat n ->
  Signal dom (Bool, Bool) ->
  Signal dom BittideWord ->
  Signal dom (Bool, Bool) ->
  Signal dom BittideWord ->
  ( Signal dom BittideWord
  , Signal dom (Bool, Bool)
  , Signal dom BittideWord
  , Signal dom (Bool, Bool)
  )
handshakesWithDelays delayAtoB delayBtoA regsA wordA regsB wordB = (wordA', regsA', wordB', regsB')
 where
  rst = resetGen @dom
  en = enableGen @dom
  (wordA', wordToB, regsA') = handshake rst en wordToADelayed wordA regsA
  (wordB', wordToA, regsB') = handshake rst en wordToBDelayed wordB regsB

  wordToADelayed = registerN delayBtoA initValue wordToA
  wordToBDelayed = registerN delayAtoB initValue wordToB
  initValue = unpack 0

-- Test the property that no handshake can transition to user data before all enables from both sides are asserted
testEnableSafety :: Property
testEnableSafety = property $ do
  let testLength = 100
  txEnA <- forAll $ genStickyTrue 0 testLength testLength
  rxEnA <- forAll $ genStickyTrue 0 testLength testLength
  txEnB <- forAll $ genStickyTrue 0 testLength testLength
  rxEnB <- forAll $ genStickyTrue 0 testLength testLength

  delayAtoB <- forAll $ Gen.integral (Range.linear 1 19)
  delayBtoA <- forAll $ Gen.integral (Range.linear 1 19)

  let regsA = bundle (fromList txEnA, fromList rxEnA)
      regsB = bundle (fromList txEnB, fromList rxEnB)
      wordA = pure (unpack 0)
      wordB = pure (unpack 0)

      -- Simulate the handshake
      (_outA, lastTxRxA, _outB, lastTxRxB) =
        case (someSNat delayAtoB, someSNat delayBtoA) of
          (SomeSNat (_a@SNat :: SNat delayAtoB), SomeSNat (_b@SNat :: SNat delayBtoA)) ->
            withClockResetEnable clockGen resetGen enableGen
              $ handshakesWithDelays @System (SNat @delayAtoB) (SNat @delayBtoA) regsA wordA regsB wordB

      (lastTxA, lastRxA) = unbundle lastTxRxA
      (lastTxB, lastRxB) = unbundle lastTxRxB

      -- Declare the property that the handshake will NOT assert ANY lastTx/lastRx BEFORE (ALL txEn/rxEn have been asserted TRUE)
      allEnablesAreTrue =
        List.zipWith4
          (\a b c d -> a && b && c && d)
          (stickyTrue txEnA)
          (stickyTrue rxEnA)
          (stickyTrue txEnB)
          (stickyTrue rxEnB)
      anyTransitionToUserData =
        List.zipWith4
          (\a b c d -> a || b || c || d)
          (sample lastTxA)
          (sample lastRxA)
          (sample lastTxB)
          (sample lastRxB)
      myPropertyList =
        List.take (testLength - 10) $ List.zipWith (&&) anyTransitionToUserData (not <$> allEnablesAreTrue)
      propertyHoldsOverWholeList = and (not <$> myPropertyList)

  assert propertyHoldsOverWholeList

-- Test the property that after all enables are asserted, the handshake completes after a bounded number of cycles
testHandshakeLiveness :: Property
testHandshakeLiveness = property $ do
  let testLength = 100
  let enableBy = 20
  txEnA <- forAll $ genStickyTrue 0 enableBy testLength
  rxEnA <- forAll $ genStickyTrue 0 enableBy testLength
  txEnB <- forAll $ genStickyTrue 0 enableBy testLength
  rxEnB <- forAll $ genStickyTrue 0 enableBy testLength

  delayAtoB <- forAll $ Gen.integral (Range.linear 1 19)
  delayBtoA <- forAll $ Gen.integral (Range.linear 1 19)

  let regsA = bundle (fromList txEnA, fromList rxEnA)
      regsB = bundle (fromList txEnB, fromList rxEnB)
      wordA = pure (unpack 0)
      wordB = pure (unpack 0)

      -- Simulate the handshake
      (_outA, lastTxRxA, _outB, lastTxRxB) =
        case (someSNat delayAtoB, someSNat delayBtoA) of
          (SomeSNat (_a@SNat :: SNat delayAtoB), SomeSNat (_b@SNat :: SNat delayBtoA)) ->
            withClockResetEnable clockGen resetGen enableGen
              $ handshakesWithDelays @System (SNat @delayAtoB) (SNat @delayBtoA) regsA wordA regsB wordB

      (lastTxA, lastRxA) = unbundle lastTxRxA
      (lastTxB, lastRxB) = unbundle lastTxRxB

      -- Declare the property that the handshake will assert ALL lastTx/lastRx WITHIN 90 cycles of (ALL txEn/rxEn have been asserted TRUE)
      finiteTest = List.take (testLength - 10)
      countNumAssertTrue = List.sum . List.map fromEnum
      numAssertTrue =
        ( countNumAssertTrue $ finiteTest (sample lastTxA)
        , countNumAssertTrue $ finiteTest (sample lastRxA)
        , countNumAssertTrue $ finiteTest (sample lastTxB)
        , countNumAssertTrue $ finiteTest (sample lastRxB)
        )

  -- Assertions
  numAssertTrue === (1, 1, 1, 1)

testMetadataParsing :: Assertion
testMetadataParsing = do
  let regularWord = unpack 0 :: BittideWord
  let metadataWord = unpack $ magicConstant ++# 0b1110_0000
  let metadata = Meta True True True

  assertEqual "Parsing regular word returns Nothing" Nothing (metadataFromWord regularWord)
  assertEqual "Parsing metadata word returns Just xxx" (Just metadata) (metadataFromWord metadataWord)
  assertEqual "id ~ fromWord . toWord" (Just metadata) (metadataFromWord $ metadataToWord metadata)

tests :: TestTree
tests =
  testGroup
    "Bittide.Handshake"
    [ testCase "testMetadataParsing" testMetadataParsing
    , testProperty "testEnableSafety" testEnableSafety
    , testProperty "testHandshakeLiveness" testHandshakeLiveness
    ]
