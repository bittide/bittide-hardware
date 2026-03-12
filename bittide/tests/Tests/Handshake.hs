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

{- | Number of bytes in a BittideWord. Meant to be adjustable based on expected
workload size
-}
type BittideWordSize = 8

{- | To work around Clash's constraint solver, we declare all functions to work on
@n+1@, so we must declare the size to be the actual word size - 1.
-}
type BittideWordSizeAdj = BittideWordSize - 1

{- Generate a finite list that starts False, switches to True after a random
number of cycles in range @[minK, maxK]@, and stays True.

The list is finite so that hedgehog does not try to print/shrink an infinite
data structure.
-}
genStickyTrue :: Int -> Int -> Int -> Gen [Bool]
genStickyTrue minK maxK len = do
  k <- Gen.int (Range.linear minK maxK)
  let l = List.replicate k False List.++ List.replicate (len - k) True
  pure l

{- Generate a finite list that starts False, switches to True for once cycle after
a random number of cycles in range @[minK, maxK]@, and then stays False.

The list is finite so that hedgehog does not try to print/shrink an infinite
data structure.
-}
genOneshotTrue :: Int -> Int -> Int -> Gen [Bool]
genOneshotTrue minK maxK len = do
  k <- Gen.int (Range.linear minK maxK)
  let l = List.replicate k False List.++ List.replicate 1 True List.++ List.replicate (len - k - 1) False
  pure l

-- | Output a list that is always true as soon as the input list has a True value.
stickyTrue :: [Bool] -> [Bool]
stickyTrue = List.drop 1 . List.scanl (||) False

-- | Create @n@ registers in a row, each with initial value @a@.
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

{- Connect two handshake state machines together, with a variable number of cycle delay
between the two state machines. The cycle delays need not be symmetrical.
-}
handshakesWithDelays ::
  forall dom n delayAB delayBA.
  (HiddenClockResetEnable dom) =>
  (KnownNat delayAB, KnownNat delayBA) =>
  (KnownNat n) =>
  SNat delayAB ->
  SNat delayBA ->
  Signal dom (Bool, Bool) ->
  Signal dom (BitVector ((n + 1) * 8)) ->
  Signal dom (Bool, Bool) ->
  Signal dom (BitVector ((n + 1) * 8)) ->
  ( Signal dom (BitVector ((n + 1) * 8))
  , Signal dom (Bool, Bool)
  , Signal dom (BitVector ((n + 1) * 8))
  , Signal dom (Bool, Bool)
  )
handshakesWithDelays delayAtoB delayBtoA regsA wordA regsB wordB = (wordA', regsA', wordB', regsB')
 where
  (wordA', wordToB, regsA') = handshake @n wordToADelayed wordA regsA
  (wordB', wordToA, regsB') = handshake @n wordToBDelayed wordB regsB

  wordToADelayed = registerN delayBtoA initValue wordToA
  wordToBDelayed = registerN delayAtoB initValue wordToB
  initValue = unpack 0

{- A simple state machine that starts by sending dummy data. When it receives a
lastTx to be True, it sends out the @ugn@ value next cycle, then continuously
sends out user data values.
-}
ugnSender ::
  forall dom n.
  (KnownNat n) =>
  (HiddenClockResetEnable dom) =>
  Signal dom Bool ->
  Signal dom (BitVector ((n + 1) * 8))
ugnSender = moore updateState outputUserData initState
 where
  dummyWord = 0x00 :: BitVector ((n + 1) * 8)
  ugnWord = 0xaa :: BitVector ((n + 1) * 8)
  userDataWord = 0xff :: BitVector ((n + 1) * 8)

  initState = (False, False)

  updateState (True, False) _ = (True, True)
  updateState (False, False) isLast = (isLast, False)
  updateState state _ = state

  outputUserData (False, _) = dummyWord -- Output dummy data since it'll be overwritten by metadata words
  outputUserData (True, False) = ugnWord -- Output UGN
  outputUserData (True, True) = userDataWord -- Output user data

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
      wordA = pure 0
      wordB = pure 0

      -- Simulate the handshake
      (_outA, lastTxRxA, _outB, lastTxRxB) =
        case (someSNat delayAtoB, someSNat delayBtoA) of
          (SomeSNat (_a@SNat :: SNat delayAtoB), SomeSNat (_b@SNat :: SNat delayBtoA)) ->
            withClockResetEnable clockGen resetGen enableGen
              $ handshakesWithDelays @System @BittideWordSizeAdj
                (SNat @delayAtoB)
                (SNat @delayBtoA)
                regsA
                wordA
                regsB
                wordB

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
      wordA = pure 0
      wordB = pure 0

      -- Simulate the handshake
      (_outA, lastTxRxA, _outB, lastTxRxB) =
        case (someSNat delayAtoB, someSNat delayBtoA) of
          (SomeSNat (_a@SNat :: SNat delayAtoB), SomeSNat (_b@SNat :: SNat delayBtoA)) ->
            withClockResetEnable clockGen resetGen enableGen
              $ handshakesWithDelays @System @BittideWordSizeAdj
                (SNat @delayAtoB)
                (SNat @delayBtoA)
                regsA
                wordA
                regsB
                wordB

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

-- Test that when @lastRx@ is asserted, the next cycle will be the UGN, therefore ensuring there
-- are not timing mismatches in the handshake signals.
testHandshakeUgnCapture :: Property
testHandshakeUgnCapture = property $ do
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
      wordA = (withClockResetEnable clockGen resetGen enableGen $ register 0 $ ugnSender lastTxA)
      wordB = (withClockResetEnable clockGen resetGen enableGen $ register 0 $ ugnSender lastTxB)

      -- Simulate the handshake
      (outA, lastTxRxA, outB, lastTxRxB) =
        case (someSNat delayAtoB, someSNat delayBtoA) of
          (SomeSNat (_a@SNat :: SNat delayAtoB), SomeSNat (_b@SNat :: SNat delayBtoA)) ->
            withClockResetEnable clockGen resetGen enableGen
              $ handshakesWithDelays @System @BittideWordSizeAdj
                (SNat @delayAtoB)
                (SNat @delayBtoA)
                regsA
                wordA
                regsB
                wordB

      (lastTxA, lastRxA) = unbundle lastTxRxA
      (lastTxB, lastRxB) = unbundle lastTxRxB

      -- Capture how many times the UGN and lastRx are the true on 1) BOTH cycles 2) EITHER cycles
      capturedUgnA =
        (outA .==. pure 0xaa)
          .&&. (withClockResetEnable clockGen resetGen enableGen $ register False lastRxA)
      capturedUgnA' =
        (outA .==. pure 0xaa)
          .||. (withClockResetEnable clockGen resetGen enableGen $ register False lastRxA)
      capturedUgnB =
        (outB .==. pure 0xaa)
          .&&. (withClockResetEnable clockGen resetGen enableGen $ register False lastRxB)
      capturedUgnB' =
        (outB .==. pure 0xaa)
          .||. (withClockResetEnable clockGen resetGen enableGen $ register False lastRxB)

      finiteTest = List.take (testLength - 10)
      countNumAssertTrue = List.sum . List.map fromEnum
      ugnCapturedA = countNumAssertTrue $ finiteTest $ sample capturedUgnA
      ugnCapturedA' = countNumAssertTrue $ finiteTest $ sample capturedUgnA'
      ugnCapturedB = countNumAssertTrue $ finiteTest $ sample capturedUgnB
      ugnCapturedB' = countNumAssertTrue $ finiteTest $ sample capturedUgnB'

  -- Assert that the UGN and lastRx (delayed one cycle) are both only True once,
  -- and that they are True on the same cycle.
  ugnCapturedA === 1
  ugnCapturedA' === 1
  ugnCapturedB === 1
  ugnCapturedB' === 1

testMetadataParsing :: Assertion
testMetadataParsing = do
  let regularWord = unpack 0 :: (BitVector 64)
  let metadataWord = unpack $ magicConstant @BittideWordSizeAdj ++# (0b1110_0000 :: BitVector 8)
  let metadata = Meta True True True 0

  assertEqual "Parsing regular word returns Nothing" Nothing (wordToMetadata regularWord)
  assertEqual "Parsing metadata word returns Just xxx" (Just metadata) (wordToMetadata metadataWord)
  assertEqual
    "id ~ fromWord . toWord"
    (Just metadata)
    (wordToMetadata $ metadataToWord @BittideWordSizeAdj metadata)

tests :: TestTree
tests =
  testGroup
    "Bittide.Handshake"
    [ testCase "testMetadataParsing" testMetadataParsing
    , testProperty "testEnableSafety" testEnableSafety
    , testProperty "testHandshakeUgnCapture" testHandshakeUgnCapture
    , testProperty "testHandshakeLiveness" testHandshakeLiveness
    ]
