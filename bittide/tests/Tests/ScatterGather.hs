{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Tests.ScatterGather(sgGroup) where

import Clash.Prelude
import qualified Prelude as P

import Clash.Sized.Internal.BitVector
import Clash.Sized.Vector (unsafeFromList)
import Data.Bifunctor
import Data.String
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Data.List as L
import qualified Data.Set as Set
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Bittide.ScatterGather

isUndefined :: forall n . KnownNat n => BitVector n -> Bool
isUndefined (BV mask _) = mask == full
 where full = 1 `shiftL` (natToNum @n @Int) - 1

maybeIsUndefined :: (KnownNat n, 1 <= n) => BitVector n -> Maybe (BitVector n)
maybeIsUndefined v  | isUndefined v = Nothing
                    | otherwise     = Just v

-- | The extra in SomeCalendar extra defines the minimum amount of elements in the vector
-- and the minimum addressable indexes in the vector elements. I.e, vectors of 0 elements
-- and Index 0 as element are not allowed.
data SomeCalendar extra where
  SomeCalendar :: (1 <= (extra + n)) => SNat n -> Vec (n + extra) (Index (n + extra)) -> SomeCalendar extra

instance Show (SomeCalendar extra) where
  show (SomeCalendar SNat list) = show list

-- | Returns a calendar for a engine with a memory depth equal to the calendar depth,
-- the addresses in the calendar are all unique.
genSomeCalendar :: Gen (SomeCalendar 1)
genSomeCalendar = do
  calendarSize <- Gen.enum 1 255
  case TN.someNatVal calendarSize of
    (SomeNat size) -> do
      cal <- Gen.shuffle [0.. fromIntegral calendarSize]
      return (SomeCalendar (snatProxy size) $ unsafeFromList cal)

genData :: Gen (BitVector 64)
genData = Gen.integral Range.linearBounded

genFrame :: Gen (Maybe (BitVector 64))
genFrame = Gen.maybe genData

genFrameList :: Gen [Maybe (BitVector 64)]
genFrameList = Gen.list (Range.constant 1 100) genFrame

sgGroup :: TestTree
sgGroup = testGroup "Scatter Gather group"
  [ testPropertyNamed "GatherSequential - No overwriting implies no lost frames." "engineNoFrameLoss gatherEngine" (engineNoFrameLoss gatherEngine)
  , testPropertyNamed "ScatterSequential - No overwriting implies no lost frames." "engineNoFrameLoss scatterEngine" (engineNoFrameLoss scatterEngine)
  , testPropertyNamed "ScatterGather - No overwriting implies no lost frames." "scatterGatherNoFrameLoss" scatterGatherNoFrameLoss]

-- |The type of a sequential engine that is tested by engineNoFrameLoss.
type MemoryEngine =
  forall dom memDepth a .
  (NFDataX a, KnownNat memDepth, 1 <= memDepth, HiddenClockResetEnable dom) =>
  -- | Indicates when a new metacycle has started.
  Signal dom Bool ->
  -- | Incoming frame from link, if it contains Just a, a will be written to the memory.
  Signal dom (Maybe a) ->
  -- | Write address, when the incoming frame contains Just a, a will be written to this address.
  Signal dom (Index memDepth) ->
  -- | Outgoing data
  Signal dom a

-- | Tests that for a calendar that contains only unique entries,
-- all frames send to the gather engine will appear at its output.
-- The amount of frames we test for is equal to twice the size of the buffer, since
-- it is double buffered and +2 to account for reset cycle and the one cycle delay.
engineNoFrameLoss :: MemoryEngine -> Property
engineNoFrameLoss engine = property $ do
  someCalendar <- forAll genSomeCalendar
  case someCalendar of
    SomeCalendar size@SNat (toList -> calendar) -> do
      inputFrames <- forAll genFrameList
      let
        topEntity (unbundle -> (frameIn, calIn, newMeta)) =
          maybeIsUndefined <$> withClockResetEnable clockGen resetGen enableGen
          engine newMeta frameIn calIn
        -- Simulate for at least twice the size of the doublebuffered memory + the
        -- length of the inputdata + 2 to compensate for the reset cycle and delay cycle.
        simLength = 2 * snatToNum size + P.length inputFrames + 2
        inputFrames' = P.take simLength $ inputFrames <> P.repeat Nothing
        newMetaSignal = cycle $ (==0) <$> [0..P.length calendar - 1]
        topEntityInput = P.zip3 inputFrames' (cycle calendar) newMetaSignal
        simOut = simulateN @System simLength topEntity topEntityInput
      footnote . fromString $ showX simOut
      footnote . fromString $ showX topEntityInput
      Set.fromList inputFrames' === Set.fromList simOut

filterSGOut ::
  (KnownDomain dom, KnownNat n, 1 <= n) =>
  (Signal dom (BitVector n), Signal dom (Maybe (BitVector n))) ->
  (Signal dom (Maybe (BitVector n)), Signal dom (Maybe (BitVector n)))
filterSGOut (toP, toS) = (maybeIsUndefined <$> toP, (maybeIsUndefined =<<) <$> toS)

filterZeroes :: (Num a, Eq a) => [Maybe f] -> [a] -> [Maybe f]
filterZeroes fs as = [ if a == 0 then Nothing else f | (f, a) <- P.zip fs as]

-- | Tests that for a calendar that contains only unique entries,
-- all frames send from the PE side to a non zero address, appear at the PE side output.
scatterGatherNoFrameLoss :: Property
scatterGatherNoFrameLoss = property $ do
  someCalendarS <- forAll genSomeCalendar
  someCalendarG <- forAll genSomeCalendar
  inputFramesSwitch <- forAll genFrameList
  inputFramesPE <- forAll genFrameList
  case (someCalendarS, someCalendarG) of
    (SomeCalendar depthScat@SNat calScat, SomeCalendar depthGath@SNat calGath) -> do
      let
        addressesScat = cycle $ toList calScat
        addressesGath = cycle $ toList calGath
        inputFramesSwitch' = inputFramesSwitch <> P.repeat Nothing
        inputFramesPE' = inputFramesPE <> P.repeat Nothing

        topEntity (unbundle -> (frameInS, frameInP, readAddrPE, writeAddrPE)) = bundle $
          filterSGOut @System (withClockResetEnable clockGen resetGen enableGen
          scatterGatherEngine calScat calGath (pure Nothing) (pure Nothing) frameInS frameInP
          readAddrPE writeAddrPE)

        maxCalDepth = max (snatToNum depthScat) (snatToNum depthGath)
        maxInputLength = max (P.length inputFramesSwitch) (P.length inputFramesPE)
        -- Simulate for at least the largest calendar + twice the length of the longest input.
        -- This ensures all frames will appear at the output.
        simLength = maxCalDepth + 2 * maxInputLength
        topEntityInput = L.zip4 inputFramesSwitch' inputFramesPE' addressesScat addressesGath
        simOut = simulateN simLength topEntity topEntityInput

        expectedScat = filterZeroes (P.take simLength inputFramesSwitch') addressesScat
        expectedGath = filterZeroes (P.take simLength inputFramesPE') addressesGath
        expected = bimap Set.fromList Set.fromList (expectedScat, expectedGath)
        result = bimap Set.fromList Set.fromList $ P.unzip simOut

      footnote . fromString $ "Frames to PE: " <> showX (fmap fst simOut)
      footnote . fromString $ "Frames to Switch: " <> showX (fmap snd simOut)
      footnote . fromString $ "Frames from PE: " <> show (P.zip addressesGath $ P.take simLength inputFramesPE')
      footnote . fromString $ "Frames from Switch: " <> show (P.zip addressesScat $ P.take simLength inputFramesSwitch')

      expected === result
