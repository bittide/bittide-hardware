{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module Tests.ScatterGather(sgGroup, filterSGOut, filterZeroes) where
import Bittide.ScatterGather
import Clash.Prelude
import Clash.Sized.Internal.BitVector
import Clash.Sized.Vector ( unsafeFromList )
import Data.Bifunctor
import Data.String
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Data.Set as Set
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Prelude as P
import qualified Data.List as L

isUndefined :: forall n . KnownNat n => BitVector n -> Bool
isUndefined (BV mask _) = mask == full
 where full = 1 `shiftL` (natToNum @n @Int) - 1

maybeIsUndefined :: (KnownNat n, 1 <= n) => BitVector n -> Maybe (BitVector n)
maybeIsUndefined v  | isUndefined v = Nothing
                    | otherwise     = Just v

unNestMaybe :: Maybe (Maybe a)  -> Maybe a
unNestMaybe (Just (Just a)) = Just a
unNestMaybe (Just Nothing)  = Nothing
unNestMaybe Nothing         = Nothing

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

-- TODO: the frames we generater are currently integrals, we won't get any undefined bits
genData :: Gen (BitVector 64)
genData = Gen.integral Range.linearBounded

genFrame :: Gen (Maybe (BitVector 64))
genFrame = Gen.maybe genData

genFrameList :: Gen [Maybe (BitVector 64)]
genFrameList = Gen.list (Range.constant 1 100) genFrame

sgGroup :: TestTree
sgGroup = testGroup "Scatter Gather group"
  [ testProperty "GatherSequential - No overwriting implies no lost frames." (engineNoFrameLoss gatherEngine)
  , testProperty "ScatterSequential - No overwriting implies no lost frames." (engineNoFrameLoss scatterEngine)
  , testProperty "ScatterGather - No overwriting implies no lost frames." scatterGatherNoFrameLoss]

-- | Tests that for a calendar that contains only unique entries,
-- all frames send to the scatter engine will appear at its output.

type MemoryEngine =
  forall dom memDepth a .
  (NFDataX a, KnownNat memDepth, 1 <= memDepth, HiddenClockResetEnable dom) =>
  -- | Boolean signal indicating when a new metacycle has started.
  Signal dom Bool ->
  -- | Incoming frame from link, if it contains Just a, a will be written to the memory.
  Signal dom (Maybe a) ->
  -- | Write address, when the incoming frame contains Just a, a will be written to this address.
  Signal dom (Index memDepth) ->
  -- | Outgoing data
  Signal dom a

-- | Tests that for a calendar that contains only unique entries,
-- all frames send to the gather engine will appear at its output.
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
        postFramesInput = P.replicate (2 * snatToNum size + 2) Nothing
        inputFrames' = Nothing : inputFrames P.++ postFramesInput
        newMetaSignal = cycle $ (==0) <$> [0..P.length calendar - 1]
        topEntityInput = P.zip3 inputFrames' (cycle calendar) newMetaSignal
        simOut = simulateN @System (P.length topEntityInput) topEntity topEntityInput
      footnote . fromString $ showX simOut
      footnote . fromString $ showX topEntityInput
      Set.fromList inputFrames' === Set.fromList simOut

tupMap :: (a -> b) -> (a,a) -> (b,b)
tupMap f (a, b) = bimap f f (a,b)


filterSGOut :: (KnownNat n, 1 <= n) => (BitVector n, Maybe (BitVector n)) -> (Maybe (BitVector n), Maybe (BitVector n))
filterSGOut (toP, toS) = (maybeIsUndefined toP, unNestMaybe $ fmap maybeIsUndefined toS)
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
        postFrames d = P.replicate (2 * snatToNum d + 2) Nothing <> P.repeat Nothing
        inputFramesSwitch' = inputFramesSwitch <> postFrames depthScat
        inputFramesPE' = inputFramesPE P.++ postFrames depthGath

        topEntity (unbundle -> (frameInS, frameInP, readAddrPE, writeAddrPE)) =
          filterSGOut <$> withClockResetEnable @System clockGen resetGen enableGen
          scatterGatherEngine calScat calGath (pure (Nothing,Nothing)) frameInS frameInP
          readAddrPE writeAddrPE

        maxCalDepth = max (snatToNum depthScat) (snatToNum depthGath)
        simLength = 2 + maxCalDepth + max (P.length inputFramesSwitch) (P.length inputFramesPE)
        topEntityInput = L.zip4 inputFramesSwitch' inputFramesPE' addressesScat addressesGath
        simOut = simulateN simLength topEntity topEntityInput

        expectedScat = filterZeroes (P.take simLength inputFramesSwitch') addressesScat
        expectedGath = filterZeroes (P.take simLength inputFramesPE') addressesGath
        expected = tupMap Set.fromList (expectedScat, expectedGath)
        result = tupMap Set.fromList $ P.unzip simOut

      footnote . fromString $ "Frames to PE: " <> showX (fmap fst simOut)
      footnote . fromString $ "Frames to Switch: " <> showX (fmap snd simOut)
      footnote . fromString $ "Frames from PE: " <> show (P.zip addressesGath $ P.take simLength inputFramesPE')
      footnote . fromString $ "Frames from Switch: " <> show (P.zip addressesScat $ P.take simLength inputFramesSwitch')

      expected === result
