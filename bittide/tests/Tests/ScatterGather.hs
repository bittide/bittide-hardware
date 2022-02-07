{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Tests.ScatterGather(sgGroup) where
import Bittide.ScatterGather
import Clash.Prelude
import Clash.Sized.Internal.BitVector
import Clash.Sized.Vector ( unsafeFromList )
import Data.String
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Data.Set as Set
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Prelude as P

isUndefined :: forall n . KnownNat n => BitVector n -> Bool
isUndefined (BV mask _) = mask == full
 where full = 1 `shiftL` (natToNum @n @Int) - 1

maybeIsUndefined :: (KnownNat n, 1 <= n) => BitVector n -> Maybe (BitVector n)
maybeIsUndefined v | isUndefined v = Nothing
                  | otherwise     = Just v

unNestMaybe :: Maybe (Maybe a) -> Maybe a
unNestMaybe (Just (Just a)) = Just a
unNestMaybe (Just Nothing) = Nothing
unNestMaybe Nothing        = Nothing

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
  [ testProperty "GatherSequential - No overwriting implies no lost frames" gSeqNoFrameLoss
  , testProperty "ScatterSequential - No overwriting implies no lost frames" sSeqNoFrameLoss
  , testProperty "ScatterGather - Switch to PE Communication" sgSwitchToPECommunication
  , testProperty "ScatterGather - PE to Switch Communication" sgPEToSwitchCommunication]

-- | Tests that for a calendar that contains only unique entries,
-- all frames send to the scatter engine will appear at its output.
sSeqNoFrameLoss :: Property
sSeqNoFrameLoss = property $ do
  someCalendar <- forAll genSomeCalendar
  let topEntity (unbundle -> (frameIn, calIn, newMeta)) =
        maybeIsUndefined <$> scatterEngine newMeta frameIn calIn
  case someCalendar of
    SomeCalendar size@SNat (toList -> calendar) -> do
      inputFrames <- forAll genFrameList
      let
        postFramesInput = P.replicate (2 * snatToNum size + 2) Nothing
        inputFrames' = Nothing : inputFrames P.++ postFramesInput
        newMetaSignal = cycle $ (==0) <$> [0..P.length calendar - 1]
        topEntityInput = P.zip3 inputFrames' (cycle calendar) newMetaSignal
        simOut = simulateN @System (P.length topEntityInput) topEntity topEntityInput
      footnote . fromString $ showX simOut
      footnote . fromString $ showX topEntityInput
      Set.fromList inputFrames' === Set.fromList simOut

-- | Tests that for a calendar that contains only unique entries,
-- all frames send to the gather engine will appear at its output.
gSeqNoFrameLoss :: Property
gSeqNoFrameLoss = property $ do
  someCalendar <- forAll genSomeCalendar
  let topEntity (unbundle -> (frameIn, calIn, newMeta)) =
        maybeIsUndefined <$> gatherEngine newMeta frameIn calIn
  case someCalendar of
    SomeCalendar size@SNat (toList -> calendar) -> do
      inputFrames <- forAll genFrameList
      let
        postFramesInput = P.replicate (2 * snatToNum size + 2) Nothing
        inputFrames' = Nothing : inputFrames P.++ postFramesInput
        newMetaSignal = cycle $ (==0) <$> [0..P.length calendar - 1]
        topEntityInput = P.zip3 inputFrames' (cycle calendar) newMetaSignal
        simOut = simulateN @System (P.length topEntityInput) topEntity topEntityInput
      footnote . fromString $ showX simOut
      Set.fromList inputFrames' === Set.fromList simOut

-- | Tests that for a calendar that contains only unique entries,
-- all frames send from the switch side, appear at the PE side output.
sgSwitchToPECommunication :: Property
sgSwitchToPECommunication = property $ do
  someCalendar <- forAll genSomeCalendar
  inputData <- forAll genFrameList
  case someCalendar of
    SomeCalendar size@SNat calendar -> do
      let
        postFramesInput = P.replicate (2 * snatToNum size + 2) Nothing
        inputData' = Nothing : inputData P.++ postFramesInput
        topEntity (unbundle -> (frameIn,readAddr)) = maybeIsUndefined .
          snd <$> withClockResetEnable @System clockGen resetGen enableGen
          scatterGatherEngine calendar calendar (pure (Nothing,Nothing)) frameIn
          (pure Nothing) readAddr readAddr
        topEntityInput = P.zip inputData' . cycle $ toList calendar
        simOut = simulateN (P.length topEntityInput) topEntity topEntityInput
      footnote . fromString $ showX simOut
      Set.fromList inputData' === Set.fromList simOut

-- | Tests that for a calendar that contains only unique entries,
-- all frames send from the PE side to a non zero address, appear at the PE side output.
sgPEToSwitchCommunication :: Property
sgPEToSwitchCommunication = property $ do
  someCalendar <- forAll genSomeCalendar
  inputFrames <- forAll genFrameList
  case someCalendar of
    SomeCalendar size@SNat calendar -> do
      let
        postFramesInput = P.replicate (2 * snatToNum size + 2) Nothing
        inputFrames' = Nothing : inputFrames P.++ postFramesInput
        topEntity (unbundle -> (frameIn,writeAddr)) = unNestMaybe . fmap maybeIsUndefined
          . fst <$> withClockResetEnable @System clockGen resetGen enableGen
          scatterGatherEngine calendar calendar (pure (Nothing,Nothing)) (pure Nothing)
          frameIn writeAddr writeAddr
        topEntityInput = P.zip inputFrames' . cycle $ toList calendar
        simOut = P.drop 1 $ simulateN (P.length topEntityInput) topEntity topEntityInput
        zeroFilteredInput = [ if addr == 0 then Nothing else frame | (frame, addr) <- topEntityInput]
      footnote . fromString $ showX simOut
      Set.fromList zeroFilteredInput === Set.fromList simOut
