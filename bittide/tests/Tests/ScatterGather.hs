{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Tests.ScatterGather(sgGroup) where
import Clash.Prelude
import Clash.Sized.Internal.BitVector
import Data.String
import qualified GHC.TypeNats as TN
import qualified Prelude as P
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Set as Set
import Hedgehog
import Bittide.ScatterGather
import Test.Tasty
import Test.Tasty.Hedgehog
import Clash.Sized.Vector ( unsafeFromList )

-- Random write / sequential read.
-- For any schedule which length is the same as the memory´s depth, all incoming frames should always appear at the output
-- length schedule = depth memory


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

sSeqNoFrameLoss :: Property
sSeqNoFrameLoss = property $ do
  someCalendar <- forAll genSomeCalendar
  case someCalendar of
    SomeCalendar size@SNat (toList -> calendar) -> do
      inputFrames <- forAll genFrameList
      let inputFrames' = Nothing : inputFrames P.++ P.replicate (2 * snatToNum size + 2) Nothing --Add extra cycles to collect results
      let topEntity (unbundle -> (frameIn, calIn, newMeta)) = maybeIsUndefined <$> scatterEngine newMeta frameIn calIn
      let topEntityInput = P.zip3 inputFrames' (cycle calendar) (cycle $ (==0) <$> [0..P.length calendar - 1])
      let simOut = simulateN @System (P.length topEntityInput) topEntity topEntityInput
      footnote . fromString $ showX simOut
      footnote . fromString $ showX topEntityInput
      Set.fromList inputFrames' === Set.fromList simOut

gSeqNoFrameLoss :: Property
gSeqNoFrameLoss = property $ do
  someCalendar <- forAll genSomeCalendar
  case someCalendar of
    SomeCalendar size@SNat (toList -> calendar) -> do
      inputFrames <- forAll genFrameList
      let inputFrames' = Nothing: inputFrames P.++ P.replicate (2 * snatToNum size + 2) Nothing --Add extra cycles to collect results
      let topEntity (unbundle -> (frameIn, calIn, newMeta)) = maybeIsUndefined <$> gatherEngine newMeta frameIn calIn
      let topEntityInput = P.zip3 inputFrames' (cycle calendar) (cycle $ (==0) <$> [0..P.length calendar - 1])
      let simOut = simulateN @System (P.length topEntityInput) topEntity topEntityInput
      footnote . fromString $ showX simOut
      Set.fromList inputFrames' === Set.fromList simOut

sgSwitchToPECommunication :: Property
sgSwitchToPECommunication = property $ do
  someCalendar <- forAll genSomeCalendar
  inputData <- forAll genFrameList
  case someCalendar of
    SomeCalendar size@SNat calendar -> do
      let inputFrames' = Nothing : inputData P.++ P.replicate (2 * snatToNum size + 2) Nothing --Add extra cycles to collect results
      let topEntity (unbundle -> (frameIn,readAddr)) = maybeIsUndefined . snd <$> withClockResetEnable @System clockGen resetGen enableGen scatterGatherEngine calendar calendar (pure (Nothing,Nothing)) frameIn (pure Nothing) readAddr readAddr
      let topEntityInput = P.zip inputFrames' . cycle $ toList calendar
      let simOut = simulateN (P.length topEntityInput) topEntity topEntityInput
      footnote . fromString $ showX simOut
      Set.fromList inputFrames' === Set.fromList simOut

sgPEToSwitchCommunication :: Property
sgPEToSwitchCommunication = property $ do
  someCalendar <- forAll genSomeCalendar
  inputFrames <- forAll genFrameList
  case someCalendar of
    SomeCalendar size@SNat calendar -> do
      let inputFrames' = Nothing : inputFrames P.++ P.replicate (2 * snatToNum size + 2) Nothing --Add extra cycles to collect results
      let topEntity (unbundle -> (frameIn,writeAddr)) = unNestMaybe . fmap maybeIsUndefined . fst <$> withClockResetEnable @System clockGen resetGen enableGen scatterGatherEngine calendar calendar (pure (Nothing,Nothing)) (pure Nothing) frameIn writeAddr writeAddr
      let topEntityInput = P.zip inputFrames' . cycle $ toList calendar
      let simOut = P.drop 1 $ simulateN (P.length topEntityInput) topEntity topEntityInput
      let zeroFilteredInput = [ if addr == 0 then Nothing else frame | (frame, addr) <- topEntityInput]
      footnote . fromString $ showX simOut
      Set.fromList zeroFilteredInput === Set.fromList simOut
