{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
module Tests.ScatterGather(sgGroup) where
import Clash.Prelude
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

-- Random write / sequential read.
-- For any schedule which length is the same as the memory´s depth, all incoming frames should always appear at the output
-- length schedule = depth memory
data SomeCalendar atLeast where
  SomeCalendar :: (1 <= (n + atLeast)) => SNat n -> [Index (atLeast + n)] -> SomeCalendar atLeast

instance Show (SomeCalendar atLeast) where
  show (SomeCalendar SNat list) = show list

genCalendar :: Gen (SomeCalendar 1)
genCalendar = do
  calendarSize <- Gen.enum 4 100
  case TN.someNatVal calendarSize of
    SomeNat size -> SomeCalendar (snatProxy size) <$> Gen.shuffle [0.. fromIntegral calendarSize]

-- TODO: the frames we generater are currently integrals, we won't get any undefined bits
genFrame :: Gen (Maybe (BitVector 64))
genFrame = Gen.maybe $ Gen.integral Range.linearBounded

genFrameList :: Gen [Maybe (BitVector 64)]
genFrameList = Gen.list (Range.constant 0 100) genFrame

sgGroup :: TestTree
sgGroup = testGroup "Scatter Gather group"
  [ testProperty "GatherSequential - No overwriting implies no lost frames" gSeqNoFrameLoss
  , testProperty "ScatterSequential- No overwriting implies no lost frames" sSeqNoFrameLoss]

gSeqNoFrameLoss :: Property
gSeqNoFrameLoss = property $ do
  someCalendar <- forAll genCalendar
  case someCalendar of
    SomeCalendar size@SNat calendar -> do
      inputFrames <- forAll genFrameList
      let inputFrames' = Nothing:inputFrames P.++ P.replicate (snatToNum size +2) Nothing --Add extra cycles to collect results
      let topEntity (unbundle -> (frameIn, calIn)) = gatherSequential frameIn calIn
      let topEntityInput = P.zip inputFrames' $ cycle calendar
      let simOut = simulateN @System (P.length topEntityInput) topEntity topEntityInput
      footnote . fromString $ showX simOut
      Set.fromList inputFrames' === Set.fromList simOut

sSeqNoFrameLoss :: Property
sSeqNoFrameLoss = property $ do
  someCalendar <- forAll genCalendar
  case someCalendar of
    SomeCalendar size@SNat calendar -> do
      inputFrames <- forAll genFrameList
      let inputFrames' = Nothing:inputFrames P.++ P.replicate (snatToNum size +2) Nothing --Add extra cycles to collect results
      let topEntity (unbundle -> (frameIn, calIn)) = scatterSequential frameIn calIn
      let topEntityInput = P.zip inputFrames' $ cycle calendar
      let simOut = simulateN @System (P.length topEntityInput) topEntity topEntityInput
      footnote . fromString $ showX simOut
      Set.fromList inputFrames' === Set.fromList simOut
