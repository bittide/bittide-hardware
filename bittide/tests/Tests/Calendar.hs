{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
{-# LANGUAGE GADTs #-}
module Tests.Calendar(calGroup) where
import Clash.Prelude
import Bittide.Calendar
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog.Gen as Gen
import Clash.Sized.Vector ( unsafeFromList )
import Hedgehog.Range as Range
import qualified GHC.TypeNats as TN
import qualified Prelude as P
import qualified Data.Set as Set


-- | The Intcalendar is a vector with a minimum size of 1 elements containing integers.
-- This data type enables us to safisfy the 1 <= size constraints imposed by the topEntities.
data IntCalendar extra where
  IntCalendar :: (1 <= (extra + n)) => SNat n -> Vec (n + extra) Int -> IntCalendar extra

instance Show (IntCalendar extra) where
  show (IntCalendar SNat c) = show c

calGroup :: TestTree
calGroup = testGroup "Calendar group"
  [ testProperty "Reading the calendar." readCalendar
  , testProperty "Writing and reading new calendars" reconfigCalendar
  , testProperty "Metacycle signal generation" metaCycleIndication]

genIntCalendar :: Int -> Gen (IntCalendar 1)
genIntCalendar calendarSize = do
  case TN.someNatVal (fromIntegral $ calendarSize - 1) of
    (SomeNat size) -> do
      cal <- Gen.list (Range.singleton $ fromIntegral calendarSize) $ Gen.int Range.constantBounded
      return (IntCalendar (snatProxy size) $ unsafeFromList cal)

-- | This test checks if we can read the initialized calendars.
readCalendar :: Property
readCalendar = property $ do
  calSize <- forAll $ Gen.enum 1 32
  intCal <- forAll $ genIntCalendar calSize
  simLength <- forAll $ Gen.enum calSize 100
  switchSignal <- forAll $ Gen.list (Range.singleton simLength) Gen.bool
  case intCal of
    IntCalendar SNat cal -> do
      let
        topEntity switch = withClockResetEnable clockGen resetGen enableGen $ fst (calendar cal switch (pure Nothing))
        simOut = simulateN @System (fromIntegral simLength) topEntity switchSignal
      Set.fromList simOut === Set.fromList (toList cal)

-- | This test checks if we can write to the shadowbuffer and read back the written
-- elements later.
reconfigCalendar :: Property
reconfigCalendar = property $ do
  calSize <- forAll $ Gen.enum 1 32
  intCal <- forAll $ genIntCalendar calSize
  let minSimLength = 2 * calSize
  simLength <- forAll $ Gen.enum minSimLength 100
  let
    newEntriesRead = simLength - minSimLength
    newEntriesRange = Range.singleton newEntriesRead
  newEntries <- forAll . Gen.list newEntriesRange $ Gen.int Range.constantBounded
  case intCal of
    (IntCalendar SNat cal) -> do
      let
        switchList = P.drop 2 . cycle $ True : P.replicate (calSize-1) False
        configAddresses = fmap fromIntegral $ cycle [0..(calSize - 1)]
        topEntity (unbundle -> (switch, writePort)) = withClockResetEnable clockGen
          resetGen enableGen $ fst (calendar cal switch writePort)
        topEntityInput = P.take simLength $ P.zip switchList (P.zipWith (curry Just) configAddresses newEntries <> P.repeat Nothing)
        simOut = simulateN @System simLength topEntity topEntityInput
      Set.fromList simOut === Set.fromList (P.take (fromIntegral simLength) (toList cal <> newEntries))

-- | This test checks if the metacycle signal (which indicates that the first entry of a
-- new calendar is present at the output), is correctly being generated.
metaCycleIndication :: Property
metaCycleIndication = property $ do
  calSize <- forAll $ Gen.enum 1 31
  intCal <- forAll $ genIntCalendar calSize
  case intCal of
    IntCalendar SNat cal -> do
      simLength <- forAll $ Gen.enum 1 100
      let
        topEntity (unbundle -> (switch, writePort)) = withClockResetEnable clockGen
          resetGen enableGen $ snd (calendar cal switch writePort)
        writeGen = Gen.maybe $ (,) <$> Gen.enum 0 (fromIntegral $ calSize - 1) <*> Gen.int Range.constantBounded
      topEntityInput <- forAll . Gen.list (Range.singleton simLength) $ (,) <$> Gen.bool <*> writeGen
      let
        simOut = simulateN @System simLength topEntity topEntityInput
        expectedOut = P.take simLength $ False : cycle (P.replicate (calSize - 1) False <> [True])
      simOut === expectedOut
