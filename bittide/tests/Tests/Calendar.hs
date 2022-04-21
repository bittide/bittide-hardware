{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Calendar(calGroup) where

import Clash.Prelude
import Clash.Hedgehog.Sized.Vector

import Bittide.Calendar
import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Data.Set as Set
import qualified Prelude as P

calGroup :: TestTree
calGroup = testGroup "Calendar group"
  [ testPropertyNamed "Reading the calendar." "readCalendar" readCalendar
  , testPropertyNamed "Writing and reading new calendars" "reconfigCalendar" reconfigCalendar
  , testPropertyNamed "Metacycle signal generation" "metaCycleIndication" metaCycleIndication]

genIntCalendar :: Int -> Gen (SomeVec 1 Int)
genIntCalendar calSize =
  genSomeVec (Range.singleton $ fromIntegral calSize - 1) $ Gen.int Range.constantBounded

-- | This test checks if we can read the initialized calendars.
readCalendar :: Property
readCalendar = property $ do
  calSize <- forAll $ Gen.enum 1 32
  intCal <- forAll $ genIntCalendar calSize
  simLength <- forAll $ Gen.enum calSize 100
  switchSignal <- forAll $ Gen.list (Range.singleton simLength) Gen.bool
  case intCal of
    SomeVec SNat cal -> do
      let
        topEntity switch = withClockResetEnable clockGen resetGen enableGen $ fst (calendar cal switch (pure Nothing))
        simOut = simulateN @System simLength topEntity switchSignal
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
    SomeVec SNat cal -> do
      let
        switchList = P.drop 2 . cycle $ True : P.replicate (calSize-1) False
        configAddresses = cycle [0.. fromIntegral $ calSize - 1]
        topEntity (unbundle -> (switch, writePort)) = withClockResetEnable clockGen
          resetGen enableGen $ fst (calendar cal switch writePort)
        topEntityInput = P.take simLength $ P.zip switchList (P.zipWith (curry Just) configAddresses newEntries <> P.repeat Nothing)
        simOut = simulateN @System simLength topEntity topEntityInput
      Set.fromList simOut === Set.fromList (P.take simLength (toList cal <> newEntries))

-- | This test checks if the metacycle signal (which indicates that the first entry of a
-- new calendar is present at the output), is correctly being generated.
metaCycleIndication :: Property
metaCycleIndication = property $ do
  calSize <- forAll $ Gen.enum 1 31
  intCal <- forAll $ genIntCalendar calSize
  case intCal of
    SomeVec SNat cal -> do
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
