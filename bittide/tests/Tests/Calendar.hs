-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Calendar (tests, genCalendarConfig, genValidEntry, unrollCalendar) where

import Clash.Prelude

import Clash.Hedgehog.Sized.Unsigned
import Clash.Hedgehog.Sized.Vector
import Data.Maybe (fromJust)
import Data.String
import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Protocols
import Protocols.Hedgehog (defExpectOptions, eoSampleMax, eoStopAfterEmpty)
import Protocols.MemoryMap
import Protocols.Wishbone
import Test.Tasty
import Test.Tasty.Hedgehog

import Bittide.Calendar
import Bittide.SharedTypes
import Tests.Shared

import qualified Clash.Util.Interpolate as I
import qualified Data.List as L
import qualified Data.Map as Map
import qualified GHC.TypeNats as TN
import qualified Protocols.Wishbone.Standard.Hedgehog as Wb
import qualified Prelude as P

tests :: TestTree
tests =
  testGroup
    "Tests.Calendar"
    [ testPropertyNamed "Metacycle signal generation" "metaCycleIndication" metaCycleIndication
    , testPropertyNamed
        "Wait for metacycle end and read metacycle count"
        "readMetacycleCount"
        readMetacycleCount
    ]

{- | Generates a configuration for 'Bittide.Calendar.calendar', with as first argument
the maximum depth of the stored calendar and as second argument a generator for the
calendar entries.
-}
genCalendarConfig ::
  forall addrW a validityBits.
  ( KnownNat addrW
  , KnownNat (BitSize a)
  , KnownNat validityBits
  , BitPack a
  , NFDataX a
  , Show a
  , ShowX a
  ) =>
  -- | Maximum amount of entries a calendar based on the returned configuration can hold per calendar.
  Natural ->
  -- | Generator for the entries in the shadow calendar and active calendar.
  Gen (ValidEntry a validityBits) ->
  Gen (CalendarConfig addrW a)
genCalendarConfig ms elemGen = do
  dA <- Gen.integral $ Range.linear 1 ms
  dB <- Gen.integral $ Range.linear 1 ms
  case (TN.someNatVal (ms - 2), TN.someNatVal dA, TN.someNatVal dB) of
    ( SomeNat (addSNat d2 . snatProxy -> maxSize)
      , SomeNat (snatProxy -> depthA)
      , SomeNat (snatProxy -> depthB)
      ) -> do
        let
          bsCalEntry = SNat @(BitSize a)
        case ( isInBounds d1 depthA maxSize
             , isInBounds d1 depthB maxSize
             , compareSNat d1 bsCalEntry
             ) of
          (InBounds, InBounds, SNatLE) -> go maxSize depthA depthB
          (a, b, c) ->
            error
              [I.i|
              genCalendarConfig: calEntry constraints not satisfied:

                a: #{a}
                b: #{b}
                c: #{c}

              ...
          |]
 where
  go ::
    forall maxDepth depthA depthB.
    ( LessThan depthA maxDepth
    , LessThan depthB maxDepth
    , 1 <= depthA
    , 1 <= depthB
    , 2 <= maxDepth
    ) =>
    SNat maxDepth ->
    SNat depthA ->
    SNat depthB ->
    Gen (CalendarConfig addrW a)
  go dMax SNat SNat = do
    calActive <- genVec @depthA elemGen
    calShadow <- genVec @depthB elemGen
    return $ CalendarConfig dMax SNat calActive calShadow

genValidEntry :: SNat repetitionBits -> Gen a -> Gen (ValidEntry a repetitionBits)
genValidEntry SNat genA =
  (\veEntry veRepeat -> ValidEntry{veEntry, veRepeat})
    <$> genA
    <*> genUnsigned Range.linearBounded

{- | This test checks if the metacycle signal (which indicates that the last entry of the
active calendar is present at the output), is correctly being generated.
-}
metaCycleIndication :: Property
metaCycleIndication = property $ do
  calA <- forAll $ genVec @5 (genValidEntry d10 $ genDefinedBitVector @32)
  calB <- forAll $ genVec @7 (genValidEntry d10 $ genDefinedBitVector @32)
  simLength <- forAll $ Gen.int $ Range.linear 1 100
  let
    maxCalDepth = d10
    durations = cycle $ fmap (P.length . unrollCalendar) [toList calA, toList calB]
    expected =
      L.take simLength
        $ False
        : (P.concatMap (\d -> P.replicate (fromIntegral d - 1) False <> [True]) durations)
    calCtrl = fromList @_ @System $ P.repeat (CalendarControl Nothing Nothing 0 True)
    topEntity :: Signal System Bool
    topEntity =
      ( withClockResetEnable
          clockGen
          resetGen
          enableGen
          calendar
          maxCalDepth
          calA
          calB
          calCtrl
      ).lastCycle
    simOut = sampleN simLength topEntity
  footnote . fromString $ "Durations:    " <> show durations
  footnote . fromString $ "Control:      " <> show (sampleN simLength calCtrl)
  footnote . fromString $ "Simulation:   " <> show simOut
  footnote . fromString $ "Expected:     " <> show expected
  simOut === expected

{- | The purpose of this test is to check if we can read the metacycle count.
We can generate an arbitrary calendar of a minimum length and perform the following
steps:
n = 0
Multiple iterations:
  assert metacycleCount == n
  Wait for end of metacycle
-}
readMetacycleCount :: Property
readMetacycleCount = property $ do
  let
    entryGen = do
      repetitions <- Gen.integral $ Range.linear 0 10
      pure $ ValidEntry 0 repetitions
  config <- forAll $ genCalendarConfig @30 @(BitVector 8) @32 10 entryGen
  iterations <- forAll $ Gen.integral $ Range.linear 1 4
  let
    deviceName = "calendar"
    dut =
      withBigEndian
        $ withClockResetEnable clockGen resetGen enableGen
        $ circuit
        $ \wb -> do
          _out <- mkCalendarC @30 @(BitVector 8) @System @4 "dut" config -< wb
          idC
    defs = (((getMMAny dut).deviceDefs) Map.! deviceName)
    endOfMetacycleLoc = L.find (\loc -> loc.name.name == "endOfMetacycle") defs.registers
    metacycleCoundLoc = L.find (\loc -> loc.name.name == "metacycleCount") defs.registers
  let
    endOfMetacycleAddress = fromIntegral (fromJust endOfMetacycleLoc).value.address `div` 4
    metacycleCountAddress = fromIntegral (fromJust metacycleCoundLoc).value.address `div` 4
    wbOps = [Wb.Write endOfMetacycleAddress maxBound 0, Wb.Read metacycleCountAddress maxBound]
    gen = pure $ L.concatMap (L.replicate iterations) wbOps

    model req@(Wb.Write addr _ _) resp predictedMetacycleCount
      | addr == endOfMetacycleAddress
      , resp.acknowledge =
          Right (predictedMetacycleCount + 1)
      | otherwise = err predictedMetacycleCount req resp
    model req@(Wb.Read addr _) resp predictedMetacycleCount
      | addr == metacycleCountAddress
      , resp.acknowledge
      , -- Use `>=` instead of `==` since `wishbonePropWithModel` may insert extra
        -- operations that are not visible to us. As such we should see the predicted
        -- metacycle, or a successor of it.
        resp.readData >= predictedMetacycleCount =
          Right resp.readData
      | otherwise = err predictedMetacycleCount req resp

    err state req resp =
      Left
        $ unlines
          [ "readMetacycleCount: Unexpected transaction"
          , "Request: " <> show req
          , "Response: " <> show resp
          , "Expected state: " <> show state
          ]

  footnoteShow endOfMetacycleLoc
  footnoteShow metacycleCoundLoc
  withClockResetEnable @System clockGen resetGen enableGen
    $ Wb.wishbonePropWithModel
      defExpectOptions{eoSampleMax = 100_000, eoStopAfterEmpty = Just 1000}
      model
      (unMemmap dut)
      gen
      0

{- | Unrolls a list of 'ValidEntry's into a list of entries.
This repeats each entry (1 + veRepeat) times.
-}
unrollCalendar :: (KnownNat repetitionBits) => [ValidEntry a repetitionBits] -> [a]
unrollCalendar = L.concatMap (\entry -> L.replicate (fromIntegral entry.veRepeat + 1) entry.veEntry)
