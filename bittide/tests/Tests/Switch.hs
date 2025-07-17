-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- Don't warn about partial functions: this is a test, so we'll see it fail.
{-# OPTIONS_GHC -Wno-x-partial #-}

module Tests.Switch (tests, zipList, unzipList) where

import Clash.Prelude

import Clash.Hedgehog.Sized.Index
import Clash.Hedgehog.Sized.Vector
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

import Bittide.Calendar
import Bittide.Switch
import Tests.Calendar hiding (tests)
import Tests.Shared

import Data.Maybe
import Data.Proxy
import Protocols
import Protocols.Idle
import Protocols.MemoryMap (ignoreMM)

import qualified Data.List as L
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

tests :: TestTree
tests =
  testGroup
    "Tests.Switch"
    [ testPropertyNamed "Switch Identity" "switchIdentity" switchIdentity
    , testPropertyNamed "Switch Multicast" "switchMulticast" switchMulticast
    , testPropertyNamed "Switch Null Frames" "switchNullFrames" switchNullFrames
    , testPropertyNamed "Zip Unzip Identity" "zipUnzipIdentity" zipUnzipIdentity
    , testPropertyNamed "Switch Routing" "switchRouting" switchRouting
    ]

data SwitchTestConfig nBytes addrW where
  SwitchTestConfig ::
    (KnownNat links, 1 <= nBytes) =>
    CalendarConfig addrW (CalendarEntry links) ->
    SwitchTestConfig nBytes addrW

deriving instance Show (SwitchTestConfig nBytes addrW)

-- This generator can generate a calendar entry for a switch given the amount of links.
genSwitchEntry ::
  forall links.
  SNat links ->
  Gen (ValidEntry (CalendarEntry links) 0)
genSwitchEntry SNat = genValidEntry SNat (genVec (genIndex Range.linearBounded))

{- | This generator can generate a calendar for the bittide switch, knowing the
amount of bytes and address width of the wishbone bus, and given the amount of links and
calendar depth of the switch.
-}
genSwitchCalendar ::
  forall nBytes addrW.
  (KnownNat nBytes, 1 <= nBytes, KnownNat addrW) =>
  Natural ->
  Natural ->
  Gen (SwitchTestConfig nBytes addrW)
genSwitchCalendar links calDepth = do
  case TN.someNatVal links of
    (SomeNat (snatProxy -> l)) -> do
      testCal <- genCalendarConfig calDepth $ genSwitchEntry l
      return $ SwitchTestConfig testCal

{- | An infinite list to be packed and used as link data.
Each entry contains a vector of indices and a number that increments for each entry,
this can be used to identify links and cycles in the simulation.
-}
linkData :: (KnownNat links) => [Vec links (Index (links + 1), Unsigned 32)]
linkData = [fmap (,i) vecIndices | i <- [0 ..]]
 where
  vecIndices = fmap (succ . bitCoerce . resize) indicesI

simSwitchWithCalendar ::
  forall links nBytes addrW.
  (KnownNat links, KnownNat nBytes, KnownNat addrW, 1 <= nBytes) =>
  SNat nBytes ->
  CalendarConfig addrW (CalendarEntry links) ->
  [Vec links (Index (links + 1), Unsigned 32)] ->
  [Vec links (Index (links + 1), Unsigned 32)]
simSwitchWithCalendar SNat calConfig inp = fmap (fmap unpack) actual
 where
  -- Configure the design under test
  dut = withClockResetEnable @System clockGen resetGen enableGen $ circuit $ \linksIn -> do
    wbIn <- idleSource
    mm <- ignoreMM
    (linksOut, _cal) <-
      switchC @System @nBytes @addrW @links calConfig -< (mm, (linksIn, wbIn))
    idC -< linksOut

  actual =
    zipList
      $ sampleC def{resetCycles = 0} -- We set the reset elsewhere so we don't use it here
      $ dut
      <| driveC def{resetCycles = 0} (unzipList $ fmap (fmap pack) inp)

-- | Test the switch with a calendar that selects the same link for all outputs
switchMulticast :: Property
switchMulticast = property $ do
  simDuration <- forAll $ Gen.int (Range.linear 1 100)
  links <- forAll $ Gen.integral (Range.linear 1 64)
  case TN.someNatVal (links - 1) of
    (SomeNat (addSNat d1 . snatProxy -> _ :: SNat links)) -> do
      let
        inp = linkData @links

        -- A calendar that selects the same link for all outputs
        calSize = SNat @(links + 1)
        cal = (\i -> ValidEntry (repeat i) (0 :: Unsigned 0)) <$> iterate calSize succ 0
        calConfig = CalendarConfig calSize SNat cal cal

        -- Account for reset cycle. Add null frame to the links.
        inpWithNull = repeat (unpack 0) : ((unpack 0 :>) <$> inp)
        outs = L.zipWith (\v i -> repeat $ v !! i) inpWithNull (0 : cycle [0 .. links])

        -- Account for two cycles latency and single reset cycle
        expected = L.take simDuration $ L.replicate 2 (repeat $ unpack 0) <> L.drop 1 outs

        actual = L.take simDuration $ simSwitchWithCalendar @_ @4 @32 SNat calConfig inp
      footnote $ "Calendar: " <> show cal

      actual === expected

-- | Test the switch with a calendar that only selects 0 for all links
switchNullFrames :: Property
switchNullFrames = property $ do
  simDuration <- forAll $ Gen.int (Range.linear 1 100)
  links <- forAll $ Gen.integral (Range.linear 1 64)
  case TN.someNatVal (links - 1) of
    (SomeNat (addSNat d1 . snatProxy -> _ :: SNat links)) -> do
      let
        inp = L.take simDuration (linkData @links)

        -- A calender that only selects 0 for all links
        cal = ValidEntry (repeat 0) (0 :: Unsigned 0) :> Nil
        calConfig = CalendarConfig d2 SNat cal cal

        -- Account for two cycles latency and single reset cycle
        expected = L.replicate simDuration (repeat $ unpack 0)

        actual = L.take simDuration $ simSwitchWithCalendar @_ @4 @32 SNat calConfig inp
      footnote $ "Calendar: " <> show cal

      actual === expected

switchIdentity :: Property
switchIdentity = property $ do
  simDuration <- forAll $ Gen.int (Range.linear 1 100)
  links <- forAll $ Gen.integral (Range.linear 1 64)
  case TN.someNatVal (links - 1) of
    (SomeNat (addSNat d1 . snatProxy -> _ :: SNat links)) -> do
      let
        inp = L.take simDuration (linkData @links)

        -- A calendar that selects the matching input for each output
        cal = ValidEntry (iterateI succ 1) (0 :: Unsigned 0) :> Nil
        calConfig = CalendarConfig d2 SNat cal cal

        -- Account for two cycles latency and single reset cycle
        expected = L.take simDuration $ L.replicate 3 (repeat $ unpack 0) <> L.drop 1 inp

        actual = L.take simDuration $ simSwitchWithCalendar @_ @4 @32 SNat calConfig inp
      footnote $ "Calendar: " <> show cal

      actual === expected

switchModel ::
  (BitPack b, Enum a, KnownNat n) =>
  [Vec n a] ->
  [Vec n b] ->
  [Vec n b]
switchModel cal inp = outStage
 where
  inputStage = L.replicate 2 (repeat (unpack 0)) <> ((unpack 0 :>) <$> L.drop 1 inp)
  midStage = L.zipWith (\v is -> (v !!) <$> is) inputStage (L.head cal : cycle cal)
  outStage = repeat (unpack 0) : midStage

switchRouting :: Property
switchRouting = property $ do
  simDuration <- forAll $ Gen.int (Range.linear 1 100)
  links <- forAll $ Gen.integral (Range.linear 1 64)
  someCalConfig <- forAll $ genSwitchCalendar @4 @32 links 2

  case someCalConfig of
    (SwitchTestConfig (calConfig :: CalendarConfig addrw (CalendarEntry links))) -> do
      let
        inp = linkData @links
        (CalendarConfig _ SNat (unrollCalendar . toList -> cal) _) = calConfig

        -- One cycle output latency
        expected = L.take simDuration $ switchModel cal inp

        actual = L.take simDuration $ simSwitchWithCalendar d4 calConfig inp
      footnote $ "Calendar: " <> show cal
      actual === expected

-- | Test whether (unzipList . zipList) and (zipList . unzipList) are identity functions
zipUnzipIdentity :: Property
zipUnzipIdentity = property $ do
  vecLen <- forAll $ Gen.integral (Range.linear 1 10)
  listLen <- forAll $ Gen.integral (Range.linear 0 10)
  fw <- forAll $ Gen.integral (Range.linear 0 64)
  case (TN.someNatVal vecLen, TN.someNatVal fw) of
    (SomeNat (_ :: Proxy l), SomeNat (_ :: Proxy fw)) -> do
      list <-
        forAll $ Gen.list (Range.singleton listLen) $ genVec @l (genDefinedBitVector @fw)
      vec <-
        forAll $ genVec @l (Gen.list (Range.singleton listLen) (genDefinedBitVector @fw))
      zipList (unzipList list) === list
      unzipList (zipList vec) === vec

-- | Unzips a list of vectors into a vector of lists
unzipList :: forall n a. (KnownNat n) => [Vec n a] -> Vec n [a]
unzipList = L.foldr (zipWith (:)) (repeat [])

{- | Zips a vector of lists into a list of vectors
The list gets truncated to the length of the shortest list
-}
zipList :: forall n a. (KnownNat n, NFDataX a, Show a) => Vec n [a] -> [Vec n a]
zipList vec
  | null vec = []
  | not (any null vec) = heads : zipList tails
  | otherwise = []
 where
  (heads, tails) = funzip $ fmap (fromJust . L.uncons) vec

-- | Unzips a functor of pairs into a pair of functors
funzip :: (Functor f) => f (a, b) -> (f a, f b)
funzip fab = (fst <$> fab, snd <$> fab)
