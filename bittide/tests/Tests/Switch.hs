-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fconstraint-solver-iterations=5 #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Tests.Switch(switchGroup) where

import Clash.Hedgehog.Sized.Vector
import Clash.Prelude
import qualified Prelude as P


import Clash.Sized.Vector ( unsafeFromList)
import Contranomy.Wishbone
import Data.String
import GHC.Natural
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog
import Tests.Calendar
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Bittide.Calendar (CalendarConfig(..))
import Bittide.Switch

switchGroup :: TestTree
switchGroup = testGroup "Switch group"
  [testPropertyNamed "Routing works" "switchFrameRoutingWorks" switchFrameRoutingWorks]

data SwitchConfig  bs aw where
  SwitchConfig :: (1 <= memDepth) => SNat links -> SNat memDepth -> CalendarConfig bs aw (CalendarEntry links memDepth) -> SwitchConfig bs aw

deriving instance Show (SwitchConfig bs aw)

-- This generator can generate a calendar entry for a switch given the amount of links and
-- memory depth.
genSwitchEntry :: forall l d . (KnownNat d, 1 <= d) => SNat l -> SNat d -> Gen (CalendarEntry l d)
genSwitchEntry SNat SNat = genVec elemGen
  where
    genL = Gen.enum 0 (natToNum @(d-1))
    genR = Gen.enum 0 $ natToNum @l
    elemGen = (,) <$> genL <*> genR

-- | This generator can generate a any calendar for the bittide switch, knowing the
-- amount of bytes and addresswidth of the wishbone bus, and given the amount of links,
-- memory depth of the scatter engine and calendar depth of the switch.
genSwitchCalendar :: forall bs aw . (KnownNat bs, 1 <= bs, KnownNat aw) => Natural -> Natural -> Natural -> Gen (SwitchConfig bs aw)
genSwitchCalendar links memDepth calDepth = do
  case (TN.someNatVal links, TN.someNatVal (memDepth - 1)) of
    (SomeNat (snatProxy -> l), SomeNat (succSNat . snatProxy -> d)) -> do
      testCal <- genCalendarConfig calDepth $ genSwitchEntry l d
      return $ SwitchConfig l d testCal

-- | This generator generates a calendar for the switch that has the following properties:
-- - The depth of the scatter memories is equal to the depth of the calendar.
-- - For each scatter engine, every memory address is included in one of the calendar entries.
-- - Each link is selected as source by one of the outgoing links through the crossbar,
-- These properties ensure that all incoming frames will also appear at the output of
-- one of the links.

-- | This test checks that for any switch calendar with memory depth 1 and calendar depth 1
--, all outputs select the correct frame.
switchFrameRoutingWorks :: Property
switchFrameRoutingWorks = property $ do
  links <- forAll $ Gen.enum 1 15
  let
    calDepth = 1
    memDepth = 1
  switchCal <- forAll $ genSwitchCalendar @4 @32 links memDepth calDepth
  case switchCal of
    SwitchConfig SNat SNat calConfig@(CalendarConfig _ (toList -> cal) _) -> do
      let
        links0 = fromIntegral links
        latency = fromIntegral memDepth + 1

      simLength <- forAll $ Gen.enum latency 100
      let
        genFrame = Just <$> Gen.integral Range.linearBounded
        allLinks = Gen.list (Range.singleton links0) genFrame
      topEntityInput <- forAll $ Gen.list (Range.singleton simLength) allLinks

      let
        topEntity streamsIn = withClockResetEnable clockGen resetGen enableGen $
         fst (switch @_ @_ @_ @_ @_ @64 calConfig (pure False) (pure wbNothingM2S) streamsIn)
        simOut = simulateN @System simLength topEntity $ fmap unsafeFromList topEntityInput
        simOut1 = P.drop latency $ fmap toList simOut

      let
        expectedFrames = P.take simLength (P.replicate links0 Nothing : P.replicate links0 Nothing : topEntityInput)
        expectedOutput = P.drop latency . P.take simLength $ P.zipWith selectAllOutputs expectedFrames (cycle $ fmap toList cal)
      footnote . fromString $ "expected:" <> showX expectedOutput
      footnote . fromString $ "simOut1: " <> showX simOut1
      footnote . fromString $ "simOut: " <> showX simOut
      footnote . fromString $ "input: " <> showX topEntityInput
      simOut1 === expectedOutput

selectAllOutputs :: (KnownNat l, KnownNat d) => [Maybe a] -> [(Index d, Index (l+1))] -> [Maybe a]
selectAllOutputs incomingFrames = fmap (selectionFunc . fromEnum . snd)
 where
   allFrames = Nothing : incomingFrames
   selectionFunc = (allFrames P.!!)

wbNothingM2S :: forall bytes aw . (KnownNat bytes, KnownNat aw) => WishboneM2S bytes aw
wbNothingM2S = (wishboneM2S (SNat @bytes) (SNat @aw))
 { addr = 0
 , writeData = 0
 , busSelect = 0}
