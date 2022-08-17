-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fconstraint-solver-iterations=5 #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Tests.Switch(switchGroup) where

import Clash.Hedgehog.Sized.Index
import Clash.Hedgehog.Sized.Vector
import Clash.Prelude
import qualified Prelude as P

import Clash.Sized.Vector ( unsafeFromList)

import Data.String
import GHC.Natural
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

import qualified Data.Sequence as Seq
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Bittide.Calendar (CalendarConfig(..))
import Bittide.Extra.Wishbone
import Bittide.Switch
import Tests.Calendar
import Tests.Shared

switchGroup :: TestTree
switchGroup = testGroup "Switch group"
  [testPropertyNamed "Routing works" "switchFrameRoutingWorks" switchFrameRoutingWorks]

data SwitchConfig  nBytes addrW where
  SwitchConfig ::
    1 <= memDepth =>
    SNat links ->
    SNat memDepth ->
    CalendarConfig nBytes addrW (CalendarEntry links memDepth) ->
    SwitchConfig nBytes addrW

deriving instance Show (SwitchConfig nBytes addrW)

-- This generator can generate a calendar entry for a switch given the amount of links and
-- memory depth.
genSwitchEntry ::
  forall links scatterDepth .
  1 <= scatterDepth =>
  SNat links ->
  SNat scatterDepth ->
  Gen (CalendarEntry links scatterDepth)
genSwitchEntry SNat SNat = genVec elemGen
 where
  genScatterEntry = genIndex Range.constantBounded
  genLinkEntry = genIndex Range.constantBounded
  elemGen = (,) <$> genScatterEntry <*> genLinkEntry

-- | This generator can generate a any calendar for the bittide switch, knowing the
-- amount of bytes and address width of the wishbone bus, and given the amount of links,
-- memory depth of the scatter engine and calendar depth of the switch.
genSwitchCalendar ::
  forall nBytes addrW .
  (KnownNat nBytes, 1 <= nBytes, KnownNat addrW) =>
  Natural ->
  Natural ->
  Natural ->
  Gen (SwitchConfig nBytes addrW)
genSwitchCalendar links memDepth calDepth = do
  case (TN.someNatVal links, TN.someNatVal (memDepth - 1)) of
    (SomeNat (snatProxy -> l), SomeNat (succSNat . snatProxy -> d)) -> do
      testCal <- genCalendarConfig calDepth $ genSwitchEntry l d
      return $ SwitchConfig l d testCal

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
        genFrame = Just <$> genDefinedBitVector @64
        allLinks = Gen.list (Range.singleton links0) genFrame
      topEntityInput <- forAll $ Gen.list (Range.singleton simLength) allLinks

      let
        topEntity streamsIn = withClockResetEnable clockGen resetGen enableGen $
         fst (switch calConfig (pure emptyWishboneM2S) streamsIn)
        simOut = simulateN @System simLength topEntity $ fmap unsafeFromList topEntityInput
        simOut1 = P.drop latency $ fmap toList simOut

      let
        expectedFrames = P.take simLength
          (P.replicate links0 Nothing : P.replicate links0 Nothing : topEntityInput)
        expectedOutput = P.drop latency . P.take simLength $
          P.zipWith selectAllOutputs expectedFrames (cycle $ fmap toList cal)
      footnote . fromString $ "expected:" <> showX expectedOutput
      footnote . fromString $ "simOut1: " <> showX simOut1
      footnote . fromString $ "simOut: " <> showX simOut
      footnote . fromString $ "input: " <> showX topEntityInput
      simOut1 === expectedOutput

selectAllOutputs ::
  (KnownNat l, KnownNat d) =>
  [Maybe a] ->
  [(Index d, Index (l+1))] ->
  [Maybe a]
selectAllOutputs incomingFrames = fmap (selectionFunc . fromEnum . snd)
 where
  allFrames = Nothing Seq.<| Seq.fromList incomingFrames
  selectionFunc = (allFrames `Seq.index`)
