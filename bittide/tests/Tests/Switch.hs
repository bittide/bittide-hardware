-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fconstraint-solver-iterations=5 #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Switch(switchGroup) where

import Clash.Prelude

import Clash.Hedgehog.Sized.Index
import Clash.Hedgehog.Sized.Vector
import Clash.Sized.Vector ( unsafeFromList)
import Data.String
import GHC.Natural
import Hedgehog
import Protocols.Wishbone
import Test.Tasty
import Test.Tasty.Hedgehog

import Bittide.Calendar (CalendarConfig(..))
import Bittide.Switch
import Tests.Calendar
import Tests.Shared

import qualified Data.Sequence as Seq
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Prelude as P

switchGroup :: TestTree
switchGroup = testGroup "Switch group"
  [testPropertyNamed "Routing works" "switchFrameRoutingWorks" switchFrameRoutingWorks]

data SwitchTestConfig  nBytes addrW where
  SwitchTestConfig ::
    SNat links ->
    CalendarConfig nBytes addrW (CalendarEntry links) ->
    SwitchTestConfig nBytes addrW

deriving instance Show (SwitchTestConfig nBytes addrW)

-- This generator can generate a calendar entry for a switch given the amount of links.
genSwitchEntry ::
  forall links .
  SNat links ->
  Gen (CalendarEntry links)
genSwitchEntry SNat = genVec (genIndex Range.constantBounded)

-- | This generator can generate a calendar for the bittide switch, knowing the
-- amount of bytes and address width of the wishbone bus, and given the amount of links and
-- calendar depth of the switch.
genSwitchCalendar ::
  forall nBytes addrW .
  (KnownNat nBytes, 1 <= nBytes, KnownNat addrW) =>
  Natural ->
  Natural ->
  Gen (SwitchTestConfig nBytes addrW)
genSwitchCalendar links calDepth = do
  case TN.someNatVal links of
    (SomeNat (snatProxy -> l)) -> do
      testCal <- genCalendarConfig calDepth $ genSwitchEntry l
      return $ SwitchTestConfig l testCal

-- | This test checks that for any switch calendar all outputs select the correct frame.
switchFrameRoutingWorks :: Property
switchFrameRoutingWorks = property $ do
  links <- forAll $ Gen.int (Range.constant 1 15)
  calDepth <- forAll $ Gen.enum 1 8
  switchCal <- forAll $ genSwitchCalendar @4 @32 (fromIntegral links) calDepth
  case switchCal of
    SwitchTestConfig (SNat :: SNat links) calConfig@(CalendarConfig _ (toList -> cal) _) -> do
      simLength <- forAll $ Gen.enum 1 (3 * fromIntegral calDepth)
      preamble <- forAll (genDefinedBitVector @64)
      let
        genFrame = Just <$> genDefinedBitVector @64
        allLinks = Gen.list (Range.singleton links) genFrame
      topEntityInput <- forAll $ Gen.list (Range.singleton simLength) allLinks
      let
        topEntity streamsIn = withClockResetEnable clockGen resetGen enableGen $ bundle $
         fst $ switch preamble calConfig (repeat $ pure emptyWishboneM2S) $ unbundle streamsIn
        simOut = simulateN @System simLength topEntity $ fmap unsafeFromList topEntityInput
      let
        expectedFrames = P.replicate links Nothing : topEntityInput
        expectedOutput = P.take simLength $ P.replicate links Nothing :
          P.zipWith selectAllOutputs expectedFrames (cycle $ fmap toList cal)
      footnote . fromString $ "expected:" <> showX expectedOutput
      footnote . fromString $ "simOut: " <> showX simOut
      footnote . fromString $ "input: " <> showX topEntityInput
      fmap toList simOut === expectedOutput

selectAllOutputs ::
  (KnownNat l) =>
  [Maybe a] ->
  [Index (l+1)] ->
  [Maybe a]
selectAllOutputs incomingFrames = fmap (selectionFunc . fromEnum)
 where
  allFrames = Nothing Seq.<| Seq.fromList incomingFrames
  selectionFunc = (allFrames `Seq.index`)
