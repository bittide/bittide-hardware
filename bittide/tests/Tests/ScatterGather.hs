-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- For Show (SNatLE a b)
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.ScatterGather (tests) where

import Clash.Prelude hiding (fromList)
import qualified Prelude as P

import Clash.Sized.Vector (fromList)
import Data.Maybe
import Hedgehog
import Protocols.Wishbone
import Test.Tasty
import Test.Tasty.Hedgehog

import Bittide.Calendar hiding (ExtraRegs)
import Bittide.ScatterGather
import Bittide.SharedTypes
import Tests.Shared

import qualified Bittide.Calendar as Cal (ExtraRegs)
import qualified Clash.Util.Interpolate as I
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen

{- | The extra in SomeCalendar extra defines the minimum amount of elements in the vector
and the minimum addressable indexes in the vector elements. I.e, vectors of 0 elements
and Index 0 as element are not allowed.
-}
data SomeCalendar extra where
  SomeCalendar ::
    (1 <= (extra + n)) => SNat n -> Vec (n + extra) (Index (n + extra)) -> SomeCalendar extra

instance Show (SomeCalendar extra) where
  show (SomeCalendar SNat list) = show list

tests :: TestTree
tests =
  testGroup
    "Tests.ScatterGather"
    [ testPropertyNamed
        "S/G units - Ack stalling address at metacycle end."
        "metacycleStalling"
        metacycleStalling
    ]

-- | Generates a 'CalendarConfig' for the 'gatherUnitWb' or 'scatterUnitWb'
genCalendarConfig ::
  forall nBytes addrW calEntry maxDepth.
  ( KnownNat nBytes
  , 1 <= nBytes
  , KnownNat maxDepth
  , 2 <= maxDepth
  , KnownNat addrW
  , calEntry ~ Index maxDepth
  ) =>
  SNat maxDepth ->
  Gen (CalendarConfig nBytes addrW calEntry)
genCalendarConfig sizeNat@(snatToNum -> dMax) = do
  dA <- Gen.enum 1 dMax
  dB <- Gen.enum 1 dMax
  case (TN.someNatVal dA, TN.someNatVal dB) of
    ( SomeNat (snatProxy -> depthA)
      , SomeNat (snatProxy -> depthB)
      ) -> do
        let
          regAddrBits = SNat @(2 + NatRequiredBits (Regs calEntry (nBytes * 8) + Cal.ExtraRegs))
          bsCalEntry = SNat @(BitSize calEntry)
        case ( isInBounds d1 depthA sizeNat
             , isInBounds d1 depthB sizeNat
             , compareSNat regAddrBits (SNat @addrW)
             , compareSNat d1 bsCalEntry
             ) of
          (InBounds, InBounds, SNatLE, SNatLE) -> go depthA depthB
          (a, b, c, d) ->
            error
              [I.i|
              genCalendarConfig: calEntry constraints not satisfied:

                a: #{a}
                b: #{b}
                c: #{c}
                d: #{d}

              ...
          |]
 where
  go ::
    forall depthA depthB.
    ( 1 <= depthA
    , 1 <= depthB
    , LessThan depthA maxDepth
    , LessThan depthB maxDepth
    ) =>
    SNat depthA ->
    SNat depthB ->
    Gen (CalendarConfig nBytes addrW (Index maxDepth))
  go SNat SNat = do
    calActive <-
      fmap nonRepeatingEntry
        . fromMaybe errmsg
        . fromList @depthA
        . P.take (natToNum @depthA)
        <$> Gen.shuffle @_ @(Index maxDepth)
          [0 .. natToNum @(maxDepth - 1)]
    calShadow <-
      fmap nonRepeatingEntry
        . fromMaybe errmsg
        . fromList @depthB
        . P.take (natToNum @depthB)
        <$> Gen.shuffle @_ @(Index maxDepth)
          [0 .. natToNum @(maxDepth - 1)]
    return $ CalendarConfig sizeNat calActive calShadow
  errmsg = errorX "genCalendarConfig: list to vector conversion failed"

{- | Simple  test which generates a 'scatterUnitWb' and 'gatherUnitWb' with a certain calendar
Their wishbone busses are statically hooked up to a transaction that reads from the
stalling address. This test checks that it generates an acknowledge on this address
one cycle after the end of each metacycle (at the start of every _new_ metacycle).
-}
metacycleStalling :: Property
metacycleStalling = property $ do
  maxCalSize <- forAll $ Gen.enum 2 32
  case TN.someNatVal (maxCalSize - 2) of
    SomeNat (addSNat d2 . snatProxy -> p) -> do
      runTest =<< forAll (genCalendarConfig @4 @32 p)
 where
  runTest ::
    forall maxSize.
    (KnownNat maxSize, 2 <= maxSize) =>
    CalendarConfig 4 32 (Index maxSize) ->
    PropertyT IO ()
  runTest calConfig@(CalendarConfig _ (length -> calSize) _) = do
    metacycles <- forAll $ Gen.enum 1 5
    let
      simLength = 1 + metacycles * calSize
      topEntity = bundle (acknowledge <$> suWB, acknowledge <$> guWB)
       where
        suWB =
          wcre
            $ fst
            $ scatterUnitWb @System
              (ScatterConfig SNat calConfig)
              (pure emptyWishboneM2S)
              linkIn
              wbStall
        guWB =
          wcre
            $ (\(_, x, _) -> x)
            $ gatherUnitWb @System
              (GatherConfig SNat calConfig)
              (pure emptyWishboneM2S)
              wbStall
        wbStall =
          pure
            $ (emptyWishboneM2S @32)
              { -- 2 because addressing is 64 bit aligned.
                addr = (2 * (natToNum @maxSize @(BitVector 32)))
              , busCycle = True
              , strobe = True
              }
        linkIn = pure $ deepErrorX "linkIn undefined."
      expectedAcks =
        P.take simLength
          $ P.replicate (1 + calSize) False
          <> cycle (True : P.replicate (calSize - 1) False)
      simOut = sampleN simLength topEntity
    simOut === fmap (\a -> (a, a)) expectedAcks
