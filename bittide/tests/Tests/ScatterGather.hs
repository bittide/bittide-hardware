-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
-- For Show (SNatLE a b)
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Tests.ScatterGather (tests) where

import Clash.Prelude hiding (fromList)
import qualified Prelude as P

import Clash.Sized.Vector (fromList)
import Data.Maybe
import Data.String
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
import qualified Hedgehog.Range as Range

{- | The extra in SomeCalendar extra defines the minimum amount of elements in the vector
and the minimum addressable indexes in the vector elements. I.e, vectors of 0 elements
and Index 0 as element are not allowed.
-}
data SomeCalendar extra where
  SomeCalendar ::
    (1 <= (extra + n)) => SNat n -> Vec (n + extra) (Index (n + extra)) -> SomeCalendar extra

instance Show (SomeCalendar extra) where
  show (SomeCalendar SNat list) = show list

genData :: Gen (BitVector 64)
genData = Gen.integral Range.linearBounded

genFrame :: Gen (Maybe (BitVector 64))
genFrame = Gen.maybe genData

genFrameList :: Range Int -> Gen [Maybe (BitVector 64)]
genFrameList range = Gen.list range genFrame

tests :: TestTree
tests =
  testGroup
    "Tests.ScatterGather"
    [ testPropertyNamed
        "scatterUnitWb - No overwriting implies no lost frames."
        "scatterUnitNoFrameLoss"
        scatterUnitNoFrameLoss
    , testPropertyNamed
        "gatherUnitWb - No overwriting implies no lost frames."
        "gatherUnitNoFrameLoss"
        gatherUnitNoFrameLoss
    , testPropertyNamed
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

-- | Check if the scatter unit with wishbone interface loses no frames.
scatterUnitNoFrameLoss :: Property
scatterUnitNoFrameLoss = property $ do
  maxCalSize <- forAll $ Gen.enum 2 32
  case TN.someNatVal (maxCalSize - 2) of
    SomeNat (addSNat d2 . snatProxy -> p) -> do
      runTest =<< forAll (genCalendarConfig @4 @32 p)
 where
  runTest ::
    (KnownNat maxSize, 1 <= maxSize) =>
    CalendarConfig 4 32 (Index maxSize) ->
    PropertyT IO ()
  runTest calConfig@(CalendarConfig _ calA@(length -> memDepth) _) = do
    -- Number of metacycles of input to generate
    metaCycles <- forAll $ Gen.enum 1 10
    let
      -- reset cycle + cycle delay, last metacycle's writes can be read in (metacycles + 1)
      simLength = 2 + (1 + metaCycles) * memDepth
      inputGen = Gen.list (Range.singleton metaCycles)
      metaCycleNothing = P.replicate memDepth Nothing
      -- Generate at most memDepth `div` 2 elements to be written each metacycle since
      -- we need two cycles to read a written element.
      metaCycleGen = genFrameList (Range.singleton $ memDepth `div` 2)

    inputFrames <-
      forAll
        $ padToLength (simLength `div` memDepth + 1) metaCycleNothing
        <$> inputGen (padToLength memDepth Nothing <$> metaCycleGen)
    let
      topEntity (unbundle -> (wbIn, linkIn)) =
        fst
          $ withClockResetEnable
            clockGen
            resetGen
            enableGen
            (scatterUnitWb @System @32)
            (ScatterConfig calConfig)
            (pure emptyWishboneM2S)
            linkIn
            wbIn

      wbReadOps =
        P.take simLength $ P.replicate memDepth emptyWishboneM2S
          P.++ P.concat
            ( padToLength memDepth emptyWishboneM2S
                . P.concat
                . P.zipWith wbRead (toList $ fmap veEntry calA)
                <$> inputFrames
            )

      topEntityInput = P.zip wbReadOps (P.concat inputFrames)
      simOut = simulateN simLength topEntity topEntityInput
    footnote . fromString $ "simOut: " <> showX simOut
    footnote . fromString $ "simIn: " <> showX wbReadOps
    footnote . fromString $ "cal: " <> showX calA
    wbDecoding simOut === P.take simLength (catMaybes (P.concat inputFrames))
  padToLength l padElement g = P.take l (g P.++ P.repeat padElement)

-- | Check if the gather unit with wishbone interface loses no frames.
gatherUnitNoFrameLoss :: Property
gatherUnitNoFrameLoss = property $ do
  maxCalSize <- forAll $ Gen.enum 2 32
  case TN.someNatVal (maxCalSize - 2) of
    SomeNat (addSNat d2 . snatProxy -> p) -> do
      runTest =<< forAll (genCalendarConfig @4 @32 p)
 where
  runTest ::
    (KnownNat maxSize, 1 <= maxSize) =>
    CalendarConfig 4 32 (Index maxSize) ->
    PropertyT IO ()
  runTest calConfig@(CalendarConfig _ calA@(length -> memDepth) _) = do
    metaCycles <- forAll $ Gen.enum 1 10
    let
      activeEntryList = toList $ fmap veEntry calA
      simLength = 2 + (1 + metaCycles) * memDepth
      inputGen = Gen.list (Range.singleton metaCycles)
      metaCycleNothing = P.replicate memDepth Nothing
      metaCycleGen = genFrameList (Range.singleton $ memDepth `div` 2)
    inputFrames <-
      forAll
        $ padToLength (simLength `div` memDepth + 1) metaCycleNothing
        <$> inputGen (padToLength memDepth Nothing <$> metaCycleGen)
    let
      topEntity wbIn =
        (\(a, _, _) -> a)
          $ withClockResetEnable
            clockGen
            resetGen
            enableGen
            (gatherUnitWb @System @30)
            (GatherConfig calConfig)
            (pure emptyWishboneM2S)
            wbIn

      wbWriteOps =
        P.take simLength
          . P.concat
          $ padToLength memDepth emptyWishboneM2S
          . P.concat
          . P.zipWith wbWrite activeEntryList
          <$> inputFrames

      simOut = simulateN simLength topEntity wbWriteOps
      addressedFrames = P.zip (P.concat inputFrames) (cycle activeEntryList)
      writtenFrames = [if snd e /= 0 then fst e else Nothing | e <- addressedFrames]
      prePad items = P.replicate (1 + memDepth) Nothing P.++ items
      expectedOutput = P.take simLength (fromMaybe 1 <$> P.filter isJust writtenFrames)

    footnote . fromString $ "simOut: " <> showX simOut
    footnote . fromString $ "simIn: " <> showX wbWriteOps
    footnote . fromString $ "cal: " <> showX calA
    footnote . fromString $ "writtenFrames: " <> showX writtenFrames

    directedDecode (prePad writtenFrames) simOut === expectedOutput

  padToLength l padElement g = P.take l (g P.++ P.repeat padElement)

directedDecode :: [Maybe a] -> [Maybe b] -> [b]
directedDecode ((Just _) : as) ((Just b) : bs) = b : directedDecode as bs
directedDecode (Nothing : as) (_ : bs) = directedDecode as bs
directedDecode _ _ = []

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
              (ScatterConfig calConfig)
              (pure emptyWishboneM2S)
              linkIn
              wbStall
        guWB =
          wcre
            $ (\(_, x, _) -> x)
            $ gatherUnitWb @System
              (GatherConfig calConfig)
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

{- | Decode an incoming slave bus by consuming two acknowledged signals and concatenating
their readData's.
-}
wbDecoding ::
  (KnownNat nBytes) =>
  [WishboneS2M (Bytes nBytes)] ->
  [Bytes (nBytes + nBytes)]
wbDecoding (s2m0 : s2m1 : s2ms)
  | acknowledge s2m0 && acknowledge s2m1 = out : wbDecoding s2ms
  | otherwise = wbDecoding (s2m1 : s2ms)
 where
  out = readData s2m0 ++# readData s2m1
wbDecoding _ = []

{- | Tranform a read address with expected frame into a wishbone read operation for testing
the 'scatterUnitWb'. The second argument indicate wether or not a frame can be read from
that read address. The read operation reads data over 2 read cycles.
-}
wbRead ::
  forall nBytes addrW maxIndex a.
  ( KnownNat nBytes
  , KnownNat addrW
  , KnownNat maxIndex
  , 1 <= maxIndex
  ) =>
  Index maxIndex ->
  Maybe a ->
  [WishboneM2S addrW nBytes (Bytes nBytes)]
wbRead readAddr (Just _) =
  [ (emptyWishboneM2S @addrW @(Bytes nBytes))
      { addr = (`shiftL` 1) . resize $ pack readAddr
      , busCycle = True
      , strobe = True
      , busSelect = maxBound
      }
  , (emptyWishboneM2S @addrW @(Bytes nBytes))
      { addr = 1 .|. ((`shiftL` 1) . resize $ pack readAddr)
      , busCycle = True
      , strobe = True
      , busSelect = maxBound
      }
  ]
wbRead _ Nothing = []

{- | Transform a write address with frame to a wishbone write operation for testing the
'gatherUnitWb'. The write operation writes the incoming bitvector over 2 write cycles.
-}
wbWrite ::
  forall nBytes addrW maxIndex.
  ( KnownNat nBytes
  , KnownNat addrW
  , KnownNat maxIndex
  , 1 <= maxIndex
  ) =>
  Index maxIndex ->
  Maybe (Bytes (nBytes * 2)) ->
  [WishboneM2S addrW nBytes (Bytes nBytes)]
wbWrite writeAddr (Just frame) =
  [ (emptyWishboneM2S @addrW @(Bytes nBytes))
      { addr = (`shiftL` 1) . resize $ pack writeAddr
      , busSelect = maxBound
      , busCycle = True
      , strobe = True
      , writeEnable = True
      , writeData = lower
      }
  , (emptyWishboneM2S @addrW @(Bytes nBytes))
      { addr = 1 .|. ((`shiftL` 1) . resize $ pack writeAddr)
      , busSelect = maxBound
      , busCycle = True
      , strobe = True
      , writeEnable = True
      , writeData = upper
      }
  ]
 where
  (lower, upper) = split frame
wbWrite _ Nothing = []
