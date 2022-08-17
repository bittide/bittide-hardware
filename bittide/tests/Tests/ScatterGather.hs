-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

-- For Show (SNatLE a b)
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Tests.ScatterGather(sgGroup) where

import Clash.Prelude hiding (fromList)
import qualified Prelude as P

import Clash.Sized.Vector (fromList)
import Data.String
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Clash.Util.Interpolate as I
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Bittide.Calendar
import Bittide.Extra.Wishbone
import Bittide.ScatterGather
import Bittide.SharedTypes
import Data.Maybe
import Tests.Shared

-- | The extra in SomeCalendar extra defines the minimum amount of elements in the vector
-- and the minimum addressable indexes in the vector elements. I.e, vectors of 0 elements
-- and Index 0 as element are not allowed.
data SomeCalendar extra where
  SomeCalendar :: (1 <= (extra + n)) => SNat n -> Vec (n + extra) (Index (n + extra)) -> SomeCalendar extra

instance Show (SomeCalendar extra) where
  show (SomeCalendar SNat list) = show list

genData :: Gen (BitVector 64)
genData = Gen.integral Range.linearBounded

genFrame :: Gen (Maybe (BitVector 64))
genFrame = Gen.maybe genData

genFrameList :: Range Int -> Gen [Maybe (BitVector 64)]
genFrameList range = Gen.list range genFrame

sgGroup :: TestTree
sgGroup = testGroup "Scatter Gather group"
  [ testPropertyNamed "scatterUnitWb - No overwriting implies no lost frames." "scatterUnitNoFrameLoss" scatterUnitNoFrameLoss
  , testPropertyNamed "gatherUnitWb - No overwriting implies no lost frames." "gatherUnitNoFrameLoss" gatherUnitNoFrameLoss
  ]

-- | Generates a 'CalendarConfig' for the 'gatherUnitWb' or 'scatterUnitWb'
genCalendarConfig ::
  forall nBytes addrW calEntry maxDepth .
  ( KnownNat nBytes
  , 1 <= nBytes
  , KnownNat maxDepth
  , 1 <= maxDepth
  , calEntry ~ Index maxDepth
  , KnownNat addrW) =>
  SNat maxDepth ->
  Gen (CalendarConfig nBytes addrW calEntry)
genCalendarConfig sizeNat@(snatToNum -> dMax) = do
  dA <- Gen.enum 1 dMax
  dB <- Gen.enum 1 dMax
  case (TN.someNatVal dA, TN.someNatVal dB) of
    ( SomeNat (snatProxy -> depthA)
     ,SomeNat (snatProxy -> depthB)) -> do
        let
          regAddrBits = SNat @(NatRequiredBits (Regs calEntry (nBytes * 8)))
          bsCalEntry = SNat @(BitSize calEntry)
        case
         ( isInBounds d1 depthA sizeNat
         , isInBounds d1 depthB sizeNat
         , compareSNat regAddrBits (SNat @addrW)
         , compareSNat d1 bsCalEntry) of
          (InBounds, InBounds, SNatLE, SNatLE) -> go depthA depthB
          (a, b, c, d) -> error [I.i|
              genCalendarConfig: calEntry constraints not satisfied:

                a: #{a}
                b: #{b}
                b: #{c}
                c: #{d}

              ...
          |]
 where
    go :: forall depthA depthB .
      ( LessThan depthA maxDepth
      , LessThan depthB maxDepth
      , NatFitsInBits (Regs calEntry (nBytes * 8)) addrW) =>
      SNat depthA ->
      SNat depthB ->
      Gen (CalendarConfig nBytes addrW (Index maxDepth))
    go SNat SNat = do
      calActive <- fromMaybe errmsg . fromList @depthA . P.take (natToNum @depthA)
        <$> Gen.shuffle @_ @(Index maxDepth) [0.. natToNum @(maxDepth-1)]
      calShadow <- fromMaybe errmsg . fromList @depthB . P.take (natToNum @depthB)
        <$> Gen.shuffle @_ @(Index maxDepth) [0.. natToNum @(maxDepth-1)]
      return $ CalendarConfig sizeNat calActive calShadow
    errmsg = errorX "genCalendarConfig: list to vector conversion failed"

-- | Check if the scatter unit with wishbone interface loses no frames.
scatterUnitNoFrameLoss :: Property
scatterUnitNoFrameLoss = property $ do
  maxCalSize <- forAll $ Gen.enum 2 32
  case TN.someNatVal (maxCalSize - 1) of
    SomeNat (succSNat . snatProxy -> p) -> do
      runTest =<< forAll (genCal p)
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
      simLength = 2 + (1+metaCycles) * memDepth
      inputGen = Gen.list (Range.singleton metaCycles)
      metaCycleNothing = P.replicate memDepth Nothing
      -- Generate at most memDepth `div` 2 elements to be written each metacycle since
      -- we need two cycles to read a written element.
      metaCycleGen = genFrameList (Range.singleton $ memDepth `div` 2)

    inputFrames <- forAll $ padToLength (simLength `div` memDepth + 1) metaCycleNothing
      <$> inputGen (padToLength memDepth Nothing <$> metaCycleGen)
    let
      topEntity (unbundle -> (wbIn, linkIn)) = fst $
        withClockResetEnable clockGen resetGen enableGen (scatterUnitWb @System @_ @32)
        calConfig (pure emptyWishboneM2S) linkIn wbIn

      wbReadOps = P.take simLength $ P.replicate memDepth emptyWishboneM2S P.++  P.concat
        (padToLength memDepth emptyWishboneM2S . P.concat . P.zipWith wbRead (toList calA) <$> inputFrames)

      topEntityInput = P.zip wbReadOps (P.concat inputFrames)
      simOut = simulateN simLength topEntity topEntityInput
    footnote . fromString $ "simOut: " <> showX simOut
    footnote . fromString $ "simIn: " <> showX wbReadOps
    footnote . fromString $ "cal: " <> showX calA
    wbDecoding simOut === P.take simLength (catMaybes (P.concat inputFrames))

  genCal :: forall maxSize . 1 <= maxSize => SNat maxSize -> Gen (CalendarConfig 4 32 (Index maxSize))
  genCal SNat = genCalendarConfig @4 @32 (SNat @maxSize)
  padToLength l padElement g = P.take l (g P.++ P.repeat padElement)

-- | Check if the gather unit with wishbone interface loses no frames.
gatherUnitNoFrameLoss :: Property
gatherUnitNoFrameLoss = property $ do
  maxCalSize <- forAll $ Gen.enum 2 32
  case TN.someNatVal (maxCalSize - 1) of
    SomeNat (succSNat . snatProxy -> p) -> do
      runTest =<< forAll (genCal p)
 where
  runTest ::
    (KnownNat maxSize, 1 <= maxSize) =>
    CalendarConfig 4 32 (Index maxSize) -> PropertyT IO ()
  runTest calConfig@(CalendarConfig _ calA@(length -> memDepth) _) = do
    metaCycles <- forAll $ Gen.enum 1 10
    let
      simLength = 2 + (1+metaCycles) * memDepth
      inputGen = Gen.list (Range.singleton metaCycles)
      metaCycleNothing = P.replicate memDepth Nothing
      metaCycleGen = genFrameList (Range.singleton $ memDepth `div` 2)
    inputFrames <- forAll $ padToLength (simLength `div` memDepth + 1) metaCycleNothing
      <$> inputGen (padToLength memDepth Nothing <$> metaCycleGen)
    let
      topEntity wbIn = (\ (a, _ ,_) -> a) $
        withClockResetEnable clockGen resetGen enableGen (gatherUnitWb @System @_ @32)
        calConfig (pure emptyWishboneM2S) wbIn

      wbWriteOps = P.take simLength . P.concat $
        padToLength memDepth emptyWishboneM2S . P.concat . P.zipWith wbWrite (toList calA) <$> inputFrames

      simOut = simulateN simLength topEntity wbWriteOps
      addressedFrames = P.zip (P.concat inputFrames) (cycle $ toList calA)
      writtenFrames = [if snd e /= 0 then fst e else Nothing | e <- addressedFrames]
      prePad items = P.replicate (1+memDepth) Nothing P.++ items
      expectedOutput = P.take simLength (fromMaybe 1 <$> P.filter isJust writtenFrames)

    footnote . fromString $ "simOut: " <> showX simOut
    footnote . fromString $ "simIn: " <> showX wbWriteOps
    footnote . fromString $ "cal: " <> showX calA
    footnote . fromString $ "writtenFrames: " <> showX writtenFrames

    directedDecode (prePad writtenFrames) simOut === expectedOutput

  genCal :: forall maxSize .
   1 <= maxSize =>
   SNat maxSize ->
   Gen (CalendarConfig 4 32 (Index maxSize))
  genCal SNat = genCalendarConfig @4 @32 (SNat @maxSize)
  padToLength l padElement g = P.take l (g P.++ P.repeat padElement)

directedDecode :: [Maybe a] -> [Maybe b] -> [b]
directedDecode ((Just _) : as) ((Just b) : bs) = b : directedDecode as bs
directedDecode (Nothing : as) (_ : bs) = directedDecode as bs
directedDecode _ _ = []

-- | Decode an incoming slave bus by consuming two acknowledged signals and concatenating
-- their readData's.
wbDecoding ::
  KnownNat nBytes =>
  [WishboneS2M nBytes] ->
  [BitVector ((8 * nBytes) + (8 * nBytes))]
wbDecoding (s2m0 : s2m1 : s2ms)
  | acknowledge s2m0 && acknowledge s2m1 = out : wbDecoding s2ms
  | otherwise = wbDecoding (s2m1 : s2ms)
 where
  out = readData s2m1 ++# readData s2m0
wbDecoding _ = []

-- | Tranform a read address with expected frame into a wishbone read operation for testing
-- the 'scatterUnitWb'. The second argument indicate wether or not a frame can be read from
-- that read address. The read operation reads data over 2 read cycles.
wbRead ::
  forall nBytes addrW maxIndex a .
  ( KnownNat nBytes
  , KnownNat addrW
  , KnownNat maxIndex
  , 1 <= maxIndex) =>
  Index maxIndex ->
  Maybe a ->
  [WishboneM2S nBytes addrW]
wbRead readAddr (Just _) =
  [ (emptyWishboneM2S @nBytes @addrW)
    { addr = (`shiftL` 3) . resize $ pack readAddr
    , busCycle = True
    , strobe = True }

  , (emptyWishboneM2S @nBytes @addrW)
    { addr =  4 .|.  ((`shiftL` 3) . resize $ pack readAddr)
    , busCycle = True
    , strobe = True }
  ]
wbRead _ Nothing = []

-- | Transform a write address with frame to a wishbone write operation for testing the
-- 'gatherUnitWb'. The write operation writes the incoming bitvector over 2 write cycles.
wbWrite ::
  forall nBytes addrW maxIndex .
  ( KnownNat nBytes
  , KnownNat addrW
  , KnownNat maxIndex
  , 1 <= maxIndex) =>
  Index maxIndex ->
  Maybe (BitVector (nBytes*2*8)) ->
  [WishboneM2S nBytes addrW]
wbWrite writeAddr (Just frame) =
  [ (emptyWishboneM2S @nBytes @addrW)
    { addr = (`shiftL` 3) . resize $ pack writeAddr
    , busSelect = maxBound
    , busCycle = True
    , strobe = True
    , writeEnable = True
    , writeData = lower }

  , (emptyWishboneM2S @nBytes @addrW)
    { addr =  4 .|.  ((`shiftL` 3) . resize $ pack writeAddr)
    , busSelect = maxBound
    , busCycle = True
    , strobe = True
    , writeEnable = True
    , writeData = upper }
  ]
 where
  (upper, lower) = split frame
wbWrite _ Nothing = []
