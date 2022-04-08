-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Tests.ScatterGather(sgGroup) where

import Clash.Prelude hiding (fromList)
import qualified Prelude as P

import Clash.Sized.Internal.BitVector
import Clash.Sized.Vector ( unsafeFromList, fromList)
import Contranomy.Wishbone
import Data.Bifunctor
import Data.String
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Data.List as L
import qualified Data.Set as Set
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Bittide.ScatterGather
import Bittide.Calendar
import Data.Maybe
import Bittide.SharedTypes
import Tests.Shared

isUndefined :: forall n . KnownNat n => BitVector n -> Bool
isUndefined (BV mask _) = mask == full
 where full = 1 `shiftL` (natToNum @n @Int) - 1

maybeIsUndefined :: (KnownNat n, 1 <= n) => BitVector n -> Maybe (BitVector n)
maybeIsUndefined v  | isUndefined v = Nothing
                    | otherwise     = Just v

-- | The extra in SomeCalendar extra defines the minimum amount of elements in the vector
-- and the minimum addressable indexes in the vector elements. I.e, vectors of 0 elements
-- and Index 0 as element are not allowed.
data SomeCalendar extra where
  SomeCalendar :: (1 <= (extra + n)) => SNat n -> Vec (n + extra) (Index (n + extra)) -> SomeCalendar extra

instance Show (SomeCalendar extra) where
  show (SomeCalendar SNat list) = show list

-- | Returns a calendar for a engine with a memory depth equal to the calendar depth,
-- the addresses in the calendar are all unique.
genSomeCalendar :: Gen (SomeCalendar 1)
genSomeCalendar = do
  calendarSize <- Gen.enum 1 255
  case TN.someNatVal calendarSize of
    (SomeNat size) -> do
      cal <- Gen.shuffle [0.. fromIntegral calendarSize]
      return (SomeCalendar (snatProxy size) $ unsafeFromList cal)

genData :: Gen (BitVector 64)
genData = Gen.integral Range.linearBounded

genFrame :: Gen (Maybe (BitVector 64))
genFrame = Gen.maybe genData

genFrameList :: Range Int -> Gen [Maybe (BitVector 64)]
genFrameList range = Gen.list range genFrame

sgGroup :: TestTree
sgGroup = testGroup "Scatter Gather group"
  [ testPropertyNamed "GatherSequential - No overwriting implies no lost frames." "engineNoFrameLoss gatherEngine" (engineNoFrameLoss gatherEngine)
  , testPropertyNamed "ScatterSequential - No overwriting implies no lost frames." "engineNoFrameLoss scatterEngine" (engineNoFrameLoss scatterEngine)
  , testPropertyNamed "ScatterGather - No overwriting implies no lost frames." "scatterGatherNoFrameLoss" scatterGatherNoFrameLoss
  , testPropertyNamed "scatterUnitWB - No overwriting implies no lost frames." "scatterUnitNoFrameLoss" scatterUnitNoFrameLoss
  , testPropertyNamed "gatherUnitWB - No overwriting implies no lost frames." "gatherUnitNoFrameLoss" gatherUnitNoFrameLoss
  ]

-- |The type of a sequential engine that is tested by engineNoFrameLoss.
type MemoryEngine =
  forall dom memDepth a .
  (NFDataX a, KnownNat memDepth, 1 <= memDepth, HiddenClockResetEnable dom) =>
  -- | Indicates when a new metacycle has started.
  Signal dom Bool ->
  -- | Incoming frame from link, if it contains Just a, a will be written to the memory.
  Signal dom (Maybe a) ->
  -- | Write address, when the incoming frame contains Just a, a will be written to this address.
  Signal dom (Index memDepth) ->
  -- | Outgoing data
  Signal dom a

-- | Tests that for a calendar that contains only unique entries,
-- all frames send to the gather engine will appear at its output.
-- The amount of frames we test for is equal to twice the size of the buffer, since
-- it is double buffered and +2 to account for reset cycle and the one cycle delay.
engineNoFrameLoss :: MemoryEngine -> Property
engineNoFrameLoss engine = property $ do
  someCalendar <- forAll genSomeCalendar
  case someCalendar of
    SomeCalendar size@SNat (toList -> calendar0) -> do
      inputFrames <- forAll $ genFrameList (Range.constant 1 100)
      let
        topEntity (unbundle -> (frameIn, calIn, newMeta)) =
          maybeIsUndefined <$> withClockResetEnable clockGen resetGen enableGen
          engine newMeta frameIn calIn
        -- Simulate for at least twice the size of the doublebuffered memory + the
        -- length of the inputdata + 2 to compensate for the reset cycle and delay cycle.
        simLength = 2 * snatToNum size + P.length inputFrames + 2
        inputFrames' = P.take simLength $ inputFrames <> P.repeat Nothing
        newMetaSignal = cycle $ (==0) <$> [0..P.length calendar0 - 1]
        topEntityInput = P.zip3 inputFrames' (cycle calendar0) newMetaSignal
        simOut = simulateN @System simLength topEntity topEntityInput
      footnote . fromString $ showX simOut
      footnote . fromString $ showX topEntityInput
      Set.fromList inputFrames' === Set.fromList simOut

filterSGOut ::
  (KnownDomain dom, KnownNat n, 1 <= n) =>
  (Signal dom (BitVector n), Signal dom (Maybe (BitVector n))) ->
  (Signal dom (Maybe (BitVector n)), Signal dom (Maybe (BitVector n)))
filterSGOut (toP, toS) = (maybeIsUndefined <$> toP, (maybeIsUndefined =<<) <$> toS)

filterZeroes :: (Num a, Eq a) => [Maybe f] -> [a] -> [Maybe f]
filterZeroes fs as = [ if a == 0 then Nothing else f | (f, a) <- P.zip fs as]

-- | Tests that for a calendar that contains only unique entries,
-- all frames send from the PE side to a non zero address, appear at the PE side output.
scatterGatherNoFrameLoss :: Property
scatterGatherNoFrameLoss = property $ do
  someCalendarS <- forAll genSomeCalendar
  someCalendarG <- forAll genSomeCalendar
  inputFramesSwitch <- forAll $ genFrameList (Range.constant 1 100)
  inputFramesPE <- forAll $ genFrameList (Range.constant 1 100)
  case (someCalendarS, someCalendarG) of
    (SomeCalendar depthScat@SNat calScat, SomeCalendar depthGath@SNat calGath) -> do
      let
        addressesScat = cycle $ toList calScat
        addressesGath = cycle $ toList calGath
        inputFramesSwitch' = inputFramesSwitch <> P.repeat Nothing
        inputFramesPE' = inputFramesPE <> P.repeat Nothing

        topEntity (unbundle -> (frameInS, frameInP, readAddrPE, writeAddrPE)) = bundle $
          filterSGOut @System (withClockResetEnable clockGen resetGen enableGen
          scatterGatherEngine calScat calGath (pure Nothing) (pure Nothing) frameInS frameInP
          readAddrPE writeAddrPE)

        maxCalDepth = max (snatToNum depthScat) (snatToNum depthGath)
        maxInputLength = max (P.length inputFramesSwitch) (P.length inputFramesPE)
        -- Simulate for at least the largest calendar + twice the length of the longest input.
        -- This ensures all frames will appear at the output.
        simLength = maxCalDepth + 2 * maxInputLength + 1
        topEntityInput = L.zip4 inputFramesSwitch' inputFramesPE' addressesScat addressesGath
        simOut = simulateN simLength topEntity topEntityInput

        expectedScat = filterZeroes (P.take simLength inputFramesSwitch') addressesScat
        expectedGath = filterZeroes (P.take simLength inputFramesPE') addressesGath
        expected = bimap Set.fromList Set.fromList (expectedScat, expectedGath)
        result = bimap Set.fromList Set.fromList $ P.unzip simOut

      footnote . fromString $ "Frames to PE: " <> showX (fmap fst simOut)
      footnote . fromString $ "Frames to Switch: " <> showX (fmap snd simOut)
      footnote . fromString $ "Frames from PE: " <> show (P.zip addressesGath $ P.take simLength inputFramesPE')
      footnote . fromString $ "Frames from Switch: " <> show (P.zip addressesScat $ P.take simLength inputFramesSwitch')

      expected === result

-- TODO: Remove instance once Clash.Prelude has it.
deriving instance Show (SNatLE a b)

-- | Generates a 'CalendarConfig' for the 'gatherUnitWB' or 'scatterUnitWB'
genCalendarConfig ::
  forall bytes addressWidth calEntry maxDepth .
  ( KnownNat bytes
  , 1 <= bytes
  , KnownNat maxDepth
  , 1 <= maxDepth
  , calEntry ~ Index maxDepth
  , KnownNat addressWidth) =>
  SNat maxDepth ->
  Gen (CalendarConfig bytes addressWidth calEntry)
genCalendarConfig sizeNat@(snatToNum -> dMax) = do
  dA <- Gen.enum 1 dMax
  dB <- Gen.enum 1 dMax
  case (TN.someNatVal dA, TN.someNatVal dB) of
    ( SomeNat (snatProxy -> depthA)
     ,SomeNat (snatProxy -> depthB)) -> do
        let
          regAddrBits = SNat @(NatRequiredBits (Regs calEntry (bytes * 8)))
          bsCalEntry = SNat @(BitSize calEntry)
        case
         ( isInBounds d1 depthA sizeNat
         , isInBounds d1 depthB sizeNat
         , compareSNat regAddrBits (SNat @addressWidth)
         , compareSNat d1 bsCalEntry) of
          (InBounds, InBounds, SNatLE, SNatLE)-> go depthA depthB
          (a,b,c,d) -> error $ "genCalendarConfig: calEntry constraints not satisfied: ("
           <> show a <> ", " <> show b <> ", " <> show c <> ", "  <> show d <>
           "), \n(depthA, depthB, maxDepth, calEntry bitsize) = (" <> show depthA <> ", "
           <> show depthB <> ", " <> show sizeNat <> ", " <> show bsCalEntry <> ")"
 where
    go :: forall depthA depthB .
      ( LessThan depthA maxDepth
      , LessThan depthB maxDepth
      , NatFitsInBits (Regs calEntry (bytes * 8)) addressWidth) =>
      SNat depthA ->
      SNat depthB ->
      Gen (CalendarConfig bytes addressWidth (Index maxDepth))
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
      calConfig <- forAll $ genCal p
      case calConfig of
        CalendarConfig _ calA@(length -> depth) _ -> do
          -- Amount of metacycles of input to generate
          metaCycles <- forAll $ Gen.enum 1 10
          let
            -- reset cycle + cycle delay, last metacycle's writes can be read in (metacycles + 1)
            simLength = 2 + (1+metaCycles) * depth
            inputGen = Gen.list (Range.singleton metaCycles)
            metaCycleNothing = P.replicate depth Nothing
            -- Generate at most depth `div` 2 elements to be written each metacycle since
            -- we need two cycles to read a written element.
            metaCycleGen = genFrameList (Range.singleton $ depth `div` 2)

          inputFrames <- forAll $ padToLength (simLength `div` depth + 1) metaCycleNothing
           <$> inputGen (padToLength depth Nothing <$> metaCycleGen)
          let
           topEntity (unbundle -> (wbIn, linkIn)) = fst $
            withClockResetEnable clockGen resetGen enableGen (scatterUnitWB @System @_ @32)
            (deepErrorX "scatterUnit initial elements undefined") calConfig
            (pure $ wishboneM2S SNat SNat) (pure False) linkIn wbIn

           wbReadOps = P.take simLength $ P.replicate depth idleM2S P.++  P.concat
            (padToLength depth idleM2S . P.concat . P.zipWith wbRead (toList calA) <$> inputFrames)

           topEntityInput = P.zip wbReadOps (P.concat inputFrames)
           simOut = simulateN simLength topEntity topEntityInput
          footnote . fromString $ "simOut: " <> showX simOut
          footnote . fromString $ "simIn: " <> showX wbReadOps
          footnote . fromString $ "cal: " <> showX calA
          wbDecoding simOut === P.take simLength (catMaybes (P.concat inputFrames))
 where
  genCal :: forall maxSize . 1 <= maxSize => SNat maxSize -> Gen (CalendarConfig 4 32 (Index maxSize))
  genCal SNat = genCalendarConfig @4 @32 (SNat @maxSize)
  padToLength l padElement g = P.take l (g P.++ P.repeat padElement)

-- | Check if the gather unit with wishbone interface loses no frames.
gatherUnitNoFrameLoss :: Property
gatherUnitNoFrameLoss = property $ do
  maxCalSize <- forAll $ Gen.enum 2 32
  case TN.someNatVal (maxCalSize - 1) of
    SomeNat (succSNat . snatProxy -> p) -> do
      calConfig <- forAll $ genCal p
      case calConfig of
        CalendarConfig _ calA@(length -> depth) _ -> do
          metaCycles <- forAll $ Gen.enum 1 10
          let
           simLength = 2 + (1+metaCycles) * depth
           inputGen = Gen.list (Range.singleton metaCycles)
           metaCycleNothing = P.replicate depth Nothing
           metaCycleGen = genFrameList (Range.singleton $ depth `div` 2)
          inputFrames <- forAll $ padToLength (simLength `div` depth + 1) metaCycleNothing
           <$> inputGen (padToLength depth Nothing <$> metaCycleGen)
          let
           topEntity wbIn = (\ (a, _ ,_) -> a) $
            withClockResetEnable clockGen resetGen enableGen (gatherUnitWB @System @_ @32)
            (deepErrorX "scatterUnit initial elements undefined") calConfig
            (pure $ wishboneM2S SNat SNat) (pure False) wbIn

           wbWriteOps = P.take simLength . P.concat $
            padToLength depth idleM2S . P.concat . P.zipWith wbWrite (toList calA) <$> inputFrames

           simOut = simulateN simLength topEntity wbWriteOps
           addressedFrames = P.zip (P.concat inputFrames) (cycle $ toList calA)
           writtenFrames = [if snd e /= 0 then fst e else Nothing | e <- addressedFrames]
           prePad = (P.replicate (1+depth) Nothing P.++)
           expectedOutput = P.take simLength (fromMaybe 1 <$> P.filter isJust writtenFrames)

          footnote . fromString $ "simOut: " <> showX simOut
          footnote . fromString $ "simIn: " <> showX wbWriteOps
          footnote . fromString $ "cal: " <> showX calA
          footnote . fromString $ "writtenFrames: " <> showX writtenFrames

          directedDecode (prePad writtenFrames) simOut === expectedOutput
 where
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
wbDecoding :: KnownNat bytes =>
  [WishboneS2M bytes]
  -> [BitVector ((8 * bytes) + (8 * bytes))]
wbDecoding (s2m0 : s2m1 : s2ms)
  | acknowledge s2m0 && acknowledge s2m1 = out : wbDecoding s2ms
  | otherwise = wbDecoding (s2m1 : s2ms)
 where
  out = readData s2m1 ++# readData s2m0
wbDecoding _ = []

-- | Tranform a read address with expected frame into a wishbone read operation for testing
-- the 'scatterUnitWB'. The second argument indicate wether or not a frame can be read from
-- that read address. The read operation reads data over 2 read cycles.
wbRead ::
  forall bytes addressWidth maxIndex a .
  ( KnownNat bytes
  , KnownNat addressWidth
  , KnownNat maxIndex
  , 1 <= maxIndex) =>
  Index maxIndex ->
  Maybe a ->
  [WishboneM2S bytes addressWidth]
wbRead readAddr (Just _) =
  [(wishboneM2S SNat (SNat @addressWidth))
    { addr = (`shiftL` 3) . resize $ pack readAddr
    , busCycle = True
    , strobe = True}
  ,
  (wishboneM2S SNat (SNat @addressWidth))
    { addr =  4 .|.  ((`shiftL` 3) . resize $ pack readAddr)
    , busCycle = True
    , strobe = True}
  ]
wbRead _ Nothing = []

-- | Transform a write address with frame to a wishbone write operation for testing the
-- 'gatherUnitWB'. The write operation writes the incoming bitvector over 2 write cycles.
wbWrite ::
  forall bytes addressWidth maxIndex .
  ( KnownNat bytes
  , KnownNat addressWidth
  , KnownNat maxIndex
  , 1 <= maxIndex) =>
  Index maxIndex ->
  Maybe (BitVector (bytes*2*8)) ->
  [WishboneM2S bytes addressWidth]
wbWrite writeAddr (Just frame) =
  [(wishboneM2S @bytes @addressWidth SNat SNat)
    { addr = (`shiftL` 3) . resize $ pack writeAddr
    , busSelect = maxBound
    , busCycle = True
    , strobe = True
    , writeEnable = True
    , writeData = lower}
    ,
  (wishboneM2S @bytes @addressWidth SNat SNat)
    { addr =  4 .|.  ((`shiftL` 3) . resize $ pack writeAddr)
    , busSelect = maxBound
    , busCycle = True
    , strobe = True
    , writeEnable = True
    , writeData = upper}
  ]
 where
  (upper, lower) = split frame
wbWrite _ Nothing = []
