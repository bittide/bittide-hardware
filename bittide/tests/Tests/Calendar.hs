-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0


{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Tests.Calendar(tests, genCalendarConfig, genValidEntry) where

import Clash.Prelude

import Clash.Hedgehog.Sized.Index (genIndex)
import Clash.Hedgehog.Sized.Unsigned
import Clash.Hedgehog.Sized.Vector
import Clash.Sized.Vector (unsafeFromList)
import Data.Proxy
import Data.String
import Data.Type.Equality ((:~:)(Refl))
import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Protocols.Wishbone
import Test.Tasty
import Test.Tasty.Hedgehog

import Bittide.Calendar
import Bittide.SharedTypes
import Tests.Shared

import qualified Clash.Sized.Vector as V
import qualified Data.Set as Set
import qualified GHC.TypeNats as TN
import qualified Prelude as P
import qualified Clash.Util.Interpolate as I

tests :: TestTree
tests = testGroup "Tests.Calendar"
  [ testPropertyNamed "Reading the calendar." "readCalendar" readCalendar
  , testPropertyNamed "Writing and reading new calendars" "reconfigCalendar" reconfigCalendar
  , testPropertyNamed "Reading shadow buffer with wishbone" "readShadowCalendar" readShadowCalendar
  , testPropertyNamed "Metacycle signal generation" "metaCycleIndication" metaCycleIndication]

-- | A vector with a minimum size of 1 elements containing Bitvectors of arbitrary size.
-- This data type enables us to generate differently sized calendars that satisfy the constraints
-- imposed by the calendar component.
data BVCalendar addrW where
  BVCalendar ::
    ( KnownNat addrW
    , KnownNat n, 2 <= n
    , KnownNat bits, 1 <= bits
    , KnownNat validityBits) =>
    -- | Amount of entries in the BitVector calendar
    SNat n ->
    -- | Amount of bits per BitVector in the calendar.
    SNat bits ->
    -- | Number of bits for entry validity.
    SNat validityBits ->
    -- | Vector of (n+1) entries containing BitVectors of size bits.
    Vec n (ValidEntry (BitVector bits) validityBits) ->
    BVCalendar addrW

instance Show (BVCalendar addrW) where
  show (BVCalendar _ _ _ bvvec) = show bvvec

-- | Generates a configuration for 'Bittide.Calendar.calendar', with as first argument
-- the maximum depth of the stored calendar and as second argument a generator for the
-- calendar entries.
genCalendarConfig ::
  forall nBytes addrW a validityBits .
  ( KnownNat nBytes , 1 <= nBytes
  , KnownNat addrW, 2 <= addrW
  , KnownNat (BitSize a)
  , KnownNat validityBits
  , BitPack a
  , NFDataX a
  , Show a
  , ShowX a) =>
  -- | Maximum amount of entries a calendar based on the returned configuration can hold per calendar.
  Natural ->
  -- | Generator for the entries in the shadow calendar and active calendar.
  Gen (ValidEntry a validityBits) ->
  Gen (CalendarConfig nBytes addrW a)
genCalendarConfig ms elemGen = do
  dA <- Gen.enum 1 ms
  dB <- Gen.enum 1 ms
  case (TN.someNatVal (ms - 2), TN.someNatVal dA, TN.someNatVal dB) of
    ( SomeNat (addSNat d2 . snatProxy -> maxSize)
     ,SomeNat (snatProxy -> depthA)
     ,SomeNat (snatProxy -> depthB)) -> do
        let
          regAddrBits = SNat
           @( NatRequiredBits (Regs (ValidEntry a validityBits) (nBytes * 8) + ExtraRegs))
          bsCalEntry = SNat @(BitSize a)
        case
         ( isInBounds d1 depthA maxSize
         , isInBounds d1 depthB maxSize
         , compareSNat regAddrBits (SNat @addrW)
         , compareSNat d1 bsCalEntry) of
          (InBounds, InBounds, SNatLE, SNatLE)-> go maxSize depthA depthB
          (a, b, c, d) -> error [I.i|
              genCalendarConfig: calEntry constraints not satisfied:

                a: #{a}
                b: #{b}
                c: #{c}
                d: #{d}

              ...
          |]
 where
    go ::
      forall maxDepth depthA depthB .
      ( LessThan depthA maxDepth
      , LessThan depthB maxDepth
      , 1 <= depthA
      , 1 <= depthB
      , 2 <= maxDepth
      , NatFitsInBits (Regs (ValidEntry a validityBits) (nBytes * 8) + ExtraRegs) addrW) =>
      SNat maxDepth ->
      SNat depthA ->
      SNat depthB ->
      Gen (CalendarConfig nBytes addrW a)
    go dMax SNat SNat = do
      calActive <- genVec @_ @depthA elemGen
      calShadow <- genVec @_ @depthB elemGen
      return $ CalendarConfig dMax calActive calShadow

genValidEntry :: SNat repetitionBits -> Gen a -> Gen (ValidEntry a repetitionBits)
genValidEntry SNat genA = (\veEntry veRepeat -> ValidEntry{veEntry, veRepeat})
  <$> genA
  <*> genUnsigned Range.linearBounded

-- | Generates a 'BVCalendar' of a certain size and width for the stored BitVectors.
genBVCalendar :: Integer -> Integer -> Integer -> Gen (BVCalendar 32)
genBVCalendar calSize bitWidth validityBits = do
  let
   calNat = TN.someNatVal (fromIntegral $ calSize - 2)
   bitNat = TN.someNatVal (fromIntegral bitWidth)
   valNat = TN.someNatVal (fromIntegral validityBits)
  case (calNat, bitNat, valNat) of
    (SomeNat size, SomeNat bits, SomeNat validity) ->
      go (addSNat d2 $ snatProxy size) (snatProxy bits) (snatProxy validity)
 where
  go ::
    forall calSize bitWidth validityBits .
    (KnownNat calSize, 2 <= calSize, KnownNat bitWidth) =>
    SNat calSize ->
    SNat bitWidth->
    SNat validityBits ->
    Gen (BVCalendar 32)
  go s b v@SNat = do
    let
      calNatBits = clogBaseSNat d2 s
    case
      ( compareSNat calNatBits d32
      , compareSNat d1 calNatBits
      , compareSNat d1 b) of
      (SNatLE, SNatLE, SNatLE) -> do
        cal <- Gen.list (Range.singleton $ fromIntegral calSize) $
          genValidEntry (SNat @validityBits)
          (genDefinedBitVector @bitWidth)
        return (BVCalendar s b v $ unsafeFromList cal)
      _ -> error $
        "genIntCalendar: Constraints not satisfied: 1 <= " <> show calNatBits <> " <= 32."

-- | This test checks if we can read the initialized calendars.
readCalendar :: Property
readCalendar = property $ do
  calSize <- forAll $ Gen.int $ Range.constant 2 31
  bitWidth <- forAll $ Gen.enum 1 100
  validityBits <- forAll $ Gen.enum 0 2
  bvCal <- forAll $ genBVCalendar (fromIntegral calSize) bitWidth validityBits
  case bvCal of
    BVCalendar calSize' SNat SNat cal -> do
      let
        -- 1 to compensate for reset, length for 1 cycle per element, sum of snds for
        -- additional validity delays.
        simLength = 1 + length cal + sum (fmap (fromIntegral . veRepeat) cal)
        topEntity = (\(a,_,_) -> a) $ withClockResetEnable clockGen resetGen enableGen
          calendarWbSpecVal calSize' cal cal $ pure (emptyWishboneM2S @32 @(BitVector 32))
        simOut = sampleN @System (fromIntegral simLength) topEntity
        expected = toList $ fmap veEntry cal
      footnote . fromString $ "simOut: " <> show simOut
      footnote . fromString $ "expected: " <> show expected

      Set.fromList simOut === Set.fromList expected

-- | This test checks if we can write to the shadowbuffer and read back the written
-- elements later.
reconfigCalendar :: Property
reconfigCalendar = property $ do
  calSize <- forAll $ Gen.int $ Range.constant 2 32
  bitWidth <- forAll $ Gen.enum 1 100
  validityBits <- forAll $ Gen.enum 0 2
  bvCal <- forAll $ genBVCalendar (fromIntegral calSize) bitWidth validityBits
  case bvCal of
    BVCalendar calSize' _ (SNat :: SNat validityBits) cal -> do
      newEntries <- forAll . Gen.list (Range.singleton calSize) $
        genValidEntry (SNat @validityBits) genDefinedBitVector
      let
        (entries0, delays0) = unzip $ fmap (\e -> (veEntry e, veRepeat e)) cal
        (entries1, delays1) = P.unzip $ fmap (\e -> (veEntry e, veRepeat e)) newEntries
        cal0Duration = calSize + sum (fmap fromIntegral delays0)
        cal1Duration = calSize + sum (fmap fromIntegral delays1)
        writeOps = P.zip (cycle [0.. indexOf calSize']) newEntries
        swapCall = let a = (bitWidth + validityBits) `divRU` 32 + 3 in wbWriteOp (a, 0)
        wbWrites = wbNothingM2S @4 @32 : P.concatMap writeWithWishbone writeOps <> [swapCall]
        -- Arming has one cycle delay,
        writeDuration = 1 + P.length wbWrites
        -- It may take multiple metacycles to write the new calendar.
        simLength = cal0Duration * (writeDuration `divRU` cal0Duration) + cal1Duration
        topEntity writePort = (\(a,_,_) -> a) $ withClockResetEnable clockGen
          resetGen enableGen calendar calSize' cal cal writePort
        topEntityInput = P.take simLength $ wbWrites <> P.repeat wbNothingM2S
        simOut = simulateN @System simLength topEntity topEntityInput
        expected = P.take simLength $ toList entries0 <> entries1
      footnote . fromString $ "simOut: " <> show simOut
      footnote . fromString $ "expected: " <> show expected
      footnote . fromString $ "Write operations: " <> show wbWrites
      footnote . fromString $ "Write operations: " <> show writeOps
      Set.fromList simOut === Set.fromList expected

-- | This test checks if we can write to the shadowbuffer and read back the written
-- elements later.
readShadowCalendar :: Property
readShadowCalendar = property $ do
  calSize <- forAll $ Gen.enum 2 32
  bitWidth <- forAll $ Gen.enum 1 100
  validityBits <- forAll $ Gen.enum 0 2
  calA <- forAll $ genBVCalendar calSize bitWidth validityBits
  calS <- forAll $ genBVCalendar calSize bitWidth validityBits
  case (calA, calS) of
    (BVCalendar snatA bwA valA calA', BVCalendar snatS bwS valS calS') ->
      case
        ( sameNat (asProxy snatA) (asProxy snatS)
        , sameNat (asProxy bwA) (asProxy bwS)
        , sameNat (asProxy valA) (asProxy valS)) of
          (Just Refl, Just Refl, Just Refl) -> do
            let
              entryRegs = snatToInteger $ requiredRegs (bwS `addSNat` valS) d32
              readAddresses = fmap fromIntegral [0.. indexOf snatS]
              simLength = P.length wbReads + 1
              wbReads = P.concatMap (\ i -> wbReadEntry @4 @32 i entryRegs) readAddresses
              topEntity writePort = (\(_,_,wb) -> wb) $
                withClockResetEnable clockGen resetGen enableGen
                calendar (addSNat d2 snatS) calA' calS' writePort
              topEntityInput = P.take simLength $ wbReads <> P.repeat wbNothingM2S
              simOut = simulateN @System simLength topEntity topEntityInput
              wbOutEntries = directedWbDecoding topEntityInput simOut
            wbOutEntries === toList calS'
          _ -> error "readShadowCalendar: Calendar sizes or bitwidths do not match."

-- | This test checks if the metacycle signal (which indicates that the last entry of the
-- active calendar is present at the output), is correctly being generated.
metaCycleIndication :: Property
metaCycleIndication = property $ do
  calSize <- forAll $ Gen.enum 3 4
  bitWidth <- forAll $ Gen.enum 1 100
  validityBits <- forAll $ Gen.enum 0 1
  bvCal <- forAll $ genBVCalendar calSize bitWidth validityBits
  metaCycles <- forAll $ Gen.int $ Range.constant 2 5
  case bvCal of
    BVCalendar (calSize' :: SNat calDepth) _ _ cal -> do
      let
        genDepth = fromIntegral <$> genIndex @_ @calDepth
          (Range.constant 2 (fromIntegral $ pred calSize))
      newDepths <- forAll $ Gen.list (Range.singleton (metaCycles - 1)) genDepth
      let
        reqRegs = (bitWidth + validityBits) `divRU` 32
        newDepthAddr = reqRegs + 2
        swapAddr = reqRegs + 3
        allDepths = (fromIntegral calSize - 1) : newDepths
        delayPerDepth = tail $ scanl (\a b -> a + fromIntegral (veRepeat b)) 0 cal
        allDurations = fmap (\d -> d + delayPerDepth !! d) allDepths
      simLength <- forAll $ fromIntegral <$> Gen.enum 0 (sum allDurations + sum newDepths)
      let
        swapCall = wbWriteOp @4 @32 (swapAddr, 0)
        wbWrites = P.drop 3 $ P.concatMap writeAndSwitch (P.zip allDepths allDurations)
        writeAndSwitch (dep, dur) = P.take (succ dur) $
          [wbWriteOp (newDepthAddr, fromIntegral dep),swapCall] <> P.repeat wbNothingM2S
        topEntity writePort = (\(_,m,_) -> m) $ withClockResetEnable
          clockGen resetGen enableGen calendarWbSpecVal calSize' cal cal writePort
        topEntityInput = wbWrites <> P.repeat wbNothingM2S
        simOut = simulateN @System simLength topEntity topEntityInput
        expectedOut = P.take simLength $
          P.concatMap (\ (fromIntegral -> n) -> P.replicate n False <> [True])
          (repeatLast allDurations)
      footnote . fromString $ "Simulation:   " <> show simOut
      footnote . fromString $ "Expected:     " <> show expectedOut
      footnote . fromString $ "All Depths:     " <> show allDurations
      footnote . fromString $ "wishbone in:   " <> show (P.take simLength wbWrites)
      simOut === expectedOut


repeatLast :: [a] -> [a]
repeatLast [] = []
repeatLast [l] = P.repeat l
repeatLast (l:ist) = l : repeatLast ist

-- | Gets the index of element (n+1)
indexOf :: forall n . (KnownNat n, 1 <= n) => SNat n -> Index n
indexOf = leToPlus @1 @n (fromSNat . predSNat)

-- | Interpret SNat as Proxy for use by 'sameNat'.
asProxy :: SNat n -> Proxy n
asProxy SNat = Proxy

-- | Get the amount of required registers for storing a BitVector bits in registers of regSize.
requiredRegs
  :: (1 <= regSize)
  => SNat bits
  -> SNat regSize
  -> SNat (Regs (BitVector bits) regSize)
requiredRegs SNat SNat = SNat

-- | idle 'Protocols.Wishbone.WishboneM2S' bus.
wbNothingM2S ::
  forall nBytes addrW.
  (KnownNat nBytes, KnownNat addrW) =>
  WishboneM2S addrW nBytes (Bytes nBytes)
wbNothingM2S =
  (emptyWishboneM2S @addrW @(Bytes nBytes))
    { addr = 0,
      writeData = 0,
      busSelect = 0
    }

-- | Write an entry to some address in 'Bittide.Calendar.calendar', this may require
-- multiple write operations.
writeWithWishbone ::
  forall nBytes addrW n entry .
  (KnownNat nBytes, 1 <= nBytes, KnownNat addrW, KnownNat n, Paddable entry) =>
  (Index n, entry) ->
  [WishboneM2S addrW nBytes (Bytes nBytes)]
writeWithWishbone (a, entry) =
  case getRegsLe entry of
    RegisterBank vec -> toList $ fmap wbWriteOp $ zip indicesI (vec :< fromIntegral a)

-- | Use both the wishbone M2S bus and S2M bus to decode the S2M bus operations into the
-- expected type a.
directedWbDecoding
  :: forall nBytes addrW a
  . (KnownNat nBytes
   , 1 <= nBytes
   , KnownNat addrW
   , Paddable a)
  => [WishboneM2S addrW nBytes (Bytes nBytes)]
  -> [WishboneS2M (Bytes nBytes)]
  -> [a]
directedWbDecoding (wbM2S:m2sRest) (_:s2mRest) = out
 where
  active = strobe wbM2S && busCycle wbM2S
  foundBeginning = writeEnable wbM2S && active

  expectReadData ::
    ( WishboneM2S addrW nBytes (Bytes nBytes)
    , WishboneS2M (Bytes nBytes) ) ->
    Bool
  expectReadData (WishboneM2S{strobe, busCycle, writeEnable},_) =
    strobe && busCycle && not writeEnable

  entryList =
    fmap (readData . snd)
    $ takeWhile expectReadData . filterNoOps
    $ P.zip m2sRest s2mRest

  filterNoOps l = [(m2s,s2m)| (m2s,s2m) <- l, m2s /= wbNothingM2S]
  entry = case V.fromList $ P.reverse entryList of
    Just (vec :: Vec (Regs a (nBytes * 8)) (Bytes nBytes)) -> getDataLe (RegisterBank vec)
    Nothing  ->
      error $
        "directedWbDecoding: list to vector conversion failed: "
        <> show entryList <> "from " <> show (wbM2S:m2sRest)

  consumedReads = P.length entryList
  remainingM2S = P.drop consumedReads m2sRest
  remainingS2M = P.drop consumedReads s2mRest

  out | foundBeginning = entry : directedWbDecoding remainingM2S remainingS2M
      | otherwise = directedWbDecoding m2sRest s2mRest

directedWbDecoding _ _ = []

-- | Returns the wishbone M2S bus inputs required to read a calendar entry from
-- 'Bittide.Calendar.calendar'. It first writes the entry's address to the read register,
-- then adds the read operations.
wbReadEntry ::
  forall nBytes addrW i .
  (KnownNat nBytes, KnownNat addrW, Integral i) =>
  i ->
  i ->
  [WishboneM2S addrW nBytes (Bytes nBytes)]
wbReadEntry i dataRegs = addrWrite : wbNothingM2S : dataReads
 where
  addrWrite = (emptyWishboneM2S @addrW @(Bytes nBytes))
    { addr      = 4 * fromIntegral (dataRegs + 1)
    , writeData = fromIntegral i
    , busSelect = maxBound
    , busCycle    = True
    , strobe      = True
    , writeEnable = True}
  dataReads = readReg <$> P.reverse [0..(dataRegs-1)]
  readReg n = (emptyWishboneM2S @addrW @(Bytes nBytes))
    { addr = 4 * fromIntegral n
    , writeData = 0
    , busSelect = maxBound
    , busCycle = True
    , strobe = True
    , writeEnable = False
    }

-- | Transform a target address i and a bitvector to a Wishbone write operation that writes
-- the bitvector to address i.
wbWriteOp ::
  forall nBytes addrW i .
  (KnownNat nBytes, KnownNat addrW, Integral i) =>
  (i, Bytes nBytes) ->
  WishboneM2S addrW nBytes (Bytes nBytes)
wbWriteOp (i, bv) =
  (emptyWishboneM2S @addrW @(Bytes nBytes))
    { addr        = 4 * fromIntegral i
    , writeData   = bv
    , busSelect   = maxBound
    , busCycle    = True
    , strobe      = True
    , writeEnable = True}

-- | Version of 'Bittide.Calendar.calendar' which performs Wishbone spec validation
calendarWbSpecVal ::
  forall dom nBytes addrW maxCalDepth a validityBits bootstrapSizeA bootstrapSizeB .
  ( HiddenClockResetEnable dom
  , KnownNat addrW, 2 <= addrW
  , KnownNat bootstrapSizeA, 1 <= bootstrapSizeA
  , KnownNat bootstrapSizeB, 1 <= bootstrapSizeB
  , KnownNat nBytes, 1 <= nBytes
  , KnownNat validityBits
  , 2 <= maxCalDepth
  , LessThan bootstrapSizeA maxCalDepth
  , LessThan bootstrapSizeB maxCalDepth
  , Paddable a
  , ShowX a
  , Show a) =>
  SNat maxCalDepth ->
  -- ^ The maximum amount of entries that can be stored in the individual calendars.
  Calendar bootstrapSizeA a validityBits ->
  -- ^ Bootstrap calendar for the active buffer.
  Calendar bootstrapSizeB a validityBits ->
  -- ^ Bootstrap calendar for the shadow buffer.
  Signal dom (WishboneM2S addrW nBytes (Bytes nBytes)) ->
  -- ^ Incoming wishbone interface
  (Signal dom a, Signal dom Bool, Signal dom (WishboneS2M (Bytes nBytes)))
  -- ^ Currently active entry, Metacycle indicator and outgoing wishbone interface.
calendarWbSpecVal mDepth bootstrapActive bootstrapShadow m2s0 =
  (active, metaIndicator, s2m1)
  where
    (active, metaIndicator, s2m0) =
      calendar @dom @nBytes @addrW
        mDepth
        bootstrapActive
        bootstrapShadow
        m2s1
    (m2s1, s2m1) = validateWb m2s0 s2m0
