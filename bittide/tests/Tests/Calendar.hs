-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0


{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=7 #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Tests.Calendar(calGroup, genCalendarConfig) where

import Clash.Prelude
import Clash.Hedgehog.Sized.Vector

import Bittide.Calendar
import Bittide.SharedTypes
import Tests.Shared

import Clash.Sized.Vector (unsafeFromList)
import Bittide.Extra.Wishbone
import Data.Constraint
import Data.Constraint.Nat.Extra
import Data.Proxy
import Data.String
import Data.Type.Equality ((:~:)(Refl))
import GHC.Natural
import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Clash.Sized.Vector as V
import qualified Data.Set as Set
import qualified GHC.TypeNats as TN
import qualified Prelude as P
import Clash.Hedgehog.Sized.Index (genIndex)

calGroup :: TestTree
calGroup = testGroup "Calendar group"
  [ testPropertyNamed "Reading the calendar." "readCalendar" readCalendar
  , testPropertyNamed "Writing and reading new calendars" "reconfigCalendar" reconfigCalendar
  , testPropertyNamed "Metacycle signal generation" "metaCycleIndication" metaCycleIndication
  , testPropertyNamed "Reading shadow buffer with wishbone" "readShadowCalendar" readShadowCalendar]

-- | A vector with a minimum size of 1 elements containing Bitvectors of arbitrary size.
-- This data type enables us to generate differently sized calendars that satisfy the constraints
-- imposed by the calendar component.
data BVCalendar addrW where
  BVCalendar ::
    ( KnownNat n
    , KnownNat addrW
    , KnownNat bits
    , 1 <= bits
    , NatFitsInBits n addrW
    , 1 <= NatRequiredBits n
    , NatFitsInBits (Regs (BitVector bits) addrW) addrW) =>
    -- | Amount of entries in the BitVector calendar minus 1.
    SNat n ->
    -- | Amount of bits per BitVector in the calendar.
    SNat bits ->
    -- | Vector of (n+1) entries containing BitVectors of size bits.
    Vec (n + 1) (BitVector bits) ->
    BVCalendar addrW

instance Show (BVCalendar addrW) where
  show (BVCalendar _ _ bvvec) = show bvvec

-- | Generates a configuration for 'Bittide.Calendar.calendar', with as first argument
-- the maximum depth of the stored calendar and as second argument a generator for the
-- calendar entries.
genCalendarConfig ::
  forall nBytes addrW calEntry .
  ( KnownNat nBytes
  , 1 <= nBytes
  , KnownNat (BitSize calEntry)
  , BitPack calEntry
  , NFDataX calEntry
  , Show calEntry
  , ShowX calEntry
  , KnownNat addrW) =>
  -- | Maximum amount of entries a calendar based on the returned configuration can hold per calendar.
  Natural ->
  -- | Generator for the entries in the shadow calendar and active calendar.
  Gen calEntry ->
  Gen (CalendarConfig nBytes addrW calEntry)
genCalendarConfig ms elemGen = do
  dA <- Gen.enum 1 ms
  dB <- Gen.enum 1 ms
  case (TN.someNatVal ms, TN.someNatVal dA, TN.someNatVal dB) of
    ( SomeNat (snatProxy -> maxSize)
     ,SomeNat (snatProxy -> depthA)
     ,SomeNat (snatProxy -> depthB)) -> do
        let
          regAddrBits = SNat @(NatRequiredBits (Regs calEntry (nBytes * 8)))
          bsCalEntry = SNat @(BitSize calEntry)
        case
         ( isInBounds d1 depthA maxSize
         , isInBounds d1 depthB maxSize
         , compareSNat regAddrBits (SNat @addrW)
         , compareSNat d1 bsCalEntry) of
          (InBounds, InBounds, SNatLE, SNatLE)-> go maxSize depthA depthB
          (a,b,c,d) -> error $ "genCalendarConfig: calEntry constraints not satisfied: ("
           <> show a <> ", " <> show b <> ", " <> show c <> ", "  <> show d <>
           "), \n(depthA, depthB, maxDepth, calEntry bitsize) = (" <> show depthA <> ", "
           <> show depthB <> ", " <> show maxSize <> ", " <> show bsCalEntry <> ")"
 where
    go ::
      forall maxDepth depthA depthB .
      ( LessThan depthA maxDepth
      , LessThan depthB maxDepth
      , NatFitsInBits (Regs calEntry (nBytes * 8)) addrW) =>
      SNat maxDepth ->
      SNat depthA ->
      SNat depthB ->
      Gen (CalendarConfig nBytes addrW calEntry)
    go dMax SNat SNat = do
      calActive <- genVec @_ @depthA elemGen
      calShadow <- genVec @_ @depthB elemGen
      return $ CalendarConfig dMax calActive calShadow

-- | Generates a 'BVCalendar' of a certain size and width for the stored BitVectors.
genBVCalendar :: Integer -> Integer -> Gen (BVCalendar 32)
genBVCalendar calSize bitWidth = do
  let
   calNat = TN.someNatVal (fromIntegral $ calSize - 1)
   bitNat = TN.someNatVal (fromIntegral bitWidth)
  case (calNat, bitNat) of
    (SomeNat size, SomeNat bits) -> go size bits
 where
  go ::
    forall calSize bitWidth .
    (KnownNat calSize, KnownNat bitWidth) =>
    Proxy calSize ->
    Proxy bitWidth->
    Gen (BVCalendar 32)
  go s b = do
    let
      calNatBits = clogBaseSNat d2 . succSNat $ snatProxy s
      requiredAddrWidth = SNat @(NatRequiredBits (Regs (BitVector bitWidth) 32))
    case
      ( compareSNat calNatBits d32
      , compareSNat d1 calNatBits
      , compareSNat requiredAddrWidth d32
      , compareSNat d1 (snatProxy b)) of
      (SNatLE, SNatLE, SNatLE, SNatLE) -> do
        cal <- Gen.list (Range.singleton $ fromIntegral calSize)
         $ Gen.integral @_ @(BitVector bitWidth) Range.constantBounded
        return (BVCalendar (snatProxy s) (snatProxy b) $ unsafeFromList cal)
      _ -> error $ "genIntCalendar: Constraints not satisfied: 1 <= " <> show calNatBits
       <> " <= 32, " <> show requiredAddrWidth <> " <= 32."

-- | This test checks if we can read the initialized calendars.
readCalendar :: Property
readCalendar = property $ do
  calSize <- forAll $ Gen.int $ Range.constant 2 31
  bitWidth <- forAll $ Gen.enum 1 1000
  bvCal <- forAll $ genBVCalendar (fromIntegral calSize) bitWidth
  simLength <- forAll $ Gen.enum (succ calSize) 100
  case bvCal of
    BVCalendar (succSNat -> calSize') SNat cal -> do
      let
        topEntity = (\(a,_,_) -> a) $ withClockResetEnable clockGen resetGen enableGen
          calendar calSize' cal cal $ pure (emptyWishboneM2S @4 @32)
        simOut = sampleN @System (fromIntegral simLength) topEntity
      footnote . fromString $ "simOut: " <> show simOut
      footnote . fromString $ "expected: " <> show (toList cal)

      Set.fromList simOut === Set.fromList (toList cal)

-- | This test checks if we can write to the shadowbuffer and read back the written
-- elements later.
reconfigCalendar :: Property
reconfigCalendar = property $ do
  calSize <- forAll $ Gen.int $ Range.constant 2 32
  bitWidth <- forAll $ Gen.enum 1 1000
  bvCal <- forAll $ genBVCalendar (fromIntegral calSize) bitWidth
  case bvCal of
    BVCalendar (succSNat -> calSize') _ cal -> do
      newEntries <- forAll . Gen.list (Range.singleton calSize) $ Gen.integral Range.constantBounded
      let
        configAddresses = cycle [0..indexOf calSize']
        writeOps = P.zip configAddresses newEntries
        swapAddr = bitWidth `divRU` 32 + 3
        swapCall = wbWriteOp (swapAddr, 0)
        wbWrites = wbNothingM2S @4 @32 : P.concatMap writeWithWishbone writeOps <> [swapCall]
        writeDuration = P.length wbWrites
        simLength = writeDuration + (2 * calSize)
        topEntity writePort = (\(a,_,_) -> a) $ withClockResetEnable clockGen
          resetGen enableGen calendar (succSNat calSize') cal cal writePort
        topEntityInput = P.take simLength $ wbWrites <> P.repeat wbNothingM2S
        simOut = simulateN @System simLength topEntity topEntityInput
      footnote . fromString $ "simOut: " <> show simOut
      footnote . fromString $ "expected: " <> show (toList cal <> newEntries)
      footnote . fromString $ "Write operations: " <> show wbWrites
      footnote . fromString $ "Write operations: " <> show writeOps
      Set.fromList simOut === Set.fromList (P.take simLength (toList cal <> newEntries))

-- | This test checks if we can write to the shadowbuffer and read back the written
-- elements later.
readShadowCalendar :: Property
readShadowCalendar = property $ do
  calSize <- forAll $ Gen.enum 10 32
  bitWidth <- forAll $ Gen.enum 1 1000
  calA <- forAll $ genBVCalendar calSize bitWidth
  calS <- forAll $ genBVCalendar calSize bitWidth
  case (calA, calS) of
    (BVCalendar snatA bwA calA', BVCalendar snatS bwS calS') ->
      case (sameNat (asProxy snatA) (asProxy snatS), sameNat (asProxy bwA) (asProxy bwS)) of
        (Just Refl, Just Refl) -> do
          let
            entryRegs = snatToInteger $ requiredRegs bwS d32
            readAddresses = [0..indexOf (succSNat snatS)]
            simLength = P.length wbReads + 1
            wbReads = P.concatMap (\ i -> wbReadEntry @4 @32 (fromIntegral i) entryRegs) readAddresses
            topEntity writePort = (\(_,_,wb) -> wb) $ withClockResetEnable clockGen resetGen enableGen
              calendar (succSNat snatS) calA' calS' writePort
            topEntityInput = P.take simLength $ wbReads <> P.repeat wbNothingM2S
            simOut = simulateN @System simLength topEntity topEntityInput
            wbOutEntries = directedWbDecoding topEntityInput simOut
          wbOutEntries === toList calS'
        _ -> error "readShadowCalendar: Calendar sizes or bitwidths do not match."

-- | This test checks if the metacycle signal (which indicates that the last entry of the
-- active calendar is present at the output), is correctly being generated.
metaCycleIndication :: Property
metaCycleIndication = property $ do
  calSize <- forAll $ Gen.enum 3 31
  bitWidth <- forAll $ Gen.enum 1 1000
  bvCal <- forAll $ genBVCalendar calSize bitWidth
  metaCycles <- forAll $ Gen.int $ Range.constant 2 5
  case bvCal of
    BVCalendar (succSNat -> (calSize' :: SNat calDepth)) bitWidth' cal -> do
      let genDepth = fromIntegral <$> genIndex @_ @calDepth (Range.constant 2 (fromIntegral $ pred calSize))
      newDepths <- forAll $ Gen.list (Range.singleton (metaCycles - 1)) genDepth
      let
        newDepthAddr = 2 + snatToInteger (requiredRegs bitWidth' d32)
        allDepths = (calSize - 1) : (fromIntegral <$> newDepths)
      simLength <- forAll $ fromIntegral <$> Gen.enum calSize (sum allDepths)
      let
        swapAddr = bitWidth `divRU` 32 + 3
        swapCall = wbWriteOp @4 @32 (swapAddr, 0)
        wbWrites = P.replicate (fromIntegral calSize - 3) wbNothingM2S <> P.concatMap writeAndSwitch newDepths
        writeAndSwitch d = wbWriteOp (newDepthAddr, fromIntegral d) : swapCall : P.replicate (d-1) wbNothingM2S
        topEntity writePort = (\(_,m,_) -> m) $ withClockResetEnable
          clockGen resetGen enableGen calendar calSize' cal cal writePort
        topEntityInput = wbWrites <> P.repeat wbNothingM2S
        simOut = simulateN @System simLength topEntity topEntityInput
        expectedOut = P.take simLength $ P.concatMap (\ (fromIntegral -> n) -> P.replicate n False <> [True]) allDepths
      footnote . fromString $ "Simulation:   " <> show simOut
      footnote . fromString $ "Expected:     " <> show expectedOut
      footnote . fromString $ "wishbone in:   " <> show (P.take simLength wbWrites)
      simOut === expectedOut

-- | Gets the index of element (n+1)
indexOf :: (KnownNat n) => SNat (n+1) -> Index (n+1)
indexOf = fromSNat . predSNat

-- | Interpret SNat as Proxy for use by 'sameNat'.
asProxy :: SNat n -> Proxy n
asProxy SNat = Proxy

-- | Get the amount of required registers for storing a BitVector bits in registers of regSize.
requiredRegs :: (1 <= regSize) => SNat bits -> SNat regSize-> SNat (Regs (BitVector bits) regSize)
requiredRegs SNat SNat = SNat

-- | idle 'Bittide.Extra.Wishbone.WishboneM2S' bus.
wbNothingM2S :: forall nBytes addrW . (KnownNat nBytes, KnownNat addrW) => WishboneM2S nBytes addrW
wbNothingM2S = (emptyWishboneM2S @nBytes @addrW)
 { addr = 0
 , writeData = 0
 , busSelect = 0}

-- | Write an entry to some address in 'Bittide.Calendar.calendar', this may require
-- multiple write operations.
writeWithWishbone ::
  forall nBytes addrW n entry .
  (KnownNat nBytes, 1 <= nBytes, KnownNat addrW, KnownNat n, Paddable entry) =>
  (Index n, entry) ->
  [WishboneM2S nBytes addrW]
writeWithWishbone (a, entry) =
  case getRegs entry of
    RegisterBank vec -> toList $ fmap wbWriteOp $ zip indicesI (vec :< fromIntegral a)

-- | Use both the wishbone M2S bus and S2M bus to decode the S2M bus operations into the
-- expected type a.
directedWbDecoding :: forall nBytes addrW a . (KnownNat nBytes, 1 <= nBytes, KnownNat addrW, Paddable a) =>
  [WishboneM2S nBytes addrW] ->
  [WishboneS2M nBytes] ->
  [a]
directedWbDecoding (wbM2S:m2sRest) (_:s2mRest) = out
 where
  active = strobe wbM2S && busCycle wbM2S
  foundBeginning = writeEnable wbM2S && active

  expectReadData :: (WishboneM2S nBytes addrW,WishboneS2M nBytes) -> Bool
  expectReadData (WishboneM2S{strobe, busCycle, writeEnable},_) =
    strobe && busCycle && not writeEnable

  entryList = fmap (readData . snd) $ takeWhile expectReadData . filterNoOps $ P.zip m2sRest s2mRest

  filterNoOps l = [(m2s,s2m)| (m2s,s2m) <- l, m2s /= wbNothingM2S]
  entry = case V.fromList $ P.reverse entryList of
    Just (vec :: Vec (Regs a (nBytes * 8)) (BitVector (nBytes * 8))) ->
        case timesDivRU @(nBytes * 8) @(BitSize a) of
          Dict ->
            paddedToData . bvAsPadded @(Regs a (nBytes * 8) * nBytes * 8) $ pack vec
    Nothing  -> error $ "directedWbDecoding: list to vector conversion failed: " <> show entryList <> "from " <> show (wbM2S:m2sRest)

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
  [WishboneM2S nBytes addrW]
wbReadEntry i dataRegs = addrWrite : wbNothingM2S : dataReads
 where
  addrWrite = (emptyWishboneM2S @nBytes @addrW)
    { addr      = fromIntegral $ dataRegs + 1
    , writeData = fromIntegral i
    , busSelect = maxBound
    , busCycle    = True
    , strobe      = True
    , writeEnable = True}
  dataReads = readReg <$> P.reverse [0..(dataRegs-1)]
  readReg n = (emptyWishboneM2S @nBytes @addrW)
    { addr = fromIntegral n
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
  (i, BitVector (nBytes * 8)) ->
  WishboneM2S nBytes addrW
wbWriteOp (i, bv) = (emptyWishboneM2S @nBytes @addrW)
  { addr        = fromIntegral i
  , writeData   = bv
  , busSelect   = maxBound
  , busCycle    = True
  , strobe      = True
  , writeEnable = True}
