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
import Clash.Sized.Vector (unsafeFromList)
import Contranomy.Wishbone
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

calGroup :: TestTree
calGroup = testGroup "Calendar group"
  [ testPropertyNamed "Reading the calendar." "readCalendar" readCalendar
  , testPropertyNamed "Writing and reading new calendars" "reconfigCalendar" reconfigCalendar
  , testPropertyNamed "Metacycle signal generation" "metaCycleIndication" metaCycleIndication
  , testPropertyNamed "Reading shadow buffer with wishbone" "readShadowCalendar" readShadowCalendar]

-- | A vector with a minimum size of 1 elements containing Bitvectors of arbitrary size.
-- This data type enables us to generate differently sized calendars that satisfy the constraints
-- imposed by the calendar component.
data BVCalendar addressWidth where
  BVCalendar ::
    ( KnownNat n
    , KnownNat addressWidth
    , KnownNat bits
    , 1 <= bits
    , NatFitsInBits n addressWidth
    , 1 <= NatRequiredBits n
    , NatFitsInBits (Regs (BitVector bits) addressWidth) addressWidth) =>
    -- | Amount of entries in the BitVector calendar minus 1.
    SNat n ->
    -- | Amount of bits per BitVector in the calendar.
    SNat bits ->
    -- | Vector of (n+1) entries containing BitVectors of size bits.
    Vec (n + 1) (BitVector bits) ->
    BVCalendar addressWidth

data CalendarTestConfig bytes addressWidth where
  CalendarTestConfig ::
    ( KnownNat bytes
    , KnownNat addressWidth
    , Paddable calEntry
    , Show calEntry
    , ShowX calEntry
    , NatFitsInBits (TypeRequiredRegisters calEntry (bytes * 8)) addressWidth
    ) =>
    SNat maxCalDepth ->
    CalendarConfig bytes addressWidth calEntry ->
    CalendarTestConfig bytes addressWidth

genCalendarTestConfig ::
  forall bytes addressWidth calEntry .
  ( KnownNat bytes
  , KnownNat addressWidth
  , Paddable calEntry
  , Show calEntry
  , ShowX calEntry
  , NatFitsInBits (TypeRequiredRegisters calEntry (bytes * 8)) addressWidth) =>
  Natural ->
  Gen calEntry ->
  Gen (CalendarTestConfig bytes addressWidth)
genCalendarTestConfig maxSize elemGen = do
  depthA <- Gen.enum 0 maxSize
  depthB <- Gen.enum 0 maxSize
  case (TN.someNatVal maxSize, TN.someNatVal depthA, TN.someNatVal depthB) of
    (SomeNat a, SomeNat b, SomeNat c) -> do
        let
          a' = snatProxy a
          b' = snatProxy b
          c' = snatProxy c
        case (compareSNat b' a', compareSNat c' a') of
          (SNatLE, SNatLE) -> go a' b' c'
          _ -> error " "
 where
    go :: forall maxDepth depthA depthB . (LessThan depthA maxDepth, LessThan depthB maxDepth) => SNat maxDepth -> SNat depthA -> SNat depthB -> Gen (CalendarTestConfig bytes addressWidth)
    go dMax SNat SNat = do
      calActive <- genVec @_ @depthA elemGen
      calShadow <- genVec @_ @depthB elemGen
      return $ CalendarTestConfig dMax (CalendarConfig dMax calActive calShadow)


instance Show (BVCalendar addressWidth) where
  show (BVCalendar _ _ bvvec) = show bvvec

-- TODO: Remove this show instance after issue (https://github.com/clash-lang/clash-compiler/issues/2190) has been fixed.
deriving instance Show (SNatLE a b)

data IsInBounds a b c where
  InBounds :: (a <= b, b <= c) => IsInBounds a b c
  NotInBounds :: IsInBounds a b c

deriving instance Show (IsInBounds a b c)

-- | Returns 'InBounds' if a <= b <= c, otherwise returns 'NotInBounds'.
isInBounds :: SNat a -> SNat b -> SNat c -> IsInBounds a b c
isInBounds a b c = case (compareSNat a b, compareSNat b c) of
  (SNatLE, SNatLE) -> InBounds
  _ -> NotInBounds

-- | Generates a configuration for 'Bittide.Calendar.calendarWB', with as first argument
-- the maximum depth of the stored calendar and as second argument a generator for the
-- calendar entries.
genCalendarConfig ::
  forall bytes addressWidth calEntry .
  ( KnownNat bytes
  , 1 <= bytes
  , KnownNat (BitSize calEntry)
  , BitPack calEntry
  , NFDataX calEntry
  , Show calEntry
  , ShowX calEntry
  , KnownNat addressWidth) =>
  -- | Maximum amount of entries a calendar based on the returned configuration can hold per calendar.
  Natural ->
  -- | Generator for the entries in the shadow calendar and active calendar.
  Gen calEntry ->
  Gen (CalendarConfig bytes addressWidth calEntry)
genCalendarConfig ms elemGen = do
  dA <- Gen.enum 1 ms
  dB <- Gen.enum 1 ms
  case (TN.someNatVal ms, TN.someNatVal dA, TN.someNatVal dB) of
    ( SomeNat (snatProxy -> maxSize)
     ,SomeNat (snatProxy -> depthA)
     ,SomeNat (snatProxy -> depthB)) -> do
        let
          regAddrBits = SNat @(NatRequiredBits (Regs calEntry (bytes * 8)))
          bsCalEntry = SNat @(BitSize calEntry)
        case
         ( isInBounds d1 depthA maxSize
         , isInBounds d1 depthB maxSize
         , compareSNat regAddrBits (SNat @addressWidth)
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
      , NatFitsInBits (Regs calEntry (bytes * 8)) addressWidth) =>
      SNat maxDepth ->
      SNat depthA ->
      SNat depthB ->
      Gen (CalendarConfig bytes addressWidth calEntry)
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
  calSize <- forAll $ Gen.enum 2 31
  bitWidth <- forAll $ Gen.enum 1 1000
  bvCal <- forAll $ genBVCalendar calSize bitWidth
  simLength <- forAll $ Gen.enum (fromIntegral calSize) 100
  switchSignal <- forAll $ Gen.list (Range.singleton simLength) Gen.bool
  case bvCal of
    BVCalendar (succSNat -> calSize') SNat cal -> do
      let
        topEntity switch = (\(a,_,_) -> a) $ withClockResetEnable clockGen resetGen enableGen
          calendarWB calSize' cal cal switch (pure (wishboneM2S (SNat @4) (SNat @32)))
        simOut = simulateN @System (fromIntegral simLength) topEntity switchSignal
      footnote . fromString $ "simOut: " <> show simOut
      footnote . fromString $ "expected: " <> show (toList cal)

      Set.fromList simOut === Set.fromList (toList cal)

-- | This test checks if we can write to the shadowbuffer and read back the written
-- elements later.
reconfigCalendar :: Property
reconfigCalendar = property $ do
  calSize <- forAll $ Gen.enum 2 32
  bitWidth <- forAll $ Gen.enum 1 1000
  bvCal <- forAll $ genBVCalendar calSize bitWidth
  case bvCal of
    BVCalendar (succSNat -> calSize') _ cal -> do
      newEntries <- forAll . Gen.list (Range.singleton $ fromIntegral calSize) $ Gen.integral Range.constantBounded
      let
        configAddresses = cycle [0..indexOf calSize']
        writeOps = P.zip configAddresses newEntries
        writeDuration = P.length wbWrites
        simLength = writeDuration + fromIntegral calSize + 10
        switchList = P.replicate (writeDuration + 1) False <> (True : P.repeat False)
        wbWrites = wbNothingM2S @4 @32 : P.concatMap writeWithWishbone writeOps
        topEntity (unbundle -> (switch, writePort)) = (\(a,_,_) -> a) $ withClockResetEnable clockGen
          resetGen enableGen calendarWB (succSNat calSize') cal cal switch writePort
        topEntityInput = P.take simLength $ P.zip switchList $ wbWrites <> P.repeat wbNothingM2S
        simOut = simulateN @System simLength topEntity topEntityInput
      footnote . fromString $ "simOut: " <> show simOut
      footnote . fromString $ "expected: " <> show (toList cal <> newEntries)
      footnote . fromString $ "Write operations: " <> show wbWrites
      footnote . fromString $ "Write operations: " <> show writeOps
      footnote . fromString $ "switchList: " <> show (P.take simLength switchList)
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
              calendarWB (succSNat snatS) calA' calS' (pure False) writePort
            topEntityInput = P.take simLength $ wbReads <> P.repeat wbNothingM2S
            simOut = simulateN @System simLength topEntity topEntityInput
            wbOutEntries = directedWBDecoding topEntityInput simOut
          wbOutEntries === toList calS'
        _ -> error "readShadowCalendar: Calendar sizes or bitwidths do not match."

-- | This test checks if the metacycle signal (which indicates that the first entry of a
-- new calendar is present at the output), is correctly being generated.
metaCycleIndication :: Property
metaCycleIndication = property $ do
  calSize <- forAll $ Gen.enum 2 31
  bitWidth <- forAll $ Gen.enum 1 1000
  bvCal <- forAll $ genBVCalendar calSize bitWidth
  newDepthRange <- forAll $ Gen.integral $  Range.constant 2 calSize
  case bvCal of
    BVCalendar (succSNat -> calSize') bitWidth' cal -> do
      simLength <- forAll $ Gen.enum 10 100
      newDepths <- forAll $ Gen.list (Range.singleton $ fromIntegral newDepthRange) $ Gen.enum 1 (calSize - 1)
      let
        newDepthAddr = 2 + snatToInteger (requiredRegs bitWidth' d32)
        allDepths = calSize : newDepths <> cycle (takeLast 2 newDepths)
        wbWrites = wbWriteOp @4 @32 <$> P.zip (P.repeat newDepthAddr) (fromIntegral <$> newDepths)
        switchSignal = P.concatMap (\ (fromIntegral -> n) -> P.replicate n False <> [True]) allDepths
        wbIn = P.concatMap (\(fromIntegral -> i, wb) -> wb : P.replicate i wbNothingM2S) $ P.zip (1 + calSize : newDepths) wbWrites
        topEntity (unbundle -> (switch, writePort)) = (\(_,m,_) -> m) $ withClockResetEnable
          clockGen resetGen enableGen calendarWB calSize' cal cal switch writePort
        topEntityInput = P.zip switchSignal (wbIn <> P.repeat wbNothingM2S)
        simOut = simulateN @System simLength topEntity topEntityInput
        expectedOut = P.take simLength $ True : P.tail switchSignal
      footnote . fromString $ "Simulation:   " <> show simOut
      footnote . fromString $ "Expected:     " <> show expectedOut
      footnote . fromString $ "shadowSwitch: " <> show (P.take simLength switchSignal)
      footnote . fromString $ "wishbone in:   " <> show (P.take simLength wbIn)
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

-- | Get the last n elements of a list.
takeLast :: Int -> [a] -> [a]
takeLast n l = P.drop (P.length l - n) l

-- | idle 'Contranomy.Wishbone.WishboneM2S' bus.
wbNothingM2S :: forall bytes aw . (KnownNat bytes, KnownNat aw) => WishboneM2S bytes aw
wbNothingM2S = (wishboneM2S (SNat @bytes) (SNat @aw))
 { addr = 0
 , writeData = 0
 , busSelect = 0}

-- | Write an entry to some address in 'Bittide.Calendar.calendarWB', this may require
-- multiple write operations.
writeWithWishbone ::
  forall bytes aw n entry .
  (KnownNat bytes, 1 <= bytes, KnownNat aw, KnownNat n, Paddable entry) =>
  (Index n, entry) ->
  [WishboneM2S bytes aw]
writeWithWishbone (a, entry) =
  case getRegs entry of
    RegisterBank vec -> toList $ fmap wbWriteOp $ zip indicesI (vec :< fromIntegral a)
 where
  getRegs :: entry  -> RegisterBank (bytes * 8) entry
  getRegs = paddedToRegisters . Padded

-- | Use both the wishbone M2S bus and S2M bus to decode the S2M bus operations into the
-- expected type a.
directedWBDecoding :: forall bytes aw a . (KnownNat bytes, 1 <= bytes, KnownNat aw, Paddable a) =>
  [WishboneM2S bytes aw] ->
  [WishboneS2M bytes] ->
  [a]
directedWBDecoding (wbM2S:m2sRest) (_:s2mRest) = out
 where
  active = strobe wbM2S && busCycle wbM2S
  foundBeginning = writeEnable wbM2S && active

  expectReadData :: (WishboneM2S bytes aw,WishboneS2M bytes) -> Bool
  expectReadData (WishboneM2S{strobe, busCycle, writeEnable},_) =
    strobe && busCycle && not writeEnable

  entryList = fmap (readData . snd) $ takeWhile expectReadData . filterNoOps $ P.zip m2sRest s2mRest

  filterNoOps l = [(m2s,s2m)| (m2s,s2m) <- l, m2s /= wbNothingM2S]
  entry = case V.fromList $ P.reverse entryList of
    Just (vec :: Vec (Regs a (bytes * 8)) (BitVector (bytes * 8))) ->
        case timesDivRU @(bytes * 8) @(BitSize a) of
          Dict ->
            paddedToData . bvAsPadded @(Regs a (bytes * 8) * bytes * 8) $ pack vec
    Nothing  -> error $ "directedWBDecoding: list to vector conversion failed: " <> show entryList <> "from " <> show (wbM2S:m2sRest)

  consumedReads = P.length entryList
  remainingM2S = P.drop consumedReads m2sRest
  remainingS2M = P.drop consumedReads s2mRest

  out | foundBeginning = entry : directedWBDecoding remainingM2S remainingS2M
      | otherwise = directedWBDecoding m2sRest s2mRest

directedWBDecoding _ _ = []

-- | Returns the wishbone M2S bus inputs required to read a calendar entry from
-- 'Bittide.Calendar.calendarWB'. It first writes the entry's address to the read register,
-- then adds the read operations.
wbReadEntry ::
  forall bytes aw i .
  (KnownNat bytes, KnownNat aw, Integral i) =>
  i ->
  i ->
  [WishboneM2S bytes aw]
wbReadEntry i dataRegs = addrWrite : wbNothingM2S : dataReads
 where
  addrWrite = (wishboneM2S (SNat @bytes) (SNat @aw))
    { addr      = fromIntegral $ dataRegs + 1
    , writeData = fromIntegral i
    , busSelect = maxBound
    , busCycle    = True
    , strobe      = True
    , writeEnable = True}
  dataReads = readReg <$> P.reverse [0..(dataRegs-1)]
  readReg n = (wishboneM2S (SNat @bytes) (SNat @aw))
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
  forall bytes addressWidth i .
  (KnownNat bytes, KnownNat addressWidth, Integral i) =>
  (i, BitVector (bytes * 8)) ->
  WishboneM2S bytes addressWidth
wbWriteOp (i, bv) = (wishboneM2S (SNat @bytes) (SNat @addressWidth))
  { addr        = fromIntegral i
  , writeData   = bv
  , busSelect   = maxBound
  , busCycle    = True
  , strobe      = True
  , writeEnable = True}
