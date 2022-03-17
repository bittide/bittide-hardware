{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=7 #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Tests.Calendar(calGroup) where

import Clash.Prelude

import Bittide.Calendar
import Bittide.SharedTypes
import Clash.Sized.Vector ( unsafeFromList )
import Contranomy.Wishbone
import Data.Proxy
import Data.String
import Data.Type.Equality ((:~:)(Refl))
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

-- | The BVcalendar is a vector with a minimum size of 1 elements containing Bitvectors of arbitrary size.
-- This data type enables us to generate differently sized calendars that satisfy the constraints
-- imposed by the calendar component.
data BVCalendar addressWidth where
  BVCalendar ::
    ( KnownNat n
    , KnownNat addressWidth
    , AtLeastOne bits
    , NatFitsInBits n addressWidth
    , 1 <= NatRequiredBits n
    , NatFitsInBits (TypeRequiredRegisters (BitVector bits) addressWidth) addressWidth) =>
    SNat n ->
    SNat bits ->
    Vec (n + 1) (BitVector bits) ->
    BVCalendar addressWidth

instance Show (BVCalendar addressWidth) where
  show (BVCalendar _ _ bvvec) = show bvvec

genBVCalendar :: Integer -> Integer -> Gen (BVCalendar 32)
genBVCalendar calSize bitWidth = do
  let
   calNat = TN.someNatVal (fromIntegral $ calSize - 1)
   bitNat = TN.someNatVal (fromIntegral bitWidth)
  case (calNat, bitNat) of
    (SomeNat size, SomeNat bits) -> go size bits
 where
  go :: forall calSize bitWidth . (KnownNat calSize, KnownNat bitWidth) => Proxy calSize -> Proxy bitWidth-> Gen (BVCalendar 32)
  go s b = do
    let
      calNatBits = clogBaseSNat d2 . succSNat $ snatProxy s
      requiredAddrWidth = SNat @(NatRequiredBits (TypeRequiredRegisters (BitVector bitWidth) 32))
    case (compareSNat calNatBits d32, compareSNat d1 calNatBits, compareSNat requiredAddrWidth d32, compareSNat d1 (snatProxy b)) of
      (SNatLE, SNatLE, SNatLE, SNatLE) -> do
        cal <- Gen.list (Range.singleton $ fromIntegral calSize) $ Gen.integral @_ @(BitVector bitWidth) Range.constantBounded
        return (BVCalendar (snatProxy s) (snatProxy b) $ unsafeFromList cal)
      _ -> error $ "genIntCalendar: Constraints not satisfied: 1 <= " <> show calNatBits <> " <= 32, " <> show requiredAddrWidth <> " <= 32."

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
        topEntity switch = withClockResetEnable clockGen resetGen enableGen $ (\(a,_,_) -> a)
          <$> calendarWB calSize' cal cal switch (pure (wishboneM2S (SNat @4) (SNat @32)))
        simOut = simulateN @System (fromIntegral simLength) topEntity switchSignal
      footnote . fromString $ "simOut: " <> show simOut
      footnote . fromString $ "expected: " <> show (toList cal)

      Set.fromList simOut === Set.fromList (toList cal)

indexOf :: (KnownNat n) => SNat (n+1) -> Index (n+1)
indexOf = fromSNat . predSNat
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
        wbWrites = wbNothingM2S : P.concatMap writeWithWishbone writeOps
        topEntity (unbundle -> (switch, writePort)) = withClockResetEnable clockGen
          resetGen enableGen $ (\(a,_,_) -> a) <$> calendarWB @_ @4 @32 (succSNat calSize') cal cal switch writePort
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
            wbReads = P.concatMap (\ i -> wbReadEntry (fromIntegral i) entryRegs) readAddresses
            topEntity writePort = withClockResetEnable clockGen
              resetGen enableGen $ (\(_,_,wb) -> wb) <$> calendarWB @_ @4 @32 (succSNat snatS) calA' calS' (pure False) writePort
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
        wbWrites = wbWriteOp <$> P.zip (P.repeat newDepthAddr) (fromIntegral <$> newDepths)
        switchSignal = P.concatMap (\ (fromIntegral -> n) -> P.replicate n False <> [True]) allDepths
        wbIn = P.concatMap (\(fromIntegral -> i, wb) -> wb : P.replicate i wbNothingM2S) $ P.zip (1 + calSize : newDepths) wbWrites
        topEntity (unbundle -> (switch, writePort)) = withClockResetEnable clockGen
          resetGen enableGen $ (\(_,m,_) -> m) <$> calendarWB @_ @4 @32 calSize' cal cal switch writePort
        topEntityInput = P.zip switchSignal (wbIn <> P.repeat wbNothingM2S)
        simOut = simulateN @System simLength topEntity topEntityInput
        expectedOut = P.take simLength $ True : P.tail switchSignal
      footnote . fromString $ "Simulation:   " <> show simOut
      footnote . fromString $ "Expected:     " <> show expectedOut
      footnote . fromString $ "shadowSwitch: " <> show (P.take simLength switchSignal)
      footnote . fromString $ "wishbone in:   " <> show (P.take simLength wbIn)
      simOut === expectedOut

indexOf :: (KnownNat n) => SNat (n+1) -> Index (n+1)
indexOf = fromSNat . predSNat

asProxy :: SNat n -> Proxy n
asProxy SNat = Proxy

requiredRegs :: (1 <= regSize) => SNat bits -> SNat regSize-> SNat (TypeRequiredRegisters (BitVector bits) regSize)
requiredRegs SNat SNat = SNat

takeLast :: Int -> [a] -> [a]
takeLast n l = P.drop (P.length l - n) l

wbNothingM2S :: forall bytes aw . (KnownNat bytes, KnownNat aw) => WishboneM2S bytes aw
wbNothingM2S = (wishboneM2S (SNat @bytes) (SNat @aw))
 { addr = 0
 , writeData = 0
 , busSelect = 0}

writeWithWishbone ::
  forall bytes aw n entry .
  (AtLeastOne bytes, KnownNat aw, KnownNat n, Paddable entry) =>
  (Index n, entry) ->
  [WishboneM2S bytes aw]
writeWithWishbone (a, entry) =
  case getRegs entry of
    RegisterBank SNat SNat vec -> toList $ fmap wbWriteOp $ zip indicesI (vec :< fromIntegral a)
 where
  getRegs :: entry  -> RegisterBank (bytes * 8) entry
  getRegs = paddedToRegisters . padData

directedWBDecoding :: forall bytes aw a . (AtLeastOne bytes, KnownNat aw, Paddable a) =>
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
    Just vec -> paddedToData . bvAsPadded @(TypeRequiredRegisters a (bytes * 8) * bytes * 8) $ pack vec
    Nothing  -> error $ "directedWBDecoding: list to vector conversion failed: " <> show entryList <> "from " <> show (wbM2S:m2sRest)

  consumedReads = P.length entryList
  remainingM2S = P.drop consumedReads m2sRest
  remainingS2M = P.drop consumedReads s2mRest

  out | foundBeginning = entry : directedWBDecoding remainingM2S remainingS2M
      | otherwise = directedWBDecoding m2sRest s2mRest

directedWBDecoding _ _ = []

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
