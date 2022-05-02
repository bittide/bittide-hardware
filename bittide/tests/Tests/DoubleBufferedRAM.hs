-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -freduction-depth=1000 #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
module Tests.DoubleBufferedRAM(ramGroup) where

import Clash.Prelude
import Clash.Hedgehog.Sized.Vector
import Clash.Hedgehog.Sized.BitVector
import Clash.Hedgehog.Sized.Unsigned


import Data.Maybe
import Hedgehog
import Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Data.List as L
import qualified Data.Set as Set
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen hiding (resize)
import qualified Prelude as P
import Data.Proxy
import Data.Type.Equality (type (:~:)(Refl))
import Contranomy.Wishbone
import Data.String

import Bittide.SharedTypes
import Bittide.DoubleBufferedRAM
import Clash.Hedgehog.Sized.Index

ramGroup :: TestTree
ramGroup = testGroup "DoubleBufferedRAM group"
  [ testPropertyNamed "Reading the buffer." "readDoubleBufferedRAM" readDoubleBufferedRAM
  , testPropertyNamed "Writing and reading back buffers." "readWriteDoubleBufferedRAM" readWriteDoubleBufferedRAM
  , testPropertyNamed "Byte addressable blockRam matches behavioral model." "readWriteByteAddressableBlockram" readWriteByteAddressableBlockram
  , testPropertyNamed "Byte addressable blockRam with always high byteEnables behaves like blockRam" "byteAddressableBlockRamAsBlockRam" byteAddressableBlockRamAsBlockRam
  , testPropertyNamed "Byte addressable double buffered blockRam matches behavioral model." "doubleBufferedRAMByteAddressable0" doubleBufferedRAMByteAddressable0
  , testPropertyNamed "Byte addressable double buffered blockRam with always high byteEnables behaves like 'doubleBufferedRAM'" "doubleBufferedRAMByteAddressable1" doubleBufferedRAMByteAddressable1
  , testPropertyNamed "Byte addressable register can be written to and read from with byte enables." "readWriteRegisterByteAddressable" readWriteRegisterByteAddressable
  , testPropertyNamed "registerWB can be written to and read from with byte enables." "registerWBWriteWishbone" registerWBWriteWishbone]

genRamContents :: (MonadGen m, Integral i) => i -> m a -> m (SomeVec 1 a)
genRamContents depth = genSomeVec (Range.singleton $ fromIntegral (depth - 1))

-- | This test checks if we can read the initial values of the double buffered RAM.
readDoubleBufferedRAM :: Property
readDoubleBufferedRAM = property $ do
  ramDepth <- forAll . Gen.int $ Range.constant 1 31
  ramContents <- forAll $ genRamContents ramDepth $ genUnsigned @_ @64 Range.constantBounded
  case ramContents of
    SomeVec SNat contents -> do
      simLength <- forAll $ bitCoerce <$> genUnsigned @_ @64 (Range.constant 1 100)
      let simRange = Range.singleton simLength
      switchSignal <- forAll $ Gen.list simRange Gen.bool
      readAddresses <- forAll . Gen.list simRange . genIndex $ Range.constantBounded
      let
        topEntity (unbundle -> (switch, readAddr)) = withClockResetEnable @System clockGen
          resetGen enableGen $ doubleBufferedRAM contents switch readAddr (pure Nothing)
        topEntityInput = P.zip switchSignal readAddresses
        simOut = P.tail $ simulateN simLength topEntity topEntityInput
        expectedOut = fmap (contents !!) readAddresses
      simOut === P.init expectedOut

-- | This test checks if we can write new values to the double buffered 'blockRam' and read them.
readWriteDoubleBufferedRAM :: Property
readWriteDoubleBufferedRAM = property $ do
  ramDepth <- forAll $ Gen.enum 1 31
  ramContents <- forAll $ genRamContents ramDepth $
    bitCoerce <$> genUnsigned @_ @64 Range.constantBounded
  let minSimLength = 2 * ramDepth
  simLength <- forAll $ Gen.int (Range.constant minSimLength 100)
  case ramContents of
    SomeVec SNat contents -> do
      let
        topEntity (unbundle -> (switch, readAddr, writePort)) = withClockResetEnable
          @System clockGen resetGen enableGen $ doubleBufferedRAM contents switch readAddr
          writePort
      let
        addresses = cycle $ fmap fromIntegral [0..ramDepth-1]
        switchSignal = (==0) <$> addresses
      writeEntries <- forAll (Gen.list (Range.singleton simLength) $ Gen.int Range.constantBounded)
      let
        topEntityInput = L.zip3 switchSignal addresses $ fmap Just (P.zip addresses writeEntries)
        simOut = simulateN @System simLength topEntity topEntityInput
      Set.fromList simOut === Set.fromList (toList contents <> L.take (simLength - ramDepth - 1) writeEntries)

data BitvecVec where
  BitvecVec ::
    (1 <= bits, 1 <= depth, 1 <= Regs (BitVector bits) 8) =>
    SNat depth ->
    SNat bits ->
    Vec depth (BitVector bits) ->
    BitvecVec

instance Show BitvecVec where
  show (BitvecVec SNat SNat v) = show v

genBlockRamContents :: Int -> Int -> Gen BitvecVec
genBlockRamContents depth bits = do
  case (TN.someNatVal $ fromIntegral (depth - 1), TN.someNatVal $ fromIntegral $ bits - 1) of
    (SomeNat depth0, SomeNat bits0) -> go (snatProxy depth0) (snatProxy bits0)
 where
  go :: forall depth bits . SNat depth -> SNat bits -> Gen BitvecVec
  go depth0@SNat bits0@SNat =
    case compareSNat d1 (SNat @(Regs (BitVector (bits + 1)) 8)) of
      SNatLE -> BitvecVec (succSNat depth0) (succSNat bits0)
       <$> genNonEmptyVec genDefinedBitVector
      _ -> error "genBlockRamContents: Generated BitVector is of size 0."

-- | This test checks if we can write new values to the byte addressable 'blockRam'
-- ('blockRamByteAddressable') and read them. It uses 'byteAddressableRAMBehavior' as-
-- reference model.
readWriteByteAddressableBlockram :: Property
readWriteByteAddressableBlockram = property $ do
  ramDepth <- forAll $ Gen.enum 1 31
  nrOfBits <- forAll $ Gen.enum 1 31
  simLength <- forAll $ bitCoerce @_ @Int <$> genUnsigned (Range.constant 2 100)
  ramContents <- forAll $ genBlockRamContents ramDepth nrOfBits
  case ramContents of
    BitvecVec SNat SNat contents -> do
      let
        simRange = Range.singleton simLength
        topEntity (unbundle -> (readAddr, writePort, byteSelect)) =
          withClockResetEnable clockGen resetGen enableGen
          blockRamByteAddressable contents readAddr writePort byteSelect
      writeAddresses <- forAll $ Gen.list simRange $ genIndex Range.constantBounded
      readAddresses <- forAll $ Gen.list simRange $ genIndex Range.constantBounded
      writeEntries <- forAll (Gen.list simRange $ Gen.maybe genDefinedBitVector)
      byteSelectSignal <- forAll $ Gen.list simRange genDefinedBitVector
      let
        topEntityInput = L.zip3 readAddresses
          (P.zipWith (\adr wr -> (adr,) <$> wr) writeAddresses writeEntries) byteSelectSignal

        simOut = simulateN @System simLength topEntity topEntityInput
        (_,expectedOut) = L.mapAccumL byteAddressableRAMBehavior
          (L.head topEntityInput, contents) $ L.tail topEntityInput
      -- Drop boot behavior
      L.drop 2 simOut === L.tail expectedOut

-- | This test checks if 'blockRamByteAddressable' behaves the same as 'blockRam' when the
-- byteEnables are always high.
byteAddressableBlockRamAsBlockRam :: Property
byteAddressableBlockRamAsBlockRam = property $ do
  ramDepth <- forAll $ Gen.enum 1 31
  nrOfBits <- forAll $ Gen.enum 1 31
  simLength <- forAll $ Gen.int $ Range.constant 2 100
  ramContents <- forAll $ genBlockRamContents ramDepth nrOfBits
  case ramContents of
    BitvecVec SNat SNat contents -> do
      let
        simRange = Range.singleton simLength
        -- topEntity returns a tuple with the outputs of (byteAddressableRAM,blockRam)
        topEntity (unbundle -> (readAddr, writePort)) =
          withClockResetEnable clockGen resetGen enableGen $ bundle
          ( blockRamByteAddressable contents readAddr writePort (pure maxBound)
          , blockRam contents readAddr writePort)
      writeAddresses <- forAll $ Gen.list simRange $ genIndex Range.constantBounded
      readAddresses <- forAll $ Gen.list simRange $ genIndex Range.constantBounded
      writeEntries <- forAll (Gen.list simRange $ Gen.maybe genDefinedBitVector)
      let
        topEntityInput = L.zip readAddresses
          (P.zipWith (\adr wr -> (adr,) <$> wr) writeAddresses writeEntries)
        simOut      = simulateN @System simLength topEntity topEntityInput
        (fstOut, sndOut) = L.unzip simOut
      footnote . fromString $ "simOut: " <> showX simOut
      fstOut === sndOut

-- | This test checks if we can write new values to the byte addressable double buffered
-- 'blockRam' ('doubleBufferedRAMByteAddressable') and read them.
doubleBufferedRAMByteAddressable0 :: Property
doubleBufferedRAMByteAddressable0 = property $ do
  ramDepth <- forAll $ Gen.enum 1 31
  nrOfBits <- forAll $ Gen.enum 1 31
  simLength <- forAll $ Gen.int $ Range.constant 2 100
  ramContents <- forAll $ genBlockRamContents ramDepth nrOfBits
  case ramContents of
    BitvecVec SNat SNat contents -> do
      let
        simRange = Range.singleton simLength
        topEntity (unbundle -> (switch, readAddr, writePort, byteSelect)) = withClockResetEnable
          @System clockGen resetGen enableGen doubleBufferedRAMByteAddressable contents
          switch readAddr writePort byteSelect
      writeAddresses <- forAll $ Gen.list simRange $ genIndex Range.constantBounded
      readAddresses <- forAll $ Gen.list simRange $ genIndex Range.constantBounded
      writeEntries <- forAll (Gen.list simRange $ Gen.maybe genDefinedBitVector)
      byteSelectSignal <- forAll $ Gen.list simRange genDefinedBitVector
      switchSignal <- forAll $ Gen.list simRange Gen.bool
      let
        topEntityInput = L.zip4 switchSignal readAddresses
          (P.zipWith (\adr wr -> (adr,) <$> wr) writeAddresses writeEntries) byteSelectSignal
        simOut = simulateN @System simLength topEntity topEntityInput
        (_,expectedOut) = L.mapAccumL byteAddressableDoubleBufferedRAMBehavior
          (L.head topEntityInput, contents, contents) $ L.tail topEntityInput
      -- Drop boot behavior.
      L.drop 2 simOut === L.tail expectedOut

-- | This test checks if 'doubleBufferedRAMByteAddressable' behaves the same as
-- 'doubleBufferedRAM' when the byteEnables are always high.
doubleBufferedRAMByteAddressable1 :: Property
doubleBufferedRAMByteAddressable1 = property $ do
  ramDepth <- forAll $ Gen.enum 1 31
  nrOfBits <- forAll $ Gen.enum 1 31
  simLength <- forAll $ Gen.int $ Range.constant 2 100
  ramContents <- forAll $ genBlockRamContents ramDepth nrOfBits
  case ramContents of
    BitvecVec SNat SNat contents -> do
      let
        simRange = Range.singleton simLength
        topEntity (unbundle -> (switch, readAddr, writePort)) =
          withClockResetEnable @System clockGen resetGen enableGen $ bundle
          ( doubleBufferedRAMByteAddressable contents switch readAddr writePort (pure maxBound)
          , doubleBufferedRAM contents switch readAddr writePort)
      writeAddresses <- forAll $ Gen.list simRange $ genIndex Range.constantBounded
      readAddresses <- forAll $ Gen.list simRange $ genIndex Range.constantBounded
      writeEntries <- forAll (Gen.list simRange $ Gen.maybe genDefinedBitVector)
      switchSignal <- forAll $ Gen.list simRange Gen.bool
      let
        topEntityInput = L.zip3 switchSignal readAddresses (P.zipWith (\adr wr -> (adr,) <$> wr) writeAddresses writeEntries)
        simOut = simulateN @System simLength topEntity topEntityInput
        (duvOut, refOut) = L.unzip simOut
      duvOut === refOut

readWriteRegisterByteAddressable :: Property
readWriteRegisterByteAddressable = property $ do
  bytes <- forAll $ Gen.enum 1 10
  case TN.someNatVal bytes of
    SomeNat p -> case compareSNat d1 (snatProxy p) of
      SNatLE -> go p
      _ -> error "readWriteRegisterByteAddressable: Amount of bytes == 0."
 where
  go :: forall bytes m . (KnownNat bytes, 1 <= bytes, KnownNat (bytes*8), 1 <= (bytes * 8), Monad m) => Proxy bytes -> PropertyT m ()
  go Proxy =
    case sameNat (Proxy @bytes) (Proxy @(Regs (Vec bytes Byte) 8)) of
      Just Refl -> do
        simLength <- forAll $ Gen.enum 1 100
        let
          writeGen = genNonEmptyVec @_ @bytes $ genDefinedBitVector @_ @8
        initVal <- forAll writeGen
        writes <- forAll $ Gen.list (Range.singleton simLength) writeGen
        byteEnables <- forAll $ Gen.list (Range.singleton simLength) $ genDefinedBitVector @_ @(Regs (Vec bytes Byte) 8)
        let
          topEntity (unbundle -> (newVal, byteEnable))=
            withClockResetEnable @System clockGen resetGen enableGen $
            registerByteAddressable initVal newVal byteEnable
          expectedOut = P.scanl simFunc initVal $ P.zip writes byteEnables
          simFunc olds (news,unpack -> bools) = (\(bool,old,new) -> if bool then new else old) <$> zip3 bools olds news
          simOut = simulateN simLength topEntity $ P.zip writes byteEnables
        simOut === P.take simLength expectedOut
      _ -> error "readWriteRegisterByteAddressable: Amount of bytes not equal to registers required."

-- |
registerWBWriteWishbone :: Property
registerWBWriteWishbone = property $ do
  bits <- forAll $ Gen.enum 1 100
  case TN.someNatVal bits of
    SomeNat p -> case compareSNat d1 (snatProxy p) of
      SNatLE -> go p
      _ -> error "registerWBWriteWishbone: Amount of bits == 0."
 where
  go :: forall bits m . (KnownNat bits, 1 <= bits, Monad m) => Proxy bits -> PropertyT m ()
  go Proxy = case compareSNat d1 $ SNat @(Regs (BitVector bits) 32) of
    SNatLE -> do
      let regs = (natToNum @(DivRU bits 32))
      initVal <- forAll $ genDefinedBitVector @_ @bits
      writes <- forAll $ Gen.list (Range.constant 1 25) $ genDefinedBitVector @_ @bits
      let
        simLength = L.length writes * regs + 1
        topEntity wbIn = fst $ withClockResetEnable clockGen resetGen enableGen $
         registerWB @System @_ @4 @32 initVal wbIn Nothing
        topEntityInput = L.concatMap wbWrite writes <> L.repeat idleM2S
        simOut = L.tail $ simulateN simLength topEntity topEntityInput
        filteredOut = everyNth regs simOut
      footnote . fromString $ "simOut: " <> showX simOut
      footnote . fromString $ "filteredOut:" <> showX filteredOut
      footnote . fromString $ "input:" <> showX (L.take simLength topEntityInput)
      footnote . fromString $ "expected" <> showX writes
      writes === filteredOut
    _ -> error "registerWBWriteWishbone: Registers required to store bitvector == 0."
   where
    wbWrite v = L.zipWith bv2WbWrite (L.reverse [0.. L.length l - 1]) l
     where
      RegisterBank (toList -> l) = paddedToRegisters $ Padded v
  everyNth n l  | L.length l >= n = x : everyNth n xs
                | otherwise = []
   where
    (x:xs) = L.drop (n-1) l

bv2WbWrite :: (BitPack a, Enum a) =>
  a
  -> ("DAT_MOSI" ::: BitVector 32)
  -> WishboneM2S 4 32
bv2WbWrite i v = (wishboneM2S @4 @32 SNat SNat)
  { addr = resize (pack i) ++# (0b00 :: BitVector 2)
  , writeData = v
  , writeEnable = True
  , busCycle = True
  , busSelect = maxBound
  }

idleM2S :: (KnownNat aw, KnownNat bs) => WishboneM2S bs aw
idleM2S = wishboneM2S SNat SNat

-- | Model for 'byteAddressableRAM', it stores the inputs in its state for a one cycle delay
-- and updates the RAM based on the the write operation and byte enables.
-- Furthermore it contains read-before-write behavior based on the readAddr.
byteAddressableRAMBehavior :: forall bits depth bytes .
  (KnownNat depth, 1 <= depth, KnownNat bytes, 1 <= bytes, bytes ~ Regs (BitVector bits) 8, KnownNat bits, 1 <= bits) =>
  ((Index depth, Maybe (LocatedBits depth bits), ByteEnable bytes), Vec depth (BitVector bits))->
  (Index depth, Maybe (LocatedBits depth bits), ByteEnable bytes) ->
  (((Index depth, Maybe (LocatedBits depth bits), ByteEnable bytes), Vec depth (BitVector bits)), BitVector bits)
byteAddressableRAMBehavior state input = (state', ram !! readAddr)
 where
  ((readAddr, writeOp, byteEnable), ram) = state
  (writeAddr, writeData0) = fromMaybe (0, 0b0) writeOp
  writeTrue = isJust writeOp
  oldData = getRegs $ ram !! writeAddr

  newEntry = getData $ zipWith (\ sel (old,new) -> if sel then new else old) (unpack byteEnable) $
   zip oldData $ getRegs writeData0

  getRegs a = case paddedToRegisters @8 $ Padded a of
    RegisterBank v -> v

  getData :: Vec bytes Byte -> BitVector bits
  getData vec = registersToData @_ @8 $ RegisterBank vec

  ram1 = if writeTrue then replace writeAddr newEntry ram else ram
  state' = (input, ram1)

-- | Model for 'byteAddressableDoubleBufferedRAMBehavior', it stores the inputs in its
-- state for a one cycle delay and updates the RAM based on the the write operation and
-- byte enables. Furthermore it contains read-before-write behavior based on the readAddr.
-- The only addition compared to byteAddressableRAM is the fact that there's two buffers
-- (one read only, one write only), that can be swapped.
byteAddressableDoubleBufferedRAMBehavior :: forall bits depth bytes .
 (KnownNat depth, 1 <= depth, KnownNat bytes, 1 <= bytes, bytes ~ Regs (BitVector bits) 8, KnownNat bits, 1 <= bits) =>
 ((Bool, Index depth, Maybe (LocatedBits depth bits), BitVector bytes), Vec depth (BitVector bits), Vec depth (BitVector bits))->
 (Bool, Index depth, Maybe (LocatedBits depth bits), BitVector bytes) ->
 (((Bool, Index depth, Maybe (LocatedBits depth bits), BitVector bytes), Vec depth (BitVector bits), Vec depth (BitVector bits)), BitVector bits)
byteAddressableDoubleBufferedRAMBehavior state input = (state', pack $ bufA0 !! readAddr)
 where
  ((switchBuffers, readAddr, writeOp, byteEnable), bufA, bufB) = state
  (bufA0, bufB0) = if switchBuffers then (bufB, bufA) else (bufA, bufB)

  (writeAddr, writeData0) = fromMaybe (0, 0b0) writeOp
  writeTrue = isJust writeOp
  oldData = getRegs $ bufB0 !! writeAddr

  newEntry = getData $ zipWith (\ sel (old,new) -> if sel then new else old) (unpack byteEnable) $
   zip oldData (getRegs writeData0)

  bufB1 = if writeTrue then replace writeAddr newEntry bufB0 else bufB0
  state' = (input, bufA0, bufB1)
  getRegs a = case paddedToRegisters @8 $ Padded a of
    RegisterBank v -> v

  getData :: Vec bytes Byte -> BitVector bits
  getData vec = registersToData @_ @8 $ RegisterBank vec
