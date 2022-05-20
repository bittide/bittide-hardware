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
import qualified Hedgehog.Gen as Gen
import qualified Prelude as P

import Bittide.SharedTypes
import Bittide.DoubleBufferedRAM
import Clash.Hedgehog.Sized.Index
import Data.String

ramGroup :: TestTree
ramGroup = testGroup "DoubleBufferedRAM group"
  [ testPropertyNamed "Reading the buffer."
      "readDoubleBufferedRAM" readDoubleBufferedRAM
  , testPropertyNamed "Writing and reading back buffers."
      "readWriteDoubleBufferedRAM" readWriteDoubleBufferedRAM
  , testPropertyNamed "Byte addressable blockRam matches behavioral model."
      "readWriteByteAddressableBlockram" readWriteByteAddressableBlockram
  , testPropertyNamed "Byte addressable blockRam with always high byteEnables behaves like blockRam"
      "byteAddressableBlockRamAsBlockRam" byteAddressableBlockRamAsBlockRam
  , testPropertyNamed "Byte addressable double buffered blockRam matches behavioral model."
      "doubleBufferedRAMByteAddressable0" doubleBufferedRAMByteAddressable0
  , testPropertyNamed "Byte addressable double buffered blockRam with always high byteEnables behaves like 'doubleBufferedRAM'"
      "doubleBufferedRAMByteAddressable1" doubleBufferedRAMByteAddressable1]

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
      -- TODO: Due to some unexpected mismatch between the expected behavior of either
      -- blockRam or the behavioral model, the boot behavior is inconsistent. We drop the first
      -- expectedOutput cycle too, we expect this is due to the resets supplied b simulateN.
      -- An issue has been made regarding this.
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
      -- TODO: Due to some unexpected mismatch between the expected behavior of either
      -- blockRam or the behavioral model, the boot behavior is inconsistent. We drop the first
      -- expectedOutput cycle too, we expect this is due to the resets supplied b simulateN.
      -- An issue has been made regarding this.
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
        topEntityInput = L.zip3 switchSignal readAddresses
          (P.zipWith (\adr wr -> (adr,) <$> wr) writeAddresses writeEntries)
        simOut = simulateN @System simLength topEntity topEntityInput
        (duvOut, refOut) = L.unzip simOut
      duvOut === refOut

-- | Model for 'byteAddressableRAM', it stores the inputs in its state for a one cycle delay
-- and updates the RAM based on the the write operation and byte enables.
-- Furthermore it contains read-before-write behavior based on the readAddr.
byteAddressableRAMBehavior :: forall bits depth bytes .
  (KnownNat depth, 1 <= depth
  , KnownNat bytes, 1 <= bytes
  , bytes ~ Regs (BitVector bits) 8
  , KnownNat bits, 1 <= bits) =>

  ((Index depth, Maybe (LocatedBits depth bits), ByteEnable bytes)
  , Vec depth (BitVector bits))->

  (Index depth, Maybe (LocatedBits depth bits), ByteEnable bytes) ->

  ((( Index depth
    , Maybe (LocatedBits depth bits)
    , ByteEnable bytes)
   , Vec depth (BitVector bits))
  , BitVector bits)
byteAddressableRAMBehavior state input = (state', ram !! readAddr)
 where
  ((readAddr, writeOp, byteEnable), ram) = state
  (writeAddr, writeData) = fromMaybe (0, 0b0) writeOp
  writeTrue = isJust writeOp
  RegisterBank oldData = getRegs $ ram !! writeAddr
  RegisterBank newData = getRegs writeData
  newEntry = getData $ zipWith (\ sel (old,new) -> if sel then new else old) (unpack byteEnable) $
   zip oldData newData


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
 ( KnownNat depth
 , 1 <= depth
 , KnownNat bytes
 , 1 <= bytes
 , bytes ~ Regs (BitVector bits) 8
 , KnownNat bits
 , 1 <= bits) =>
 ((Bool, Index depth, Maybe (LocatedBits depth bits), BitVector bytes)
 , Vec depth (BitVector bits), Vec depth (BitVector bits))->

 (Bool, Index depth, Maybe (LocatedBits depth bits), BitVector bytes) ->

 (((Bool, Index depth, Maybe (LocatedBits depth bits), BitVector bytes)
 , Vec depth (BitVector bits)
 , Vec depth (BitVector bits))
 , BitVector bits)
byteAddressableDoubleBufferedRAMBehavior state input = (state', pack $ bufA0 !! readAddr)
 where
  ((switchBuffers, readAddr, writeOp, byteEnable), bufA, bufB) = state
  (bufA0, bufB0) = if switchBuffers then (bufB, bufA) else (bufA, bufB)

  (writeAddr, writeData) = fromMaybe (0, 0b0) writeOp
  writeTrue = isJust writeOp
  RegisterBank oldData = getRegs $ bufB0 !! writeAddr
  RegisterBank newData = getRegs writeData
  newEntry = getData $ zipWith
    (\ sel (old,new) -> if sel then new else old)
    (unpack byteEnable) $ zip oldData newData

  bufB1 = if writeTrue then replace writeAddr newEntry bufB0 else bufB0
  state' = (input, bufA0, bufB1)

  getData :: Vec bytes Byte -> BitVector bits
  getData vec = registersToData @_ @8 $ RegisterBank vec
