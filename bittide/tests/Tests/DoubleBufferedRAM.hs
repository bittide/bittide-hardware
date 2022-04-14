-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=5 #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module Tests.DoubleBufferedRAM(ramGroup) where

import Clash.Prelude
import Clash.Hedgehog.Sized.Vector
import Clash.Hedgehog.Sized.BitVector


import Data.Maybe
import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Data.List as L
import qualified Data.Set as Set
import qualified GHC.TypeNats as TN
import qualified Prelude as P
import Data.Proxy
import Data.Type.Equality (type (:~:)(Refl))

import Bittide.DoubleBufferedRAM
import Bittide.SharedTypes

ramGroup :: TestTree
ramGroup = testGroup "DoubleBufferedRAM group"
  [ testPropertyNamed "Reading the buffer." "readDoubleBufferedRAM" readDoubleBufferedRAM
  , testPropertyNamed "Writing and reading back buffers." "readWriteDoubleBufferedRAM" readWriteDoubleBufferedRAM
  , testPropertyNamed "Byte addressable blockram matches behavorial model." "readWriteByteAddressableBlockram" readWriteByteAddressableBlockram
  , testPropertyNamed "Byte addressable double buffered RAM matches behavorial model." "readWriteByteAddressableDoubleBufferedRAM" readWriteByteAddressableDoubleBufferedRAM
  , testPropertyNamed "Byte addressable register can be written to and read from with byte enables." "readWriteRegisterByteAddressable" readWriteRegisterByteAddressable]

genRamContents :: (MonadGen m, Integral i) => i -> m a -> m (SomeVec 1 a)
genRamContents depth = genSomeVec (Range.singleton $ fromIntegral (depth - 1))

-- | This test checks if we can read the inital values of the double buffered RAM.
readDoubleBufferedRAM :: Property
readDoubleBufferedRAM = property $ do
  ramDepth <- forAll $ Gen.int (Range.constant 1 31)
  ramContents <- forAll $ genRamContents ramDepth $ Gen.int Range.constantBounded
  case ramContents of
    SomeVec SNat contents -> do
      simLength <- forAll $ Gen.int (Range.constant 1 100)
      let simRange = Range.singleton simLength
      switchSignal <- forAll $ Gen.list simRange Gen.bool
      readAddresses <- forAll $ Gen.list simRange $ Gen.enum 0 (fromIntegral $ ramDepth - 1)
      let
        topEntity (unbundle -> (switch, readAddr)) = withClockResetEnable @System clockGen
          resetGen enableGen $ doubleBufferedRAM contents switch readAddr (pure Nothing)
        topEntityInput = P.zip switchSignal readAddresses
        simOut = P.tail $ simulateN simLength topEntity topEntityInput
        expectedOut = fmap (contents !!) readAddresses
      simOut === P.init expectedOut

-- | This test checks if we can write new values to the double buffered RAM and read them.
readWriteDoubleBufferedRAM :: Property
readWriteDoubleBufferedRAM = property $ do
  ramDepth <- forAll $ Gen.int (Range.constant 2 31)
  ramContents <- forAll $ genRamContents ramDepth $ Gen.int Range.constantBounded
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
  BitvecVec :: (1 <= bits, 1 <= depth, 1 <= Regs (BitVector bits) 8) => SNat depth -> SNat bits -> Vec depth (BitVector bits) -> BitvecVec

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
      SNatLE -> BitvecVec (succSNat $ snatProxy depth0) (succSNat $ snatProxy bits0)
       <$> genNonEmptyVec genDefinedBitVector
      _ -> error "genBlockRamContents: Generated bitvector is of size 0."


readWriteByteAddressableBlockram :: Property
readWriteByteAddressableBlockram = property $ do
  ramDepth <- forAll $ Gen.enum 1 100
  nrOfBits <- forAll $ Gen.enum 1 100
  simLength <- forAll $ Gen.int $ Range.constant 2 100
  ramContents <- forAll $ genBlockRamContents ramDepth nrOfBits
  case ramContents of
    BitvecVec SNat SNat contents -> do
      let
        simRange = Range.singleton simLength
        topEntity (unbundle -> (readAddr, writePort, byteSelect)) = withClockResetEnable
          @System clockGen resetGen enableGen blockRamByteAddressable contents
          readAddr writePort byteSelect
      writeAddresses <- forAll $ Gen.list simRange $ Gen.integral Range.constantBounded
      readAddresses <- forAll $ Gen.list simRange $ Gen.integral Range.constantBounded
      writeEntries <- forAll (Gen.list simRange $ Gen.maybe $ Gen.integral Range.constantBounded)
      byteSelectSignal <- forAll $ Gen.list simRange $ Gen.integral Range.constantBounded
      let
        topEntityInput = L.zip3 readAddresses (P.zipWith (\adr wr -> (adr,) <$> wr) writeAddresses writeEntries) byteSelectSignal
        simOut = simulateN @System simLength topEntity topEntityInput
        (_,expectedOut) = L.mapAccumL byteAddressableRAMBehaviour (L.head topEntityInput, contents) $ L.tail topEntityInput
      L.drop 2 simOut === L.tail expectedOut

readWriteByteAddressableDoubleBufferedRAM :: Property
readWriteByteAddressableDoubleBufferedRAM = property $ do
  ramDepth <- forAll $ Gen.enum 1 100
  nrOfBits <- forAll $ Gen.enum 1 100
  simLength <- forAll $ Gen.int $ Range.constant 2 100
  ramContents <- forAll $ genBlockRamContents ramDepth nrOfBits
  case ramContents of
    BitvecVec SNat SNat contents -> do
      let
        simRange = Range.singleton simLength
        topEntity (unbundle -> (switch, readAddr, writePort, byteSelect)) = withClockResetEnable
          @System clockGen resetGen enableGen doubleBufferedRAMByteAddressable contents
          switch readAddr writePort byteSelect
      writeAddresses <- forAll $ Gen.list simRange $ Gen.integral Range.constantBounded
      readAddresses <- forAll $ Gen.list simRange $ Gen.integral Range.constantBounded
      writeEntries <- forAll (Gen.list simRange $ Gen.maybe $ Gen.integral Range.constantBounded)
      byteSelectSignal <- forAll $ Gen.list simRange $ Gen.integral Range.constantBounded
      switchSignal <- forAll $ Gen.list simRange Gen.bool
      let
        topEntityInput = L.zip4 switchSignal readAddresses (P.zipWith (\adr wr -> (adr,) <$> wr) writeAddresses writeEntries) byteSelectSignal
        simOut = simulateN @System simLength topEntity topEntityInput
        (_,expectedOut) = L.mapAccumL byteAddressableDoubleBufferedRAMBehaviour (L.head topEntityInput, contents, contents) $ L.tail topEntityInput
      L.drop 2 simOut === L.tail expectedOut

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


byteAddressableRAMBehaviour :: forall bits depth bytes .
  (KnownNat depth, 1 <= depth, KnownNat bytes, 1 <= bytes, bytes ~ Regs (BitVector bits) 8, KnownNat bits, 1 <= bits) =>
  ((Index depth, WriteBits depth bits, ByteEnable bytes), Vec depth (BitVector bits))->
  (Index depth, WriteBits depth bits, ByteEnable bytes) ->
  (((Index depth, WriteBits depth bits, ByteEnable bytes), Vec depth (BitVector bits)), BitVector bits)
byteAddressableRAMBehaviour state input = (state', ram !! readAddr)
 where
  ((readAddr, writeOp, byteEnable), ram) = state
  (writeAddr, writeData) = fromMaybe (0, 0b0) writeOp
  writeTrue = isJust writeOp
  oldData = getRegs $ ram !! writeAddr

  newEntry = getData $ zipWith (\ sel (old,new) -> if sel then new else old) (unpack byteEnable) $
   zip oldData $ getRegs writeData

  getRegs a = case paddedToRegisters @8 $ Padded a of
    RegisterBank v -> v

  getData :: Vec bytes Byte -> BitVector bits
  getData vec = registersToData @_ @8 $ RegisterBank vec

  ram1 = if writeTrue then replace writeAddr newEntry ram else ram
  state' = (input, ram1)

byteAddressableDoubleBufferedRAMBehaviour :: forall bits depth bytes .
 (KnownNat depth, 1 <= depth, KnownNat bytes, 1 <= bytes, bytes ~ Regs (BitVector bits) 8, KnownNat bits, 1 <= bits) =>
 ((Bool, Index depth, WriteBits depth bits, BitVector bytes), Vec depth (BitVector bits), Vec depth (BitVector bits))->
 (Bool, Index depth, WriteBits depth bits, BitVector bytes) ->
 (((Bool, Index depth, WriteBits depth bits, BitVector bytes), Vec depth (BitVector bits), Vec depth (BitVector bits)), BitVector bits)
byteAddressableDoubleBufferedRAMBehaviour state input = (state', pack $ bufA0 !! readAddr)
 where
  ((switchBuffers, readAddr, writeOp, byteEnable), bufA, bufB) = state
  (bufA0, bufB0) = if switchBuffers then (bufB, bufA) else (bufA, bufB)

  (writeAddr, writeData) = fromMaybe (0, 0b0) writeOp
  writeTrue = isJust writeOp
  oldData = getRegs $ bufB0 !! writeAddr

  newEntry = getData $ zipWith (\ sel (old,new) -> if sel then new else old) (unpack byteEnable) $
   zip oldData (getRegs writeData)

  bufB1 = if writeTrue then replace writeAddr newEntry bufB0 else bufB0
  state' = (input, bufA0, bufB1)
  getRegs a = case paddedToRegisters @8 $ Padded a of
    RegisterBank v -> v

  getData :: Vec bytes Byte -> BitVector bits
  getData vec = registersToData @_ @8 $ RegisterBank vec
