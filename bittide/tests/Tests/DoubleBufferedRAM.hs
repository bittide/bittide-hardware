{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Tests.DoubleBufferedRAM(ramGroup) where
import Clash.Prelude
import Clash.Hedgehog.Sized.Vector

import Bittide.DoubleBufferedRAM
import Clash.Sized.Vector ( unsafeFromList )
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

deriving instance (Show a) => Show (SomeVec 1 a)

ramGroup :: TestTree
ramGroup = testGroup "DoubleBufferedRAM group"
  [ testPropertyNamed "Reading the buffer." "readDoubleBufferedRAM" readDoubleBufferedRAM
  , testPropertyNamed "Wriing and reading back buffers." "readWriteDoubleBufferedRAM" readWriteDoubleBufferedRAM
  , testProperty "Byte addressable blockram matches behavorial model." readWriteByteAddressableBlockram
  , testProperty "Byte addressable double buffered RAM matches behavorial model." readWriteByteAddressableDoubleBufferedRAM]

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

type WriteOp addr bytes = Maybe (addr, BitVector (bytes*8))
type VecVecBytes depth bytes = Vec depth (Vec bytes (BitVector 8))

data BytesRAM where
  BytesRAM :: SNat depth -> SNat extraBytes -> VecVecBytes depth (extraBytes+1) -> BytesRAM

instance Show BytesRAM where
  show (BytesRAM SNat SNat c) = show c

genBlockRamContents :: Int -> Int -> Gen BytesRAM
genBlockRamContents depth0 nrOfBytes0 = do
  case (TN.someNatVal $ fromIntegral depth0, TN.someNatVal $ fromIntegral nrOfBytes0 - 1) of
    (SomeNat depth, SomeNat nrOfBytes) -> do
      let
        depthRange = Range.singleton depth0
        bytesRange = Range.singleton nrOfBytes0
      contents <- Gen.list depthRange $
       unsafeFromList <$> Gen.list bytesRange (Gen.integral Range.constantBounded)
      return $ BytesRAM (snatProxy depth) (snatProxy nrOfBytes) $ unsafeFromList contents

readWriteByteAddressableBlockram :: Property
readWriteByteAddressableBlockram = property $ do
  ramDepth <- forAll $ Gen.enum 1 31
  nrOfBytes <- forAll $ Gen.enum 1 10
  simLength <- forAll $ Gen.enum 2 100
  ramContents <- forAll $ genBlockRamContents ramDepth nrOfBytes
  case ramContents of
    BytesRAM SNat SNat contents -> do

      let
        simRange = Range.singleton simLength
        topEntity (unbundle -> (readAddr, writePort, byteSelect)) = withClockResetEnable
          @System clockGen resetGen enableGen blockRamByteAddressable (pack <$> contents)
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
  ramDepth <- forAll $ Gen.enum 1 31
  nrOfBytes <- forAll $ Gen.enum 1 10
  simLength <- forAll $ Gen.enum 2 100
  ramContents <- forAll $ genBlockRamContents ramDepth nrOfBytes
  case ramContents of
    BytesRAM SNat SNat contents -> do

      let
        simRange = Range.singleton simLength
        topEntity (unbundle -> (switch, readAddr, writePort, byteSelect)) = withClockResetEnable
          @System clockGen resetGen enableGen doubleBufferedRAMByteAddressable (pack <$> contents)
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


byteAddressableRAMBehaviour :: (Num addr, Enum addr, KnownNat depth, KnownNat bytes) =>
  ((addr, WriteOp addr bytes, BitVector bytes), VecVecBytes depth bytes)->
  (addr, WriteOp addr bytes, BitVector bytes) ->
  (((addr, WriteOp addr bytes, BitVector bytes), VecVecBytes depth bytes), BitVector (bytes*8))
byteAddressableRAMBehaviour state input = (state', pack $ ram !! readAddr)
 where
  ((readAddr, writeOp, byteEnable), ram) = state
  (writeAddr, writeData) = fromMaybe (0, 0b0) writeOp
  writeTrue = isJust writeOp
  oldData = ram !! writeAddr

  newEntry = zipWith (\ sel (old,new) -> if sel then new else old) (unpack byteEnable) $
   zip oldData (unpack writeData)

  ram1 = if writeTrue then replace writeAddr newEntry ram else ram
  state' = (input, ram1)

byteAddressableDoubleBufferedRAMBehaviour ::
 (Num addr, Enum addr, KnownNat depth, KnownNat bytes) =>
 ((Bool, addr, WriteOp addr bytes, BitVector bytes), VecVecBytes depth bytes, VecVecBytes depth bytes)->
 (Bool, addr, WriteOp addr bytes, BitVector bytes) ->
 (((Bool, addr, WriteOp addr bytes, BitVector bytes), VecVecBytes depth bytes, VecVecBytes depth bytes), BitVector (bytes*8))
byteAddressableDoubleBufferedRAMBehaviour state input = (state', pack $ bufA0 !! readAddr)
 where
  ((switchBuffers, readAddr, writeOp, byteEnable), bufA, bufB) = state
  (bufA0, bufB0) = if switchBuffers then (bufB, bufA) else (bufA, bufB)

  (writeAddr, writeData) = fromMaybe (0, 0b0) writeOp
  writeTrue = isJust writeOp
  oldData = bufB0 !! writeAddr

  newEntry = zipWith (\ sel (old,new) -> if sel then new else old) (unpack byteEnable) $
   zip oldData (unpack writeData)

  bufB1 = if writeTrue then replace writeAddr newEntry bufB0 else bufB0
  state' = (input, bufA0, bufB1)
