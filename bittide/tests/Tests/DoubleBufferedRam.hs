-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.DoubleBufferedRam(ramGroup) where

import Clash.Prelude

import Clash.Hedgehog.Sized.Index
import Clash.Hedgehog.Sized.Unsigned
import Clash.Hedgehog.Sized.Vector
import Clash.Signal.Internal(Signal(..))
import Data.Maybe
import Data.Proxy
import Data.String
import Data.Type.Equality (type (:~:)(Refl))
import Hedgehog
import Hedgehog.Range as Range
import Protocols.Hedgehog.Internal
import Protocols.Wishbone
import Protocols.Wishbone.Standard.Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

import Bittide.SharedTypes
import Bittide.DoubleBufferedRam
import Tests.Shared

import qualified Clash.Sized.Vector as V
import qualified Data.List as L
import qualified Data.Set as Set
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen hiding (resize)
import qualified Prelude as P

ramGroup :: TestTree
ramGroup = testGroup "DoubleBufferedRam group"
  [ testPropertyNamed "Reading the buffer."
      "readDoubleBufferedRam" readDoubleBufferedRam
  , testPropertyNamed "Writing and reading back buffers."
      "readWriteDoubleBufferedRam" readWriteDoubleBufferedRam
  , testPropertyNamed "Byte addressable blockRam matches behavioral model."
      "readWriteByteAddressableBlockram" readWriteByteAddressableBlockram
  , testPropertyNamed "Byte addressable blockRam with always high byteEnables behaves like blockRam"
      "byteAddressableBlockRamAsBlockRam" byteAddressableBlockRamAsBlockRam
  , testPropertyNamed "Byte addressable double buffered blockRam matches behavioral model."
      "doubleBufferedRamByteAddressable0" doubleBufferedRamByteAddressable0
  , testPropertyNamed "Byte addressable double buffered blockRam with always high byteEnables behaves like 'doubleBufferedRam'"
      "doubleBufferedRamByteAddressable1" doubleBufferedRamByteAddressable1
  , testPropertyNamed "Byte addressable register can be written to and read from with byte enables."
      "readWriteRegisterByteAddressable" readWriteRegisterByteAddressable
  , testPropertyNamed "registerWb function as a normal register."
      "registerWbSigToSig" registerWbSigToSig
  , testPropertyNamed "registerWb can be written to with wishbone."
      "registerWbWbToSig" registerWbWbToSig
  , testPropertyNamed "registerWb can be read from with wishbone."
      "registerWbSigToWb" registerWbSigToWb
  , testPropertyNamed "registerWb write conflict resolution matches set priorities"
      "registerWbWriteCollisions" registerWbWriteCollisions
  , testPropertyNamed "Simulate the contentGenerator for an arbitrary vector."
      "testContentGen" testContentGen
  , testPropertyNamed "Test wishboneStorage spec compliance"
      "wbStorageSpecCompliance" wbStorageSpecCompliance
  , testPropertyNamed "Test whether wbStorage acts the same its Behavioral model"
      "wbStorageBehavior" wbStorageBehavior
  ]

genRamContents :: (MonadGen m, Integral i) => i -> m a -> m (SomeVec 1 a)
genRamContents memDepth = genSomeVec (Range.singleton $ fromIntegral (memDepth - 1))

-- | This test checks if we can read the initial values of the double buffered Ram.
readDoubleBufferedRam :: Property
readDoubleBufferedRam = property $ do
  ramDepth <- forAll . Gen.int $ Range.constant 1 31
  ramContents <- forAll $ genRamContents ramDepth
    $ genUnsigned @_ @64 Range.constantBounded
  case ramContents of
    SomeVec SNat (contentsSingle :: Vec (n+1) (Unsigned 64)) -> do
      simLength <- forAll $ Gen.int (Range.constant 1 100)
      let
        contentsDouble = concatMap (replicate d2) contentsSingle
        simRange = Range.singleton simLength
      switchSignal <- forAll $ Gen.list simRange (Gen.element [A,B])
      readAddresses <- forAll . Gen.list simRange . genIndex $ Range.constantBounded
      let
        topEntity (unbundle -> (switch, readAddr)) = withClockResetEnable @System clockGen
          resetGen enableGen $ doubleBufferedRam @_ @(n+1) (Vec contentsDouble)
          switch readAddr (pure Nothing)
        topEntityInput = P.zip switchSignal readAddresses
        simOut = P.tail $ simulateN simLength topEntity topEntityInput
        expectedOut = fmap (contentsSingle !!) readAddresses
      simOut === P.init expectedOut

-- | This test checks if we can write new values to the double buffered 'blockRam' and read them.
readWriteDoubleBufferedRam :: Property
readWriteDoubleBufferedRam = property $ do
  ramDepth <- forAll $ Gen.enum 1 31
  ramContents <- forAll $ genRamContents ramDepth $
    genUnsigned @_ @64 Range.constantBounded
  let minSimLength = 2 * ramDepth
  simLength <- forAll $ Gen.int (Range.constant minSimLength 100)
  case ramContents of
    SomeVec SNat (contentsSingle :: Vec (n+1) (Unsigned 64)) -> do
      let
        contentsDouble = concatMap (replicate d2) contentsSingle
        topEntity (unbundle -> (switch, readAddr, writePort)) = withClockResetEnable
          @System clockGen resetGen enableGen $ doubleBufferedRam @_ @(n+1)
          (Vec contentsDouble) switch readAddr writePort
      let
        addresses = cycle $ fmap fromIntegral [0..ramDepth-1]
        switchSignal = cycle $ L.replicate ramDepth A <> L.replicate ramDepth B
      writeEntries <- forAll (Gen.list (Range.singleton simLength)
        $ genUnsigned Range.constantBounded)
      let
        topEntityInput = L.zip3 switchSignal addresses
          $ fmap Just (P.zip addresses writeEntries)
        simOut = simulateN @System simLength topEntity topEntityInput
        expected = toList contentsSingle <> L.take (simLength - ramDepth - 1) writeEntries
      Set.fromList simOut === Set.fromList expected

data BitvecVec where
  BitvecVec ::
    (1 <= bits, 1 <= memDepth, 1 <= Regs (BitVector bits) 8) =>
    SNat memDepth ->
    SNat bits ->
    Vec memDepth (BitVector bits) ->
    BitvecVec

instance Show BitvecVec where
  show (BitvecVec SNat SNat v) = show v

genBlockRamContents :: Int -> Int -> Gen BitvecVec
genBlockRamContents memDepth bits = do
  case ( TN.someNatVal $ fromIntegral (memDepth - 1)
       , TN.someNatVal $ fromIntegral $ bits - 1) of
    (SomeNat depth0, SomeNat bits0) -> go (snatProxy depth0) (snatProxy bits0)
 where
  go :: forall memDepth bits . SNat memDepth -> SNat bits -> Gen BitvecVec
  go depth0@SNat bits0@SNat =
    case compareSNat d1 (SNat @(Regs (BitVector (bits + 1)) 8)) of
      SNatLE -> BitvecVec (succSNat depth0) (succSNat bits0)
       <$> genNonEmptyVec genDefinedBitVector
      _ -> error "genBlockRamContents: Generated BitVector is of size 0."

-- | This test checks if we can write new values to the byte addressable 'blockRam'
-- ('blockRamByteAddressable') and read them. It uses 'byteAddressableRamBehavior' as-
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
          blockRamByteAddressable (Vec contents) readAddr writePort byteSelect
      writeAddresses <- forAll $ Gen.list simRange $ genIndex Range.constantBounded
      readAddresses <- forAll $ Gen.list simRange $ genIndex Range.constantBounded
      writeEntries <- forAll (Gen.list simRange $ Gen.maybe genDefinedBitVector)
      byteSelectSignal <- forAll $ Gen.list simRange genDefinedBitVector
      let
        topEntityInput = L.zip3 readAddresses
          (P.zipWith (\adr wr -> (adr,) <$> wr) writeAddresses writeEntries) byteSelectSignal

        simOut = simulateN @System simLength topEntity topEntityInput
        (_,expectedOut) = L.mapAccumL byteAddressableRamBehavior
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
        -- topEntity returns a tuple with the outputs of (byteAddressableRam,blockRam)
        topEntity (unbundle -> (readAddr, writePort)) =
          withClockResetEnable clockGen resetGen enableGen $ bundle
          ( blockRamByteAddressable (Vec contents) readAddr writePort (pure maxBound)
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
-- 'blockRam' ('doubleBufferedRamByteAddressable') and read them.
doubleBufferedRamByteAddressable0 :: Property
doubleBufferedRamByteAddressable0 = property $ do
  ramDepth <- forAll $ Gen.enum 1 31
  nrOfBits <- forAll $ Gen.enum 1 31
  simLength <- forAll $ Gen.int $ Range.constant 2 100
  ramContents <- forAll $ genBlockRamContents ramDepth nrOfBits
  case ramContents of
    BitvecVec SNat SNat (contentsSingle :: Vec memDepth (BitVector bits)) -> do
      let
        contentsDouble = concatMap (replicate d2) contentsSingle
        simRange = Range.singleton simLength
        topEntity (unbundle -> (switch, readAddr, writePort, byteSelect)) =
          withClockResetEnable @System clockGen resetGen enableGen
          $ doubleBufferedRamByteAddressable (Vec contentsDouble)
          switch readAddr writePort byteSelect
      writeAddresses <- forAll $ Gen.list simRange $ genIndex Range.constantBounded
      readAddresses <- forAll $ Gen.list simRange $ genIndex Range.constantBounded
      writeEntries <- forAll (Gen.list simRange $ Gen.maybe genDefinedBitVector)
      byteSelectSignal <- forAll $ Gen.list simRange genDefinedBitVector
      switchSignal <- forAll $ Gen.list simRange (Gen.element [A,B])
      let
        topEntityInput = L.zip4 switchSignal readAddresses
          (P.zipWith (\adr wr -> (adr,) <$> wr) writeAddresses writeEntries) byteSelectSignal
        simOut = simulateN @System simLength topEntity topEntityInput
        (_,expectedOut) = L.mapAccumL byteAddressableDoubleBufferedRamBehavior
          (L.head topEntityInput, contentsSingle, contentsSingle) $ L.tail topEntityInput
      -- TODO: Due to some unexpected mismatch between the expected behavior of either
      -- blockRam or the behavioral model, the boot behavior is inconsistent. We drop the first
      -- expectedOutput cycle too, we expect this is due to the resets supplied b simulateN.
      -- An issue has been made regarding this.
      L.drop 2 simOut === L.tail expectedOut

-- | This test checks if 'doubleBufferedRamByteAddressable' behaves the same as
-- 'doubleBufferedRam' when the byteEnables are always high.
doubleBufferedRamByteAddressable1 :: Property
doubleBufferedRamByteAddressable1 = property $ do
  ramDepth <- forAll $ Gen.enum 1 31
  nrOfBits <- forAll $ Gen.enum 1 31
  simLength <- forAll $ Gen.int $ Range.constant 2 100
  ramContents <- forAll $ genBlockRamContents ramDepth nrOfBits
  case ramContents of
    BitvecVec SNat SNat (contentsSingle :: Vec memDepth (BitVector bits)) -> do
      let
        contentsDouble = concatMap (replicate d2) contentsSingle
        simRange = Range.singleton simLength
        topEntity (unbundle -> (switch, readAddr, writePort)) =
          withClockResetEnable @System clockGen resetGen enableGen $ bundle
          ( doubleBufferedRamByteAddressable (Vec contentsDouble) switch readAddr writePort (pure maxBound)
          , doubleBufferedRam (Vec contentsDouble) switch readAddr writePort)
      writeAddresses <- forAll $ Gen.list simRange $ genIndex Range.constantBounded
      readAddresses <- forAll $ Gen.list simRange $ genIndex Range.constantBounded
      writeEntries <- forAll (Gen.list simRange $ Gen.maybe genDefinedBitVector)
      switchSignal <- forAll $ Gen.list simRange (Gen.element [A,B])
      let
        topEntityInput = L.zip3 switchSignal readAddresses
          (P.zipWith (\adr wr -> (adr,) <$> wr) writeAddresses writeEntries)
        simOut = simulateN @System simLength topEntity topEntityInput
        (duvOut, refOut) = L.unzip simOut
      duvOut === refOut

-- | This test checks that we can generate a 'registerByteAddressable' that stores a
-- configurable amount of bytes and selectively update its contents on a per byte basis.
readWriteRegisterByteAddressable :: Property
readWriteRegisterByteAddressable = property $ do
  nBytes <- forAll $ Gen.enum 1 10
  case TN.someNatVal nBytes of
    SomeNat p -> case compareSNat d1 (snatProxy p) of
      SNatLE -> go p
      _ -> error "readWriteRegisterByteAddressable: Amount of nBytes == 0."
 where
  go ::
    forall nBytes m .
    ( KnownNat nBytes
    , 1 <= nBytes, KnownNat (nBytes*8)
    , 1 <= (nBytes * 8), Monad m) =>
    Proxy nBytes ->
    PropertyT m ()
  go Proxy =
    case sameNat (Proxy @nBytes) (Proxy @(Regs (Vec nBytes Byte) 8)) of
      Just Refl -> do
        simLength <- forAll $ Gen.enum 1 100
        let
          writeGen = genNonEmptyVec @_ @nBytes $ genDefinedBitVector @8
        initVal <- forAll writeGen
        writes <- forAll $ Gen.list (Range.singleton simLength) writeGen
        byteEnables <- forAll $ Gen.list (Range.singleton simLength)
          $ genDefinedBitVector @(Regs (Vec nBytes Byte) 8)
        let
          topEntity (unbundle -> (newVal, byteEnable))=
            withClockResetEnable @System clockGen resetGen enableGen $
            registerByteAddressable initVal newVal byteEnable
          expectedOut = P.scanl useByteEnable initVal $ P.zip writes byteEnables
          useByteEnable olds (news,unpack -> enables) =
            (\(enable,old,new) -> if enable then new else old) <$> zip3 enables olds news
          simOut = simulateN simLength topEntity $ P.zip writes byteEnables
        simOut === P.take simLength expectedOut
      _ -> error "readWriteRegisterByteAddressable: Amount of bytes not equal to registers required."

-- | This test checks that 'registerWb' can be written to and read from via its wishbone bus.
-- This test makes sure that writing and reading with the wishbone bus works both with
-- 'CircuitPriority' and 'WishbonePriority' enabled. During this test the circuit input does
-- not write to the register.
registerWbSigToSig :: Property
registerWbSigToSig = property $ do
  bits <- forAll $ Gen.enum 1 100
  case TN.someNatVal bits of
    SomeNat p -> case compareSNat d1 (snatProxy p) of
      SNatLE -> go p
      _ -> error "registerWbSigToSig: Amount of bits == 0."
 where
  go :: forall bits m . (KnownNat bits, 1 <= bits, Monad m) => Proxy bits -> PropertyT m ()
  go Proxy = case compareSNat d1 $ SNat @(Regs (BitVector bits) 32) of
    SNatLE -> do
      initVal <- forAll $ genDefinedBitVector @bits
      writes <- forAll $ Gen.list (Range.constant 1 25) $ genDefinedBitVector @bits
      let
        simLength = L.length writes + 1
        someReg prio sigIn = fst $ withClockResetEnable clockGen resetGen enableGen
          $ registerWbSpecVal @_ @_ @4 @32 prio initVal (pure emptyWishboneM2S) sigIn
        topEntity sigIn = bundle (someReg CircuitPriority sigIn, someReg WishbonePriority sigIn)
        topEntityInput = (Just <$> writes) <> [Nothing]
        simOut = simulateN @System simLength topEntity topEntityInput
        (fstOut, sndOut) = L.unzip simOut
      footnote . fromString $ "simOut: " <> showX simOut
      footnote . fromString $ "input:" <> showX topEntityInput
      footnote . fromString $ "expected" <> showX writes
      fstOut === sndOut
      writes === L.tail fstOut
    _ -> error "registerWbSigToSig: Registers required to store bitvector == 0."

-- | This test checks that 'registerWb' can be written to with the wishbone bus and read from
-- with the circuit output. This test makes sure that the behavior with 'CircuitPriority'
-- and 'WishbonePriority' is identical. During this test the circuit input does not write
-- to the register.
registerWbWbToSig :: Property
registerWbWbToSig = property $ do
  bits <- forAll $ Gen.enum 1 100
  case TN.someNatVal bits of
    SomeNat p -> case compareSNat d1 (snatProxy p) of
      SNatLE -> go p
      _ -> error "registerWbWbToSig: Amount of bits == 0."
 where
  go :: forall bits m . (KnownNat bits, 1 <= bits, Monad m) => Proxy bits -> PropertyT m ()
  go Proxy = case compareSNat d1 $ SNat @(Regs (BitVector bits) 32) of
    SNatLE -> do
      let regs = (natToNum @(DivRU bits 32))
      initVal <- forAll $ genDefinedBitVector @bits
      writes <- forAll $ Gen.list (Range.constant 1 25) $ genDefinedBitVector @bits
      let
        simLength = L.length writes * regs + 2
        someReg prio wbIn = fst $ withClockResetEnable clockGen resetGen enableGen $
         registerWbSpecVal @System @_ @4 @32 prio initVal wbIn (pure Nothing)
        topEntity wbIn = bundle (someReg CircuitPriority wbIn, someReg WishbonePriority wbIn)
        topEntityInput = L.concatMap wbWrite writes <> L.repeat emptyWishboneM2S
        simOut = simulateN simLength topEntity topEntityInput
        (fstOut, sndOut) = L.unzip simOut
        filteredOut = everyNth regs $ L.tail fstOut

      footnote . fromString $ "simOut: " <> showX simOut
      footnote . fromString $ "filteredOut:" <> showX filteredOut
      footnote . fromString $ "input:" <> showX (L.take simLength topEntityInput)
      footnote . fromString $ "expected" <> showX writes
      fstOut === sndOut
      writes === L.take (L.length writes) filteredOut
    _ -> error "registerWbWbToSig: Registers required to store bitvector == 0."
   where
    wbWrite v = L.zipWith bv2WbWrite [0.. L.length l - 1] $ L.reverse l
     where
      RegisterBank (toList -> l) = paddedToRegisters $ Padded v
  everyNth n l  | L.length l >= n = x : everyNth n xs
                | otherwise = []
   where
    (x:xs) = L.drop (n-1) l

-- | This test checks that 'registerWb' can be written to by the circuit and read from
-- with the wishbone bus. This test makes sure that the behavior with 'CircuitPriority'
-- and 'WishbonePriority' is identical. During this test the wishbone bus does not write
-- to the register.
registerWbSigToWb :: Property
registerWbSigToWb = property $ do
  bits <- forAll $ Gen.enum 1 100
  case TN.someNatVal bits of
    SomeNat p -> case compareSNat d1 (snatProxy p) of
      SNatLE -> go p
      _ -> error "registerWbSigToWb: Amount of bits == 0."
 where
  go :: forall bits m . (KnownNat bits, 1 <= bits, Monad m) => Proxy bits -> PropertyT m ()
  go Proxy = case compareSNat d1 $ SNat @(Regs (BitVector bits) 32) of
    SNatLE -> do
      initVal <- forAll $ genDefinedBitVector @bits
      writes <- forAll $ Gen.list (Range.constant 1 25) $ genDefinedBitVector @bits
      let
        someReg prio sigIn wbIn = snd $ withClockResetEnable clockGen resetGen enableGen
          $ registerWbSpecVal @_ @_ @4 @32 prio initVal wbIn sigIn
        topEntity (unbundle -> (sigIn, wbIn)) = bundle
          (someReg CircuitPriority sigIn wbIn, someReg WishbonePriority sigIn wbIn)
        padWrites x = L.take (natToNum @(Regs (BitVector bits) 32)) $ Just x : L.repeat Nothing
        readOps = emptyWishboneM2S : cycle
          (wbRead <$> L.reverse [(0 :: Int).. (natToNum @(Regs (BitVector bits) 32)-1)])
        topEntityInput = L.zip (L.concatMap padWrites writes <> [Nothing]) readOps
        simLength = L.length topEntityInput
        simOut = simulateN @System simLength topEntity topEntityInput
        (fstOut, sndOut) = L.unzip simOut
      footnote . fromString $ "simOut: " <> showX simOut
      footnote . fromString $ "input:" <> showX topEntityInput
      footnote . fromString $ "expected" <> showX writes
      postProcWb fstOut === postProcWb sndOut
      writes === wbDecoding (L.tail fstOut)
    _ -> error "registerWbSigToWb: Registers required to store bitvector == 0."
   where
    wbDecoding :: ([WishboneS2M (BitVector 32)] -> [BitVector bits])
    wbDecoding (wbNow:wbRest)
      | acknowledge wbNow = entry : wbDecoding rest
      | otherwise         = wbDecoding wbRest
     where
      (fmap readData -> entryList, rest) =
        L.splitAt (natToNum @(Regs (BitVector bits) 32)) (wbNow:wbRest)
      entry = case V.fromList entryList of
        Just (vec :: Vec (Regs (BitVector bits) 32) (BitVector 32))
          -> registersToData @(BitVector bits) @32 (RegisterBank vec)
        Nothing
          -> error $ "wbDecoding: list to vector conversion failed: "
          <> show entryList <> "from " <> show (wbNow:wbRest)

    wbDecoding [] = []
    wbRead i = (emptyWishboneM2S @32)
      { addr = resize (pack i) ++# (0b00 :: BitVector 2)
      , busCycle = True
      , busSelect = maxBound
      , strobe = True
      }
    postProcWb (WishboneS2M{..} : wbRest)
      | acknowledge = Just readData : postProcWb wbRest
      | err         = Nothing : postProcWb wbRest
      | otherwise   = postProcWb wbRest
    postProcWb _ = []

-- | This test checks that the behavior of 'registerWb' matches the set priorities when
-- a write conflict occurs. It is expected that with 'WishbonePriority', the circuit
-- ignores write operations from the circuit during a wishbone write operation.
-- With 'CircuitPriority', wishbone write operations are acknowledged, but silently
-- ignored during a circuit write cycle.
registerWbWriteCollisions :: Property
registerWbWriteCollisions = property $ do
  bits <- forAll $ Gen.enum 1 32
  case TN.someNatVal bits of
    SomeNat p -> case compareSNat d1 (snatProxy p) of
      SNatLE -> go p
      _ -> error "registerWbWriteCollisions: Amount of bits == 0."
 where
  go :: forall bits m . (KnownNat bits, 1 <= bits, Monad m) => Proxy bits -> PropertyT m ()
  go Proxy = case compareSNat d1 $ SNat @(Regs (BitVector bits) 32) of
    SNatLE -> do
      initVal <- forAll $ genDefinedBitVector @bits
      writeAmount <- forAll $ Gen.enum 1 25
      sigWrites <- forAll $ Gen.list (Range.singleton writeAmount) $ genDefinedBitVector @bits
      wbWrites <- forAll $ Gen.list (Range.singleton writeAmount) $ genDefinedBitVector @bits
      let
        simLength = writeAmount + 1
        someReg prio sigIn wbIn = fst $ withClockResetEnable clockGen resetGen enableGen $
         registerWbSpecVal @System @_ @4 @32 prio initVal wbIn sigIn
        topEntity (unbundle -> (sigIn, wbIn)) = bundle
          (someReg CircuitPriority sigIn wbIn, someReg WishbonePriority sigIn wbIn)
        topEntityInput = L.zip (Just <$> sigWrites)
          (L.concatMap wbWrite wbWrites <> L.repeat emptyWishboneM2S)
        simOut = simulateN simLength topEntity topEntityInput
        (fstOut, sndOut) = L.unzip simOut

      footnote . fromString $ "WishbonePrio out: " <> showX sndOut
      footnote . fromString $ "CircuitPrio out: " <> showX fstOut
      footnote . fromString $ "input:" <> showX (L.take simLength topEntityInput)
      footnote . fromString $ "wbIn" <> showX wbWrites
      footnote . fromString $ "sigIn" <> showX sigWrites
      sigWrites === L.tail fstOut
      wbWrites  === L.tail sndOut
    _ -> error "registerWbWriteCollisions: Registers required to store bitvector == 0."
   where
    wbWrite v = L.zipWith bv2WbWrite (L.reverse [0.. L.length l - 1]) l
     where
      RegisterBank (toList -> l) = paddedToRegisters $ Padded v

bv2WbWrite :: (BitPack a, Enum a) =>
  a
  -> ("DAT_MOSI" ::: BitVector 32)
  -> WishboneM2S 32 4 (BitVector 32)
bv2WbWrite i v = (emptyWishboneM2S @32 @(BitVector 32))
  { addr = resize (pack i) ++# (0b00 :: BitVector 2)
  , writeData = v
  , writeEnable = True
  , busCycle = True
  , strobe = True
  , busSelect = maxBound
  }

-- | Model for 'byteAddressableRam', it stores the inputs in its state for a one cycle delay
-- and updates the Ram based on the the write operation and byte enables.
-- Furthermore it contains read-before-write behavior based on the readAddr.
byteAddressableRamBehavior :: forall bits memDepth nBytes .
  (KnownNat memDepth, 1 <= memDepth
  , KnownNat nBytes, 1 <= nBytes
  , nBytes ~ Regs (BitVector bits) 8
  , KnownNat bits, 1 <= bits) =>

  ((Index memDepth, Maybe (LocatedBits memDepth bits), BitVector nBytes)
  , Vec memDepth (BitVector bits))->

  (Index memDepth, Maybe (LocatedBits memDepth bits), BitVector nBytes) ->

  ((( Index memDepth
    , Maybe (LocatedBits memDepth bits)
    , BitVector nBytes)
   , Vec memDepth (BitVector bits))
  , BitVector bits)
byteAddressableRamBehavior state input = (state', ram !! readAddr)
 where
  ((readAddr, writeOp, byteEnable), ram) = state
  (writeAddr, writeData0) = fromMaybe (0, 0b0) writeOp
  writeTrue = isJust writeOp
  RegisterBank oldData = getRegs $ ram !! writeAddr
  RegisterBank newData = getRegs writeData0
  newEntry = getData $ zipWith (\ sel (old,new) -> if sel then new else old) (unpack byteEnable) $
   zip oldData newData


  getData :: Vec nBytes Byte -> BitVector bits
  getData vec = registersToData @_ @8 $ RegisterBank vec

  ram1 = if writeTrue then replace writeAddr newEntry ram else ram
  state' = (input, ram1)

-- | Model for 'byteAddressableDoubleBufferedRamBehavior', it stores the inputs in its
-- state for a one cycle delay and updates the Ram based on the the write operation and
-- byte enables. Furthermore it contains read-before-write behavior based on the readAddr.
-- The only addition compared to byteAddressableRam is the fact that there's two buffers
-- (one read only, one write only), that can be swapped.
byteAddressableDoubleBufferedRamBehavior :: forall bits memDepth nBytes .
 ( KnownNat memDepth
 , 1 <= memDepth
 , KnownNat nBytes
 , 1 <= nBytes
 , nBytes ~ Regs (BitVector bits) 8
 , KnownNat bits
 , 1 <= bits) =>
 ((AorB, Index memDepth, Maybe (LocatedBits memDepth bits), BitVector nBytes)
 , Vec memDepth (BitVector bits), Vec memDepth (BitVector bits))->

 (AorB, Index memDepth, Maybe (LocatedBits memDepth bits), BitVector nBytes) ->

 (((AorB, Index memDepth, Maybe (LocatedBits memDepth bits), BitVector nBytes)
 , Vec memDepth (BitVector bits)
 , Vec memDepth (BitVector bits))
 , BitVector bits)
byteAddressableDoubleBufferedRamBehavior state input = (state', out)
 where
  ((switchBuffers, readAddr, writeOp, byteEnable), bufA0, bufB0) = state
  (out, bufA1, bufB1)
    | switchBuffers == B = (bufA0 !! readAddr, bufA0, updateEntry bufB0 writeOp)
    | otherwise  = (bufB0 !! readAddr, updateEntry bufA0 writeOp, bufB0)

  updateEntry buf op
    | isJust writeOp = replace writeAddr newEntry buf
    | otherwise = buf
   where
    newEntry = getNewEntry (buf !! writeAddr) writeData0
    (writeAddr, writeData0) = fromMaybe (0, 0b0) op

  getNewEntry old new = getData $ zipWith
    (\ sel (a,b) -> if sel then b else a)
    (unpack byteEnable) $ zip oldData newData
   where
    RegisterBank oldData = getRegs old
    RegisterBank newData = getRegs new

  state' = (input, bufA1, bufB1)

  getData :: Vec nBytes Byte -> BitVector bits
  getData vec = registersToData @_ @8 $ RegisterBank vec


-- | Version of 'Bittide.DoubleBufferedRam.registerWb' which performs wishbone
--   spec validation.
registerWbSpecVal ::
  forall dom a nBytes addrW .
  ( HiddenClockResetEnable dom
  , Paddable a
  , KnownNat nBytes
  , 1 <= nBytes
  , KnownNat addrW
  , 2 <= addrW
  , BitPack a
  , ShowX a) =>
  -- | Determines the write priority on write collisions
  RegisterWritePriority ->
  -- | Initial value.
  a ->
  -- | Wishbone bus (master to slave)
  Signal dom (WishboneM2S addrW nBytes (Bytes nBytes)) ->
  -- | New circuit value.
  Signal dom (Maybe a) ->
  -- |
  -- 1. Outgoing stored value
  -- 2. Outgoing wishbone bus (slave to master)
  (Signal dom a, Signal dom (WishboneS2M (Bytes nBytes)))
registerWbSpecVal writePriority initVal m2s0 sigIn = (storedVal, s2m1)
 where
  (storedVal, s2m0) = registerWb @dom @a @nBytes @addrW writePriority initVal m2s1 sigIn
  (m2s1, s2m1) = validateWb m2s0 s2m0

-- | Generate an input vector for 'contentGenerator' and check if it generates write
-- operations from this vector.
testContentGen :: Property
testContentGen = property $ do
  nat <- forAll $ Gen.enum 0 32
  case TN.someNatVal nat of
    SomeNat n -> go n
 where
  go (Proxy :: Proxy v) = do
    content <- forAll $ genVec @_ @v (genUnsigned @_ @32 Range.constantBounded)
    let
      l = length content
      simLength = 2 + l * 2
      !(writes, dones) = L.unzip $ sampleN @System simLength
        (bundle $ contentGenerator @_ @v @(v +1) (Vec content))
      expectedDones
        | l == 0 = L.repeat True
        | otherwise = L.replicate (l + 2) False <> L.repeat True
    dones === L.take simLength expectedDones
    catMaybes writes === toList (zip (iterateI succ 0) content)

wbStorageSpecCompliance :: Property
wbStorageSpecCompliance = property $ do
  nat <- forAll $ Gen.enum 1 32
  case TN.someNatVal (nat - 1) of
    SomeNat (succSNat . snatProxy -> n) -> go n

  where
    go :: forall v m . (KnownNat v, 1 <= v, Monad m) => SNat v -> PropertyT m ()
    go SNat = do
      content <- forAll $ genNonEmptyVec @_ @v (genDefinedBitVector @32)
      withClockResetEnable clockGen resetGen enableGen $
        wishbonePropWithModel @System
          defExpectOptions
          (\_ _ () -> Right ())
          (wbStorage (Reloadable $ Vec content))
          genRequests
          ()

    genRequests = Gen.list (Range.linear 0 32)
      (genWishboneTransfer @32 (genDefinedBitVector @32))

    genWishboneTransfer ::
      (KnownNat addressWidth, KnownNat (BitSize a)) =>
      Gen a ->
      Gen (WishboneMasterRequest addressWidth a)
    genWishboneTransfer genA =
      Gen.choice
        [ Read <$> genDefinedBitVector <*> genDefinedBitVector ,
          Write <$> genDefinedBitVector <*> genDefinedBitVector <*> genA
        ]

deriving instance ShowX a => ShowX (RamOp i a)

-- | Behavioral test for 'wbStorage', it checks whether the behavior of 'wbStorage' matches
-- the 'wbStorageBehaviorModel'.
wbStorageBehavior :: Property
wbStorageBehavior = property $ do
  nat <- forAll $ Gen.enum 1 32
  case TN.someNatVal (nat - 1) of
    SomeNat (succSNat . snatProxy -> n) -> go n
 where
  go :: forall v m . (KnownNat v, 1 <= v, Monad m) => SNat v -> PropertyT m ()
  go SNat = do
    content <- forAll $ genNonEmptyVec @_ @v genDefinedBitVector
    goldenInput <- forAll $ (\l -> RamNoOp : l <> [RamNoOp]) <$>
      Gen.list (Range.linear 1 32)
        (Gen.choice
          [ pure RamNoOp
          , RamRead <$> genIndex @_ @v Range.constantBounded
          , RamWrite <$> genIndex @_ @v Range.constantBounded <*> genDefinedBitVector
          ])
    let
      goldenRef ramOps = wbStorageBehaviorModel contentList fstGolden (fromIntegral <$> readAddr) writeOp
       where
        (readAddr , writeOp) = unbundle $ decodeRamOp <$> ramOps
        contentList = toList $ concatMap (\(split ->(a,b)) -> b :> a :> Nil) content
        fstGolden = deepErrorX "First element undefined."

      topEntity wbIn =
        wcre @System $ wbStorage' @_ @_ @32 (NonReloadable $ Vec content) wbIn
      topEntityInput = L.concatMap (\a ->[a, a, emptyWishboneM2S]) (ramOpToWb <$> goldenInput)
      simGolden = simulateN @System (P.length goldenInput) goldenRef goldenInput
      simOut = simulateN (P.length topEntityInput) topEntity topEntityInput
      goldenTransactions = ramOpToTransaction goldenInput $ L.tail simGolden
      simTransactions = wbToTransaction topEntityInput simOut

    footnote $ "goldenTransactions" <> showX goldenTransactions
    footnote $ "simTransactions" <> showX simTransactions
    footnote $ "simGolden" <> showX simGolden
    footnote $ "simOut" <> showX simOut
    footnote $ "goldenInput" <> showX goldenInput
    footnote $ "topEntityInput" <> showX topEntityInput

    simTransactions === goldenTransactions

  decodeRamOp :: RamOp i a -> (Index i, Maybe (Located i a))
  decodeRamOp = \case
    RamNoOp -> (addrUndef, Nothing)
    RamRead i -> (i, Nothing)
    RamWrite i a -> (i, Just (i, a))
   where
    addrUndef = deepErrorX "wbStorageBehavior: readAddr undefined."

-- | Behavioral model for 'wbStorage'. It stores is contents as half-words in Little Endian.
wbStorageBehaviorModel
  :: (KnownNat i, KnownNat m)
  => [BitVector m]
  -> BitVector (m+m)
  -> Signal dom (Index i)
  -> Signal dom (Maybe (LocatedBits i (m+m)))
  -> Signal dom (BitVector (m + m))
wbStorageBehaviorModel storedList output (address :- addresses) (writeOp :- writeOps)
  = output :- outputs
 where
  (preEntry, lower0 : upper0 : postEntry) = L.splitAt (fromIntegral address) storedList
  (_,split -> (upper, lower)) = fromJust writeOp
  newList
    | isJust writeOp = preEntry L.++ [lower, upper] L.++ postEntry
    | otherwise = storedList
  nextOutput = pack (upper0, lower0)
  outputs = wbStorageBehaviorModel newList nextOutput addresses writeOps

wcre :: KnownDomain dom => (HiddenClockResetEnable dom => r) -> r
wcre = withClockResetEnable clockGen resetGen enableGen
