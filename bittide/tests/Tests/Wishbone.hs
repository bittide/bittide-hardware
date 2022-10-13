-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fconstraint-solver-iterations=5 #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Tests.Wishbone(memMapGroup) where

import Clash.Prelude

import Clash.Hedgehog.Sized.Index
import Clash.Hedgehog.Sized.Vector
import Clash.Sized.Vector(unsafeFromList)
import Data.Constraint (Dict(Dict))
import Data.Constraint.Nat.Extra (cancelMulDiv, divWithRemainder)
import Data.Maybe
import Data.String
import Hedgehog
import Hedgehog.Range as Range
import Protocols (toSignals, Circuit(..), (|>))
import Protocols.Wishbone
import Protocols.Wishbone.Standard.Hedgehog (validatorCircuit)
import Test.Tasty
import Test.Tasty.Hedgehog

import Bittide.SharedTypes
import Bittide.Wishbone
import Tests.Shared

import qualified Data.List as L
import qualified Data.Set as Set
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen

memMapGroup :: TestTree
memMapGroup = testGroup "Memory Map group"
  [ testPropertyNamed "Reading readData from slaves." "readingSlaves" readingSlaves
  , testPropertyNamed "Writing and reading from slaves." "writingSlaves" writingSlaves
  ]

-- | generates a 'MemoryMap' for 'singleMasterInterconnect'.
genConfig ::
  forall nSlaves .
  SNat nSlaves ->
  Gen (MemoryMap nSlaves)
genConfig SNat = do
  let
    s = Gen.set (Range.singleton $ natToNum @nSlaves) (genIndex Range.constantBounded)
    l = Set.toList <$> s
  unsafeFromList <$> (Gen.shuffle =<< l)

-- | Creates a memory map with 'simpleSlave' devices and a list of read addresses and checks
-- if the correct 'simpleSlave' responds to the read operation. Reading outside of a 'simpleSlave' its
-- range should return an err.
readingSlaves :: Property
readingSlaves = property $ do
  devices <- forAll $ Gen.enum 2 16
  case TN.someNatVal (devices - 1)of
   SomeNat (succSNat . snatProxy -> devices0@SNat) ->
    case compareSNat (clogBaseSNat d2 devices0) d32 of
      SNatLE -> runTest devices0
      _      -> errorX "readingSlaves: number of devices can't be represented with 32 bits."
 where
  runTest devices = do
    config <- forAll $ genConfig @_ devices
    nrOfReads <- forAll $ Gen.enum 1 32
    let nrOfReadsRange = Range.singleton nrOfReads
    readAddresses <- forAll $ Gen.list nrOfReadsRange (genDefinedBitVector @32)
    ranges <- forAll $ genVec genDefinedBitVector
    let
      topEntityInput = (wbRead <$> readAddresses) <> [emptyWishboneM2S]
      simOut = simulateN (L.length topEntityInput) (topEntity config ranges) topEntityInput
      realTransactions = wbToTransaction topEntityInput simOut
      expectedOutput = fmap (getExpected config ranges) topEntityInput
      expectedTransactions = wbToTransaction topEntityInput expectedOutput
    footnote . fromString $ "expectedTransactions: " <> showX expectedTransactions
    footnote . fromString $ "realTransactions: " <> showX realTransactions
    footnote . fromString $ "simOut: " <> showX simOut
    footnote . fromString $ "simIn: " <> showX topEntityInput
    footnote . fromString $ "reads: " <> show readAddresses
    footnote . fromString $ "ranges: " <> show ranges
    footnote . fromString $ "config: " <> show config
    realTransactions === expectedTransactions

  topEntity config ranges masterIn = toMaster
   where
    slaves = withClockResetEnable @System clockGen resetGen enableGen simpleSlave <$>
      ranges <*> config <*> unbundle toSlaves
    (toMaster, toSlaves) = withClockResetEnable clockGen resetGen enableGen
      singleMasterInterconnect' config masterIn $ bundle slaves

  getExpected ::
    forall nSlaves .
    ( KnownNat nSlaves, 1 <= nSlaves, BitSize (Index nSlaves) <= 32
    , BitSize (Index nSlaves) <= 32) =>
    Vec nSlaves (Index nSlaves) ->
    Vec nSlaves (BitVector (32 - BitSize (Index nSlaves))) ->
    WishboneM2S 32 (Regs (Index nSlaves) 8) (Index nSlaves) ->
    WishboneS2M (Index nSlaves)
  getExpected config ranges WishboneM2S{..}
    | commAttempt && isMapped && inRange =
      (emptyWishboneS2M @(Index nSlaves)){acknowledge, readData}
    | commAttempt && isMapped            = emptyWishboneS2M{err}
    | otherwise                          = emptyWishboneS2M
   where
    (readData, acknowledge, err) = (index, True, True)
    (indexBV, restAddr) = split addr
    index = unpack indexBV
    commAttempt = busCycle && strobe
    isMapped = bitCoerce (resize indexBV) < length config
    range = ranges !! fromJust (elemIndex index config)
    inRange = restAddr <= range

-- | Creates a memory map with 'simpleSlave' devices and a list of write addresses and checks
-- that if we 'simpleSlave' responds to the read operation. Reading outside of a 'simpleSlave' its
-- range should return an err.
writingSlaves :: Property
writingSlaves = property $ do
  devices <- forAll $ Gen.enum 1 16
  case TN.someNatVal (devices - 1)of
   SomeNat (succSNat . snatProxy -> devices0) ->
    case compareSNat (clogBaseSNat d2 devices0 ) d32 of
      SNatLE -> runTest devices0
      _      -> errorX "readingSlaves: number of devices can't be represented with 32 bits."
 where

  runTest devices = do
    config <- forAll $ genConfig @_ devices
    nrOfWrites <- forAll $ Gen.enum 1 32
    let nrOfWritesRange = Range.singleton nrOfWrites
    writeAddresses <- forAll $ Gen.list nrOfWritesRange genDefinedBitVector
    ranges <- forAll $ genVec genDefinedBitVector
    let
      topEntityInput = L.concatMap wbWriteThenRead writeAddresses <> [emptyWishboneM2S ]
      simLength = L.length topEntityInput
      simOut = simulateN simLength (topEntity config ranges) topEntityInput
      realTransactions = wbToTransaction topEntityInput simOut
      expectedOutput = fmap (getExpected config ranges) topEntityInput
      expectedTransactions = wbToTransaction topEntityInput expectedOutput
    footnote . fromString $ "expectedTransactions: " <> showX expectedTransactions
    footnote . fromString $ "realTransactions: " <> showX realTransactions
    footnote . fromString $ "simOut: " <> showX simOut
    footnote . fromString $ "simIn: " <> showX topEntityInput
    footnote . fromString $ "writes: " <> show writeAddresses
    footnote . fromString $ "ranges: " <> show ranges
    footnote . fromString $ "config: " <> show config
    realTransactions === expectedTransactions

  topEntity config ranges masterIn = toMaster
   where
    slaves = withClockResetEnable @System clockGen resetGen enableGen simpleSlave <$>
      ranges <*> ranges <*> unbundle toSlaves
    (toMaster, toSlaves) = withClockResetEnable clockGen resetGen enableGen
      singleMasterInterconnect' config masterIn $ bundle slaves

  wbWriteThenRead a = [wbWrite a (resize a), wbRead a]

  getExpected ::
    forall nSlaves .
    ( KnownNat nSlaves, 1 <= nSlaves, BitSize (Index nSlaves) <= 32
    , BitSize (Index nSlaves) <= 32) =>
    Vec nSlaves (Index nSlaves) ->
    Vec nSlaves (BitVector (32 - BitSize (Index nSlaves))) ->
    WishboneM2S 32 (DivRU (32 - BitSize (Index nSlaves)) 8)
      (BitVector (32 - BitSize (Index nSlaves))) ->
    WishboneS2M (BitVector (32 - BitSize (Index nSlaves)))
  getExpected config ranges WishboneM2S{..}
    | commAttempt && isMapped && inRange && writeEnable =
      emptyWishboneS2M{acknowledge}
    | commAttempt && isMapped && inRange                =
      (emptyWishboneS2M @(Index nSlaves)){acknowledge, readData}
    | commAttempt && isMapped                           =
      (emptyWishboneS2M @(BitVector (32 - BitSize (Index nSlaves)))){err}
    | otherwise                                         = emptyWishboneS2M
   where
    (readData, acknowledge, err) = (restAddr, True, True)
    (indexBV, restAddr) = split @_ @(BitSize (Index nSlaves)) addr
    index = unpack indexBV
    commAttempt = busCycle && strobe
    isMapped = bitCoerce (resize indexBV) < length config
    range = ranges !! fromJust (elemIndex index config)
    inRange = restAddr <= range

-- | transforms an address to a 'WishboneM2S' read operation.
wbRead
  :: forall addressWidth a
  . (KnownNat addressWidth, NFDataX a, KnownNat (BitSize a))
  => BitVector addressWidth
  -> WishboneM2S addressWidth (Regs a 8) a
wbRead address = case cancelMulDiv @(Regs a 8) @8 of
  Dict ->
    (emptyWishboneM2S @addressWidth)
    { addr = address
    , strobe = True
    , busCycle = True
    , busSelect = maxBound
    }

-- | transforms an address to a 'WishboneM2S' write operation that writes the given address
-- to the given address.
wbWrite
  :: forall addressWidth a
  . (KnownNat addressWidth, NFDataX a, KnownNat (BitSize a))
  => BitVector addressWidth
  -> a
  -> WishboneM2S addressWidth (Regs a 8) a
wbWrite address a = (emptyWishboneM2S @addressWidth @a)
  { addr = address
  , strobe = True
  , busCycle = True
  , writeData = a
  , writeEnable = True
  , busSelect = maxBound
  }

simpleSlave' ::
  forall dom aw a .
  (HiddenClockResetEnable dom, KnownNat aw, NFDataX a) =>
  BitVector aw ->
  a ->
  Circuit (Wishbone dom 'Standard aw a) ()
simpleSlave' range readData0 = Circuit $ \(wbIn, ()) -> (mealy go readData0 wbIn, ())
 where
  go readData1 WishboneM2S{..} =
    (readData2, (emptyWishboneS2M @a){readData, acknowledge, err})
   where
     masterActive = busCycle && strobe
     addrInRange = addr <= range
     acknowledge = masterActive && addrInRange
     err = masterActive && not addrInRange
     writeOp = acknowledge && writeEnable
     readData2 | writeOp    = writeData
               | otherwise  = readData1
     readData | writeOp     = writeData
              | otherwise   = readData1


-- | Simple wishbone slave that responds to addresses [0..range], it responds by returning
-- a stored value (initialized by readData0), which can be overwritten by the wishbone bus.
-- any read/write attempt to an address outside of the supplied range sets the err signal.
simpleSlave ::
  forall dom aw a .
  (HiddenClockResetEnable dom, KnownNat aw, BitPack a, NFDataX a, ShowX a) =>
  BitVector aw ->
  a ->
  Signal dom (WishboneM2S aw (Regs a 8) a) ->
  Signal dom (WishboneS2M a)
simpleSlave range readData wbIn =
  case divWithRemainder @(Regs a 8) @8 @7 of
    Dict -> fst $ toSignals circuit (wbIn, ())
 where
  circuit = validatorCircuit |> simpleSlave' range readData
