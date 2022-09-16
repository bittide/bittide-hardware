-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tests.Wishbone(memMapGroup) where

import Clash.Prelude

import Clash.Hedgehog.Sized.BitVector
import Clash.Hedgehog.Sized.Vector
import Clash.Sized.Vector(unsafeFromList)
import Data.Constraint (Dict(Dict))
import Data.Constraint.Nat.Extra (timesNDivRU, timesNDivRU'')
import Data.Proxy
import Data.String
import Hedgehog
import Hedgehog.Range as Range
import Protocols (toSignals, Circuit(..), (|>))
import Protocols.Wishbone
import Protocols.Wishbone.Standard.Hedgehog (validatorCircuit)
import Test.Tasty
import Test.Tasty.Hedgehog

import Bittide.SharedTypes
import Bittide.Wishbone ( MemoryMap, singleMasterInterconnect' )

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
  forall nSlaves aw .
  (KnownNat nSlaves, KnownNat aw) =>
  Proxy nSlaves ->
  Gen (MemoryMap nSlaves aw)
genConfig = do
  let s = Gen.set (Range.singleton $ natToNum @nSlaves) genDefinedBitVector
  return $ unsafeFromList . Set.elems <$> s

-- | Creates a memory map with 'simpleSlave' devices and a list of read addresses and checks
-- if the correct 'simpleSlave' responds to the read operation. Reading outside of a 'simpleSlave' its
-- range should return an err.
readingSlaves :: Property
readingSlaves = property $ do
  devices <- forAll $ Gen.enum 1 32
  case TN.someNatVal devices of
   SomeNat devices0 -> runTest devices0
 where
  runTest devices = do
    config <- forAll $ genConfig @_ @32 devices
    nrOfReads <- forAll $ Gen.enum 1 100
    let nrOfReadsRange = Range.singleton nrOfReads
    readAddresses <- forAll . Gen.list nrOfReadsRange $ Gen.integral Range.constantBounded
    ranges <- forAll $ genVec $ Gen.integral Range.constantBounded
    let
      topEntityInput = (wbRead <$> readAddresses) <> [emptyWishboneM2S]
      simOut = simulateN (L.length topEntityInput) (topEntity config ranges) topEntityInput
    footnote . fromString $ "simOut: " <> showX simOut
    footnote . fromString $ "simIn: " <> showX topEntityInput
    footnote . fromString $ "reads: " <> show readAddresses
    footnote . fromString $ "ranges: " <> show ranges
    footnote . fromString $ "config: " <> show config
    fmap filterSimOut (L.tail simOut) === fmap (getExpected config ranges) readAddresses

  topEntity config ranges masterIn = toMaster
   where
    slaves = withClockResetEnable @System clockGen resetGen enableGen simpleSlave <$>
      ranges <*> config <*> unbundle toSlaves
    (toMaster, toSlaves) = withClockResetEnable clockGen resetGen enableGen
      (singleMasterInterconnect' @System @_ @4 @32) config masterIn $ bundle slaves

  filterSimOut WishboneS2M{..}
    | acknowledge && not err = Just readData
    | otherwise              = Nothing

  getExpected config ranges a
    | a >= baseAddr && (a - baseAddr) <= range = Just $ bitCoerce baseAddr
    | otherwise                                = Nothing
   where
    (baseAddr, range) = findBaseAddress a config ranges

-- | Creates a memory map with 'simpleSlave' devices and a list of write addresses and checks
-- that if we 'simpleSlave' responds to the read operation. Reading outside of a 'simpleSlave' its
-- range should return an err.
writingSlaves :: Property
writingSlaves = property $ do
  devices <- forAll $ Gen.enum 1 32
  case TN.someNatVal devices of
   SomeNat devices0 -> runTest devices0
 where
  runTest devices = do
    config <- forAll $ genConfig @_ @32 devices
    nrOfWrites <- forAll $ Gen.enum 1 100
    let nrOfWritesRange = Range.singleton nrOfWrites
    writeAddresses <- forAll . Gen.list nrOfWritesRange $ Gen.integral Range.constantBounded
    ranges <- forAll $ genVec $ Gen.integral Range.constantBounded
    let
      topEntityInput = L.concatMap wbWriteThenRead writeAddresses <> [emptyWishboneM2S @32 @(BitVector 32)]
      simLength = L.length topEntityInput
      simOut = simulateN simLength (topEntity ranges config) topEntityInput
    footnote . fromString $ "simOut: " <> showX simOut
    footnote . fromString $ "simIn: " <> showX topEntityInput
    footnote . fromString $ "writes: " <> show writeAddresses
    footnote . fromString $ "ranges: " <> show ranges
    footnote . fromString $ "config: " <> show config
    fmap filterSimOut (every2nd $ L.tail simOut) === fmap (getExpected config ranges) writeAddresses

  topEntity ranges config masterIn = toMaster
   where
    slaves = withClockResetEnable @System clockGen resetGen enableGen simpleSlave <$>
      ranges <*> config <*> unbundle toSlaves
    (toMaster, toSlaves) = withClockResetEnable clockGen resetGen enableGen
      (singleMasterInterconnect' @System @_ @4 @32) config masterIn $ bundle slaves
  filterSimOut WishboneS2M{..} | acknowledge = Just readData
                               | otherwise   = Nothing
  wbWriteThenRead a = [wbWrite a, wbRead a]
  every2nd (_:b:cs) = b : every2nd cs
  every2nd _ = []
  getExpected config ranges a
    | a >= baseAddr && (a - baseAddr) <= range = Just $ bitCoerce a
    | otherwise                                = Nothing
   where
    (baseAddr, range) = findBaseAddress a config ranges

-- | transforms an address to a 'WishboneM2S' read operation.
wbRead :: forall bs addressWidth . (KnownNat bs, KnownNat addressWidth) => BitVector addressWidth -> WishboneM2S addressWidth bs (Bytes bs)
wbRead address = case timesNDivRU @bs @8 of
  Dict ->
    (emptyWishboneM2S @addressWidth)
    { addr = address
    , strobe = True
    , busCycle = True
    , busSelect = maxBound
    }

-- | transforms an address to a 'WishboneM2S' write operation that writes the given address
-- to the given address.
wbWrite :: forall bs addressWidth . (KnownNat bs, KnownNat addressWidth) => BitVector addressWidth -> WishboneM2S addressWidth bs (Bytes bs)
wbWrite address = (emptyWishboneM2S @addressWidth @(Bytes bs))
  { addr = address
  , strobe = True
  , busCycle = True
  , writeData = resize address
  , writeEnable = True
  , busSelect = maxBound
  }

simpleSlave' ::
  forall dom aw bs .
  (HiddenClockResetEnable dom, KnownNat aw, KnownNat bs, aw ~ (bs * 8)) =>
  BitVector aw ->
  Bytes bs ->
  Circuit (Wishbone dom 'Standard aw (Bytes bs)) ()
simpleSlave' range readData0 = Circuit $ \(wbIn, ()) -> (mealy go readData0 wbIn, ())
 where
  go readData1 WishboneM2S{..} =
    (readData2, (emptyWishboneS2M @(Bytes bs)) {readData, acknowledge, err})
   where
     masterActive = strobe && busCycle
     addrInRange = addr <= range
     acknowledge = masterActive && addrInRange
     err = masterActive && not addrInRange
     writeOp = acknowledge && writeEnable
     readData2 | writeOp    = writeData
               | otherwise  = readData1
     readData | writeOp     = writeData
              | acknowledge = readData1
              | otherwise   = errorX "simpleSlave: readData is undefined because when ack is False"


-- | Simple wishbone slave that responds to addresses [0..range], it responds by returning
-- a stored value (initialized by readData0), which can be overwritten by the wishbone bus.
-- any read/write attempt to an address outside of the supplied range sets the err signal.
simpleSlave ::
  forall dom bs .
  (HiddenClockResetEnable dom, KnownNat bs) =>
  Bytes bs ->
  Bytes bs ->
  Signal dom (WishboneM2S (bs * 8) bs (Bytes bs)) ->
  Signal dom (WishboneS2M (Bytes bs))
simpleSlave range readData wbIn =
  case timesNDivRU'' @bs @8 of
    Dict -> fst $ toSignals circuit (wbIn, ())
 where
  circuit = validatorCircuit |> simpleSlave' @dom @(bs * 8) @bs range readData

-- findBaseAddress returns the base address that responds to a and its range
findBaseAddress :: Ord a => a -> Vec n a -> Vec m b -> (a, b)
findBaseAddress a (toList -> config) (toList -> ranges) =
  L.last $ (L.head config, L.head ranges) : lowerAddresses
  where
    lowerAddresses = L.takeWhile ((<=a) . fst) $ L.zip config ranges
