-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tests.MemoryMap(memMapGroup) where

import Clash.Hedgehog.Sized.BitVector
import Clash.Hedgehog.Sized.Vector
import Clash.Prelude
import Clash.Sized.Vector(unsafeFromList)

import Contranomy.Wishbone
import Data.Proxy
import Data.String
import Hedgehog
import Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Data.List as L
import qualified Data.Set as Set
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen

import Bittide.MemoryMap

memMapGroup :: TestTree
memMapGroup = testGroup "Memory Map group"
  [ testPropertyNamed "Reading readData from slaves." "readingSlaves" readingSlaves
  , testPropertyNamed "Writing and reading from slaves." "writingSlaves" writingSlaves
  ]

-- | generates a 'MemoryMap' for 'memoryMap'.
genConfig :: forall slaves aw . (KnownNat slaves, KnownNat aw) => Proxy slaves -> Gen (MemoryMap slaves aw)
genConfig = do
  let set = Gen.set (Range.singleton $ natToNum @slaves) genDefinedBitVector
  return $ unsafeFromList . Set.elems <$> set

-- | Creates a memory map with 'simpleSlave' devices and a list of read addresses and checks
-- if the correct 'simpleSlave' responds to the read operation. Reading outside of a 'simpleSlave' its
-- range should return an err.
readingSlaves :: Property
readingSlaves = property $ do
  devices <- forAll $ Gen.enum 1 32
  case TN.someNatVal devices of
   SomeNat devices0 -> do
    config <- forAll $ genConfig @_ @32 devices0
    nrOfReads <- forAll $ Gen.enum 1 100
    let nrOfReadsRange = Range.singleton nrOfReads
    readAddresses <- forAll . Gen.list nrOfReadsRange $ Gen.integral Range.constantBounded
    ranges <- forAll $ genVec $ Gen.integral Range.constantBounded
    let
      topEntity masterIn = toMaster
        where
          slaves = withClockResetEnable @System clockGen resetGen enableGen simpleSlave <$>
            ranges <*> config <*> unbundle toSlaves
          (toMaster, toSlaves) = withClockResetEnable clockGen resetGen enableGen (memoryMap
            @System @_ @4 @32) config masterIn $ bundle slaves
      topEntityInput = (wbRead <$> readAddresses) <> [idleM2S]
      simLength = L.length topEntityInput
      simOut = simulateN simLength topEntity topEntityInput
      configL = toList config
      rangesL = toList ranges
       -- findBaseAddress returns the base address that responds to a and its range
      findBaseAddress a = (L.last $ (L.head configL, L.head rangesL) : lowerAddresses)
        where
          lowerAddresses = L.takeWhile ((<a) . fst) $ L.zip configL rangesL
      getExpected a | a >= baseAddr && (a - baseAddr) <= range = Just $ bitCoerce baseAddr
                    | otherwise             = Nothing
       where
        (baseAddr, range) = findBaseAddress a
    footnote . fromString $ "simOut: " <> showX simOut
    footnote . fromString $ "simIn: " <> showX topEntityInput
    footnote . fromString $ "reads: " <> show readAddresses
    footnote . fromString $ "ranges: " <> show ranges
    footnote . fromString $ "config: " <> show config
    fmap filterSimOut (L.tail simOut) === fmap getExpected readAddresses
 where
  filterSimOut WishboneS2M{..} | acknowledge && not err = Just readData
                               | otherwise              = Nothing

-- | Creates a memory map with 'simpleSlave' devices and a list of write addresses and checks
-- that if we 'simpleSlave' responds to the read operation. Reading outside of a 'simpleSlave' its
-- range should return an err.
writingSlaves :: Property
writingSlaves = property $ do
  devices <- forAll $ Gen.enum 1 32
  case TN.someNatVal devices of
   SomeNat devices0 -> do
    config <- forAll $ genConfig @_ @32 devices0
    nrOfWrites <- forAll $ Gen.enum 1 100
    let nrOfWritesRange = Range.singleton nrOfWrites
    writeAddresses <- forAll . Gen.list nrOfWritesRange $ Gen.integral Range.constantBounded
    ranges <- forAll $ genVec $ Gen.integral Range.constantBounded
    let
      topEntity masterIn = toMaster
        where
          slaves = withClockResetEnable @System clockGen resetGen enableGen simpleSlave <$>
            ranges <*> config <*> unbundle toSlaves
          (toMaster, toSlaves) = withClockResetEnable clockGen resetGen enableGen (memoryMap
            @System @_ @4 @32) config masterIn $ bundle slaves
      topEntityInput = L.concatMap wbWriteThenRead writeAddresses <> [idleM2S]
      simLength = L.length topEntityInput
      simOut = simulateN simLength topEntity topEntityInput
      configL = toList config
      rangesL = toList ranges
       -- findBaseAddress returns the base address that responds to a and its range
      findBaseAddress a = (L.last $ (L.head configL, L.head rangesL) : lowerAddresses)
        where
          lowerAddresses = L.takeWhile ((<a) . fst) $ L.zip configL rangesL
      getExpected a | a >= baseAddr && (a - baseAddr) <= range = Just $ bitCoerce a
                    | otherwise                                = Nothing
       where
        (baseAddr, range) = findBaseAddress a
    footnote . fromString $ "simOut: " <> showX simOut
    footnote . fromString $ "simIn: " <> showX topEntityInput
    footnote . fromString $ "writes: " <> show writeAddresses
    footnote . fromString $ "ranges: " <> show ranges
    footnote . fromString $ "config: " <> show config
    fmap filterSimOut (every2nd $ L.tail simOut) === fmap getExpected writeAddresses
 where
  filterSimOut WishboneS2M{..} | acknowledge && not err = Just readData
                               | otherwise              = Nothing
  wbWriteThenRead a = [wbWrite a, wbRead a]
  every2nd (_:b:cs) = b : every2nd cs
  every2nd _ = []

-- | transforms an address to a 'WishboneM2S' read operation.
wbRead :: forall bs addressWidth . (KnownNat bs, KnownNat addressWidth) => BitVector addressWidth -> WishboneM2S bs addressWidth
wbRead address = (idleM2S @bs @addressWidth)
  { addr = address
  , strobe = True
  , busCycle = True
  }

-- | transforms an address to a 'WishboneM2S' write operation that writes the given address
-- to the given address.
wbWrite :: forall bs addressWidth . (KnownNat bs, KnownNat addressWidth) => BitVector addressWidth -> WishboneM2S bs addressWidth
wbWrite address = (idleM2S @bs @addressWidth)
  { addr = address
  , strobe = True
  , busCycle = True
  , writeData = resize address
  , writeEnable = True
  , busSelect = maxBound
  }

-- | Simple wishbone slave that responds to addresses [0..range], it responds by returning
-- a stored value (initialized by readData0), which can be overwritten by the wishbone bus.
-- any read/write attempt to an address outside of the supplied range sets the err signal.
simpleSlave :: (HiddenClockResetEnable dom, KnownNat aw, KnownNat bs, aw ~ bs * 8) =>
  BitVector aw ->
  BitVector (bs * 8) ->
  Signal dom (WishboneM2S bs aw) ->
  Signal dom (WishboneS2M bs)
simpleSlave range readData0 wbIn = mealy go readData0 wbIn
 where
  go readData1 WishboneM2S{..} = (readData2, WishboneS2M{readData, acknowledge, err})
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
