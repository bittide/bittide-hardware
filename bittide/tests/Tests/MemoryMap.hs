{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.MemoryMap(memMapGroup) where

import Clash.Prelude
import Clash.Hedgehog.Sized.Vector
import Clash.Hedgehog.Sized.BitVector
import Clash.Sized.Vector(unsafeFromList)

import Contranomy.Wishbone
import Data.Proxy
import Data.String
import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Data.Set as Set
import qualified GHC.TypeNats as TN
import qualified Data.List as L
import Bittide.MemoryMap



memMapGroup :: TestTree
memMapGroup = testGroup "Memory Map group"
  [ testPropertyNamed "Reading readData from slaves." "readingSlaves" readingSlaves
  ]

genConfig :: forall slaves aw . (KnownNat slaves, KnownNat aw) => Proxy slaves -> Gen (MemoryMap slaves aw)
genConfig = do
  let set = Gen.set (Range.singleton $ natToNum @slaves) genDefinedBitVector
  return $ unsafeFromList . Set.elems <$> set

readingSlaves :: Property
readingSlaves = property $ do
  devices <- forAll $ Gen.enum 1 32
  case TN.someNatVal devices of
   SomeNat devices0 -> do
    config <- forAll $ genConfig @_ @32 devices0
    nrOfReads <- forAll $ Gen.enum 1 100
    let
     nrOfReadsRange = Range.singleton nrOfReads
     topEntity masterIn = toMaster
      where
        slaves = withClockResetEnable @System clockGen resetGen enableGen simpleSlave maxBound
          <$> config <*> unbundle toSlaves
        (toMaster, toSlaves) = withClockResetEnable clockGen resetGen enableGen (memoryMap
         @System @_ @4 @32) config masterIn $ bundle slaves
    readAddresses <- forAll $ Gen.list nrOfReadsRange $ Gen.integral Range.constantBounded
    let
     topEntityInput = (wbRead <$> readAddresses) <> [idleM2S]
     simLength = L.length topEntityInput
     simOut = simulateN simLength topEntity topEntityInput
     configL = toList config
     findBaseAddress a = L.last $ (L.head configL : L.takeWhile (a>) configL)
    footnote . fromString $ "simOut: " <> show simOut
    footnote . fromString $ "reads: " <> show readAddresses
    footnote . fromString $ "config: " <> show config
    L.tail (readData <$> simOut) === fmap findBaseAddress readAddresses

wbRead :: forall bs addressWidth . (KnownNat bs, KnownNat addressWidth) => BitVector addressWidth -> WishboneM2S bs addressWidth
wbRead address = (idleM2S @bs @addressWidth)
  { addr = address
  , strobe = True
  , busCycle = True
  }

simpleSlave :: (HiddenClockResetEnable dom, KnownNat aw, KnownNat bs) =>
  BitVector aw ->
  BitVector (bs * 8) ->
  Signal dom (WishboneM2S bs aw) ->
  Signal dom (WishboneS2M bs)
simpleSlave range readData0 wbIn = mealy go readData0 wbIn
 where
  go readData1 WishboneM2S{..} = (readData, WishboneS2M{readData, acknowledge, err})
   where
     masterActive = strobe && busCycle
     addrInRange = addr <= range
     acknowledge = masterActive && addrInRange
     err = masterActive && not addrInRange
     readData | masterActive && addrInRange && writeEnable = writeData
              | otherwise = readData1
