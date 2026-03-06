-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Tests.Clash.Protocols.Wishbone.Extra (tests) where

import Clash.Explicit.Prelude

import Bittide.DoubleBufferedRam (
  ContentType (Vec),
  wbStorage,
 )
import Clash.Class.BitPackC (ByteOrder (..))
import Clash.Hedgehog.Sized.BitVector (genDefinedBitVector)
import Clash.Prelude (withClockResetEnable)
import Data.Map (Map)
import Data.String.Interpolate (i)
import Hedgehog (Gen, Property, forAll, property)
import Protocols
import Protocols.Hedgehog (ExpectOptions (eoResetCycles), defExpectOptions, eoSampleMax)
import Protocols.MemoryMap (unMemmap)
import Protocols.Wishbone
import Protocols.Wishbone.Extra (increaseBuswidth, xpmCdcHandshakeWb)
import Protocols.Wishbone.Standard.Hedgehog (
  WishboneMasterRequest (Read, Write),
  wishbonePropWithModel,
 )
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import qualified Clash.Prelude as C
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

type AddressWidth = 4

genWishboneTransfer ::
  (KnownNat aw, KnownNat nBytes) =>
  Gen (BitVector aw) ->
  Gen (WishboneMasterRequest aw nBytes)
genWishboneTransfer genAddr =
  Gen.choice
    [ Read <$> genAddr <*> pure maxBound
    , Write <$> genAddr <*> pure maxBound <*> genDefinedBitVector
    ]

mergeWithMask ::
  forall n m.
  (KnownNat n, KnownNat m) =>
  BitVector (n * m) ->
  BitVector (n * m) ->
  BitVector n ->
  BitVector (n * m)
mergeWithMask (unpack -> old) (unpack -> new) (unpack -> mask) =
  pack (mux @(Vec n) @(BitVector m) mask new old)

-- | Reverse bitvector
reverseBitVector :: (KnownNat n) => BitVector n -> BitVector n
reverseBitVector = v2bv . reverse . bv2v

reverseByteVector :: forall n. (KnownNat n) => BitVector (n * 8) -> BitVector (n * 8)
reverseByteVector = pack . reverse . (unpack @(Vec n (BitVector 8)))

-- | Generic memory model for a Wishbone slave.
memoryModel ::
  forall aw dw.
  (KnownNat aw, KnownNat dw) =>
  ByteOrder ->
  ByteOrder ->
  Int ->
  WishboneMasterRequest aw dw ->
  WishboneS2M dw ->
  Map (BitVector aw) (BitVector (dw * 8)) ->
  Either String (Map (BitVector aw) (BitVector (dw * 8)))
memoryModel busOrder memOrder maxAddr req@(Write addr mask0 dat0) resp mem
  | addrInt <= maxAddr && resp.acknowledge = Right newMem
  | resp.acknowledge = Left [i|Received acknowledge for out-of-range address #{(req, resp)}|]
  | maxAddr > addrRange = Left [i|Supplied depth #{maxAddr} exceeds addressable range #{addrRange}|]
  | resp.err = Right mem
  | otherwise = Left [i|Expected acknowledge for valid write #{(req, resp)}|]
 where
  addrInt = fromIntegral addr
  addrRange :: Int
  addrRange = fromIntegral (maxBound :: BitVector aw)
  (mask1, dat1)
    | busOrder == memOrder = (mask0, dat0)
    | otherwise = (reverseBitVector mask0, reverseByteVector dat0)
  oldData = mem Map.! addr
  newData = mergeWithMask oldData dat1 mask1
  newMem = Map.insert addr newData mem
memoryModel busOrder memOrder maxAddr req@(Read addr mask) resp mem
  | addrInt <= maxAddr && resp.acknowledge && match = Right mem
  | addrInt <= maxAddr && resp.acknowledge = Left "Data mismatch"
  | resp.acknowledge = Left [i|Received acknowledge for out-of-range address #{(req, resp)}|]
  | maxAddr > addrRange = Left [i|Supplied depth #{maxAddr} exceeds addressable range #{addrRange}|]
  | resp.err = Right mem
  | otherwise = Left [i|Expected acknowledge for valid read #{(req, resp)}|]
 where
  addrInt = fromIntegral addr
  addrRange :: Int
  addrRange = fromIntegral (maxBound :: BitVector aw)
  match = mergeWithMask 0 (resp.readData) mask == mergeWithMask 0 memData mask
  memData
    | busOrder == memOrder = mem Map.! addr
    | otherwise = reverseByteVector (mem Map.! addr)

{- | Test that xpmCdcHandshakeWb correctly transfers Wishbone transactions
between clock domains without data corruption. The test connects the CDC
bridge to a wishbone storage backend and verifies that read/write operations
work correctly.
-}
prop_xpmCdcHandshakeWb :: Property
prop_xpmCdcHandshakeWb = property $ do
  busOrder <- forAll $ Gen.element [BigEndian, LittleEndian]
  let
    dut :: Circuit (Wishbone System 'Standard AddressWidth 4) ()
    dut = xpmCdcHandshakeWb clk rst clk rst |> dutMem

    dutMem :: Circuit (Wishbone System 'Standard AddressWidth 4) ()
    dutMem =
      let ?busByteOrder = busOrder
       in withClockResetEnable clk rst ena
            $ unMemmap
            $ wbStorage "test" (SNat @(2 ^ (AddressWidth - 1))) (Just (Vec (repeat 0)))

  withClockResetEnable clk rst ena
    $ wishbonePropWithModel
      @System
      defExpectOptions{eoSampleMax = 10_000, eoResetCycles = 0}
      (memoryModel busOrder BigEndian 7)
      dut
      genInputs
      (Map.fromList (L.zip [0 .. lastAddress] (L.repeat 0)))
 where
  lastAddress = natToNum @(2 ^ (AddressWidth - 1)) -- Less elements to also test error propagation.
  clk = clockGen
  rst = noReset
  ena = enableGen

  -- Generate wishbone requests
  genInputs :: Gen [WishboneMasterRequest AddressWidth 4]
  genInputs = Gen.list (Range.linear 0 32) $ do
    Gen.choice
      [ Read <$> genBoundedIntegral <*> genBoundedIntegral
      , Write <$> genBoundedIntegral <*> genBoundedIntegral <*> genBoundedIntegral
      ]

  genBoundedIntegral :: forall a. (Integral a, Bounded a) => Gen a
  genBoundedIntegral = Gen.integral Range.constantBounded

testIncreaseBuswidth ::
  forall power nBytes.
  -- \| We are restricted by our 4 byte wide `wbStorage` backend.
  (KnownNat power, (2 ^ power) * nBytes ~ 4, nBytes ~ 4 `DivRU` (2 ^ power)) =>
  SNat power ->
  Property
testIncreaseBuswidth power = property $ do
  busOrder <- forAll $ Gen.element [BigEndian, LittleEndian]
  let
    eOpts = defExpectOptions

    -- Depth is half the number of addresses to also tests error on out-of-range accesses.
    depth = SNat @(2 ^ (AddressWidth - 2))
    lastAddress = snatToNum $ mulSNat d2 depth
    lastAddressSmall = lastAddress * (natToNum @(2 ^ power))
    genAddr = Gen.integral (Range.linear 0 lastAddressSmall)
    genInputs = Gen.list (Range.linear 1 10) (genWishboneTransfer genAddr)

    dutMem :: Circuit (Wishbone System 'Standard AddressWidth 4) ()
    dutMem =
      let ?busByteOrder = busOrder
       in withClockResetEnable clk rst ena
            $ unMemmap (wbStorage "test" depth (Just (Vec (repeat 0))))

    dut :: Circuit (Wishbone System 'Standard (AddressWidth + power) nBytes) ()
    dut = withClockResetEnable clk rst ena (increaseBuswidth power |> dutMem)

  H.footnote [i| Depth: #{snatToInteger depth}, Last Address: #{lastAddress}|]

  withClockResetEnable clk rst ena
    $ wishbonePropWithModel
      @System
      eOpts
      (memoryModel busOrder BigEndian (fromIntegral lastAddressSmall))
      dut
      genInputs
      (Map.fromList (L.zip [0 .. lastAddressSmall] (L.repeat 0)))
 where
  clk = clockGen
  rst = noReset
  ena = enableGen

prop_increaseBuswidth_0 :: Property
prop_increaseBuswidth_0 = testIncreaseBuswidth d0

prop_increaseBuswidth_1 :: Property
prop_increaseBuswidth_1 = testIncreaseBuswidth d1

prop_increaseBuswidth_2 :: Property
prop_increaseBuswidth_2 = testIncreaseBuswidth d2

prop_memoryModel :: Property
prop_memoryModel = property $ do
  busOrder <- forAll $ Gen.element [BigEndian, LittleEndian]
  let
    eOpts = defExpectOptions
    dut :: (C.HiddenClockResetEnable System) => Circuit (Wishbone System 'Standard AddressWidth 4) ()
    dut =
      let ?busByteOrder = busOrder
       in unMemmap (wbStorage "test" d8 (Just (Vec (repeat 0))))

  withClockResetEnable clk rst ena
    $ wishbonePropWithModel
      @System
      eOpts
      (memoryModel busOrder BigEndian 16)
      dut
      genInputs
      (Map.fromList (L.zip [0 .. lastAddress] (L.repeat 0)))
 where
  clk = clockGen
  rst = noReset
  ena = enableGen
  lastAddress = natToNum @(2 ^ (AddressWidth - 1)) -- Less elements to also test error propagation.
  genAddr = Gen.integral (Range.linear 0 (lastAddress * 2))
  genInputs = Gen.list (Range.linear 1 10) (genWishboneTransfer genAddr)

tests :: TestTree
tests = $(testGroupGenerator)
