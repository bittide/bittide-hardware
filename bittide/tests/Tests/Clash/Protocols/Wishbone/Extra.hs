-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Clash.Protocols.Wishbone.Extra (tests, stallStandard) where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

import Bittide.DoubleBufferedRam (
  ContentType (Vec),
  wbStorage,
 )
import Clash.Hedgehog.Sized.BitVector (genDefinedBitVector)
import Clash.Signal.Internal as S
import Data.Map (Map)
import Data.String.Interpolate (i)
import Hedgehog (Gen, Property, property)
import Protocols
import Protocols.Hedgehog (ExpectOptions (eoResetCycles), defExpectOptions, eoSampleMax)
import Protocols.MemoryMap (unMemmap)
import Protocols.Wishbone
import Protocols.Wishbone.Extra (decreaseBusWidth, increaseBusWidth, xpmCdcHandshakeWb)
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
import qualified Protocols.Wishbone.Standard.Hedgehog as Wb

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

-- | Generic memory model for a Wishbone slave.
memoryModel ::
  forall aw dw.
  (KnownNat aw, KnownNat dw) =>
  Int ->
  WishboneMasterRequest aw dw ->
  WishboneS2M dw ->
  Map (BitVector aw) (BitVector (dw * 8)) ->
  Either String (Map (BitVector aw) (BitVector (dw * 8)))
memoryModel maxAddr req@(Write addr mask dat) resp mem
  | addrInt <= maxAddr && resp.acknowledge = Right newMem
  | resp.acknowledge = Left [i|Received acknowledge for out-of-range address #{(req, resp)}|]
  | maxAddr > addrRange = Left [i|Supplied depth #{maxAddr} exceeds addressable range #{addrRange}|]
  | resp.err = Right mem
  | otherwise = Left [i|Expected acknowledge for valid write #{(req, resp)}|]
 where
  addrInt = fromIntegral addr
  addrRange :: Int
  addrRange = fromIntegral (maxBound :: BitVector aw)
  oldData = mem Map.! addr
  newData = mergeWithMask oldData dat mask
  newMem = Map.insert addr newData mem
memoryModel maxAddr req@(Read addr mask) resp mem
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
  memData = mem Map.! addr

{- | Test that xpmCdcHandshakeWb correctly transfers Wishbone transactions
between clock domains without data corruption. The test connects the CDC
bridge to a wishbone storage backend and verifies that read/write operations
work correctly.
-}
prop_xpmCdcHandshakeWb :: Property
prop_xpmCdcHandshakeWb = property $ do
  let
    dut :: Circuit (Wishbone System 'Standard AddressWidth 4) ()
    dut = xpmCdcHandshakeWb clk rst clk rst |> dutMem

    dutMem :: Circuit (Wishbone System 'Standard AddressWidth 4) ()
    dutMem =
      withClockResetEnable clk rst ena
        $ unMemmap
        $ wbStorage "test" (SNat @(2 ^ (AddressWidth - 1))) (Just (Vec (repeat 0)))

  withClockResetEnable clk rst ena
    $ wishbonePropWithModel
      @System
      defExpectOptions{eoSampleMax = 10_000, eoResetCycles = 0}
      (memoryModel 7)
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

testIncreaseBusWidth ::
  forall power nBytes.
  -- \| We are restricted by our 4 byte wide `wbStorage` backend.
  (KnownNat power, (2 ^ power) * nBytes ~ 4, nBytes ~ 4 `DivRU` (2 ^ power)) =>
  SNat power ->
  Property
testIncreaseBusWidth power = property $ do
  let
    eOpts = defExpectOptions

    -- Depth is half the number of addresses to also tests error on out-of-range accesses.
    depth = SNat @(2 ^ (AddressWidth - 2))
    lastAddress = snatToNum $ predSNat depth
    lastAddressSmall = lastAddress * (natToNum @(2 ^ power))
    genAddr = Gen.integral (Range.linear 0 lastAddressSmall)
    genInputs = Gen.list (Range.linear 1 10) (genWishboneTransfer genAddr)

    dutMem :: Circuit (Wishbone System 'Standard AddressWidth 4) ()
    dutMem =
      withClockResetEnable clk rst ena
        $ unMemmap
        $ wbStorage "test" depth (Just (Vec (repeat 0)))

    dut :: Circuit (Wishbone System 'Standard (AddressWidth + power) nBytes) ()
    dut = withClockResetEnable clk rst ena (increaseBusWidth power |> dutMem)

  H.footnote [i| Depth: #{snatToInteger depth}, Last Address: #{lastAddress}|]

  withClockResetEnable clk rst ena
    $ wishbonePropWithModel
      @System
      eOpts
      (memoryModel (fromIntegral lastAddressSmall))
      dut
      genInputs
      (Map.fromList (L.zip [0 .. lastAddressSmall] (L.repeat 0)))
 where
  clk = clockGen
  rst = noReset
  ena = enableGen

prop_increaseBusWidth_0 :: Property
prop_increaseBusWidth_0 = testIncreaseBusWidth d0

prop_increaseBusWidth_1 :: Property
prop_increaseBusWidth_1 = testIncreaseBusWidth d1

prop_increaseBusWidth_2 :: Property
prop_increaseBusWidth_2 = testIncreaseBusWidth d2

testDecreaseBusWidth ::
  forall power width.
  -- \| We are restricted by our 4 byte wide `wbStorage` backend.
  ( KnownNat power
  , 1 <= power
  , KnownNat width
  , 1 <= width
  , width <= 4
  ) =>
  SNat power ->
  SNat width ->
  Property
testDecreaseBusWidth power SNat = property $ do
  stalls <- H.forAll $ Gen.list (Range.linear 0 100) $ Gen.integral (Range.linear 0 10)
  earlyStrobe <- H.forAll Gen.bool
  let
    eOpts = defExpectOptions

    -- Depth is half the number of addresses to also tests error on out-of-range accesses.
    depth = SNat @(2 ^ (AddressWidth - 2))
    lastAddress = snatToNum $ predSNat depth
    lastAddressSmall = lastAddress * (natToNum @(2 ^ power))
    genAddr = Gen.integral (Range.linear 0 lastAddressSmall)
    genInputs = Gen.list (Range.linear 1 10) (genWishboneTransfer genAddr)

    dutMem :: Circuit (Wishbone System 'Standard (AddressWidth + power) width) ()
    dutMem =
      withClockResetEnable clk rst ena
        $ unMemmap
        $ wbStorage "test" depth (Just (Vec (repeat 0)))

    dut :: Circuit (Wishbone System 'Standard AddressWidth (2 ^ power * width)) ()
    dut =
      withClockResetEnable
        clk
        rst
        ena
        (decreaseBusWidth power |> Wb.validatorCircuit |> stallStandard earlyStrobe stalls |> dutMem)

  H.footnote [i| Depth: #{snatToInteger depth}, Last Address: #{lastAddress}|]

  withClockResetEnable clk rst ena
    $ wishbonePropWithModel
      @System
      eOpts
      (memoryModel (fromIntegral lastAddressSmall))
      dut
      genInputs
      (Map.fromList (L.zip [0 .. lastAddressSmall] (L.repeat 0)))
 where
  clk = clockGen
  rst = noReset
  ena = enableGen

prop_decreaseBusWidth_1_1 :: Property
prop_decreaseBusWidth_1_1 = testDecreaseBusWidth d1 d1

prop_decreaseBusWidth_2_1 :: Property
prop_decreaseBusWidth_2_1 = testDecreaseBusWidth d2 d1

prop_decreaseBusWidth_3_1 :: Property
prop_decreaseBusWidth_3_1 = testDecreaseBusWidth d3 d1

prop_decreaseBusWidth_1_2 :: Property
prop_decreaseBusWidth_1_2 = testDecreaseBusWidth d1 d2

prop_decreaseBusWidth_2_2 :: Property
prop_decreaseBusWidth_2_2 = testDecreaseBusWidth d2 d2

prop_decreaseBusWidth_3_2 :: Property
prop_decreaseBusWidth_3_2 = testDecreaseBusWidth d3 d2

prop_memoryModel :: Property
prop_memoryModel = property $ do
  let
    eOpts = defExpectOptions
    dut :: (C.HiddenClockResetEnable System) => Circuit (Wishbone System 'Standard AddressWidth 4) ()
    dut =
      unMemmap (wbStorage "test" d8 (Just (Vec (repeat 0))))

  withClockResetEnable clk rst ena
    $ wishbonePropWithModel
      @System
      eOpts
      (memoryModel 16)
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

-- XXX: Should be removed once this has been fixed upstream.

-- | Create a stalling wishbone 'Standard' circuit.
stallStandard ::
  forall dom addressBits dataBytes.
  ( C.KnownNat addressBits
  , C.KnownDomain dom
  , C.KnownNat dataBytes
  ) =>
  {- | Early busCycle propagation, when 'True', the busCycle signal will always be propagated.
  When 'False', the busCycle signal will be stalled just like the 'strobe' signal.
  -}
  Bool ->
  -- | Number of cycles to stall the master for on each valid bus-cycle
  [Int] ->
  Circuit
    (Wishbone dom 'Standard addressBits dataBytes)
    (Wishbone dom 'Standard addressBits dataBytes)
stallStandard earlyBusCycle stalls0 =
  Circuit goS
 where
  goS (m2s, s2m) = (s2m, go stalls0 m2s s2m)
  go ::
    [Int] ->
    Signal dom (WishboneM2S addressBits dataBytes) ->
    Signal dom (WishboneS2M dataBytes) ->
    Signal dom (WishboneM2S addressBits dataBytes)
  -- Idle, waiting for a request
  go (stall : stalls) (m :- ms) (s :- ss) = m' :- go stalls' ms ss
   where
    -- Only propagate the buscycle while stalling, this allows the manager to reserve the bus, but postpones the actual
    -- transaction until the stall is over.
    m'
      | stall > 0 = block m
      | otherwise = m
    stalls' = nextStall stall stalls m s
  go [] ms _ = ms

  block m
    | earlyBusCycle = m{strobe = False}
    | otherwise = m{busCycle = False, strobe = False}

  -- Select the correct
  nextStall
    | earlyBusCycle = \stall stalls m s ->
        if
          | m.busCycle && m.strobe && hasTerminateFlag s -> stalls
          | m.busCycle && m.strobe && stall > 0 -> stall - 1 : stalls
          | otherwise -> stall : stalls
    | otherwise = \stall stalls m s ->
        if
          | m.busCycle && m.strobe && hasTerminateFlag s -> stalls
          | m.busCycle && stall > 0 -> stall - 1 : stalls
          | otherwise -> stall : stalls

tests :: TestTree
tests = $(testGroupGenerator)
