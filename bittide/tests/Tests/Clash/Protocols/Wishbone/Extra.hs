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
import Clash.Prelude (withClockResetEnable)
import Data.Map (Map)
import Data.String.Interpolate (i)
import Hedgehog (Gen, Property, property)
import Protocols
import Protocols.Hedgehog (ExpectOptions (eoResetCycles), defExpectOptions, eoSampleMax)
import Protocols.MemoryMap (unMemmap)
import Protocols.Wishbone
import Protocols.Wishbone.Extra (xpmCdcHandshakeWb)
import Protocols.Wishbone.Standard.Hedgehog (
  WishboneMasterRequest (Read, Write),
  wishbonePropWithModel,
 )
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import qualified Data.List as L
import qualified Data.Map as Map
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

type AddressWidth = 4
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


tests :: TestTree
tests = $(testGroupGenerator)
