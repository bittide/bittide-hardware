-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Tests.Clash.Protocols.Wishbone.Extra (tests) where

import Clash.Explicit.Prelude

import Bittide.DoubleBufferedRam (
  ContentType (Vec),
  InitialContent (NonReloadable),
  wbStorage,
 )
import Clash.Prelude (withClockResetEnable)
import Clash.Protocols.Wishbone.Extra (xpmCdcHandshakeWb)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Hedgehog (Gen, Property)
import Hedgehog.Internal.Property (property)
import Protocols
import Protocols.Hedgehog (ExpectOptions (eoResetCycles), defExpectOptions, eoSampleMax)
import Protocols.MemoryMap (unMemmap)
import Protocols.Wishbone
import Protocols.Wishbone.Standard.Hedgehog (
  WishboneMasterRequest (Read, Write),
  wishbonePropWithModel,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import qualified Data.Map as Map
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

type AddressWidth = 4

tests :: TestTree
tests =
  testGroup
    "Tests.Clash.Protocols.Wishbone.Extra"
    [ testPropertyNamed
        "xpmCdcHandshakeWb preserves wishbone transactions"
        "xpmCdcHandshakeWb"
        prop_xpmCdcHandshakeWb
    ]

mergeWithMask :: BitVector 32 -> BitVector 32 -> BitVector 4 -> BitVector 32
mergeWithMask (unpack -> old) (unpack -> new) (unpack -> mask) =
  pack (mux @(Vec 4) @(BitVector 8) mask new old)

{- | Test that xpmCdcHandshakeWb correctly transfers Wishbone transactions
between clock domains without data corruption. The test connects the CDC
bridge to a wishbone storage backend and verifies that read/write operations
work correctly.
-}
prop_xpmCdcHandshakeWb :: Property
prop_xpmCdcHandshakeWb = property $ do
  withClockResetEnable clk rst ena
    $ wishbonePropWithModel
      @System
      defExpectOptions{eoSampleMax = 10_000, eoResetCycles = 0}
      model
      dut
      genInputs
      Map.empty
 where
  clk = clockGen
  rst = noReset
  ena = enableGen

  -- Behavioral model: track memory state
  model ::
    WishboneMasterRequest AddressWidth 4 ->
    WishboneS2M 4 ->
    Map (BitVector AddressWidth) (BitVector 32) ->
    Either String (Map (BitVector AddressWidth) (BitVector 32))
  model _ resp _mem | resp.err = Left "Unexpected bus error"
  --
  model (Read addr _mask) resp mem
    | resp.acknowledge =
        let expected = fromMaybe 0 (Map.lookup addr mem)
         in if resp.readData == expected then Right mem else Left "Data mismatch"
  --
  model (Write addr mask dat) resp mem
    | resp.acknowledge =
        let
          old = fromMaybe 0 (Map.lookup addr mem)
          new = mergeWithMask old dat mask
         in
          Right (Map.insert addr new mem)
  --
  model _ _ mem = Right mem

  -- Generate wishbone requests
  genInputs :: Gen [WishboneMasterRequest AddressWidth 4]
  genInputs = Gen.list (Range.linear 0 32) $ do
    Gen.choice
      [ Read <$> genBoundedIntegral <*> genBoundedIntegral
      , Write <$> genBoundedIntegral <*> genBoundedIntegral <*> genBoundedIntegral
      ]

  genBoundedIntegral :: forall a. (Integral a, Bounded a) => Gen a
  genBoundedIntegral = Gen.integral Range.constantBounded

  dut :: Circuit (Wishbone System 'Standard AddressWidth 4) ()
  dut = xpmCdcHandshakeWb clk rst clk rst |> dutMem

  dutMem :: Circuit (Wishbone System 'Standard AddressWidth 4) ()
  dutMem =
    withClockResetEnable clk rst ena
      $ unMemmap
      $ wbStorage "test" (NonReloadable (Vec (repeat @(2 ^ AddressWidth) 0)))
