-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Tests.Protocols.Wishbone.Extra (tests) where

import Clash.Explicit.Prelude (moore, noReset)
import Clash.Prelude

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Hedgehog (Gen, Property)
import Hedgehog.Internal.Property (property)
import Protocols
import Protocols.Hedgehog (ExpectOptions (eoResetCycles), defExpectOptions, eoSampleMax)
import Protocols.Wishbone
import Protocols.Wishbone.Extra (increaseBuswidth)
import Protocols.Wishbone.Standard.Hedgehog (
  WishboneMasterRequest (Read, Write),
  wishbonePropWithModel,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import qualified Data.Map as Map
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

type AddressWidth32 = 5
type AddressWidth64 = 4

tests :: TestTree
tests =
  testGroup
    "Tests.Protocols.Wishbone.Extra"
    [ testPropertyNamed
        "increaseBuswidth preserves wishbone transactions (32->64)"
        "increaseBuswidth_32_to_64"
        prop_increaseBuswidth_32_to_64
    ]

mergeWithMask ::
  forall n m.
  (KnownNat n, KnownNat m) =>
  BitVector (n (*) m) ->
  BitVector (n (*) m) ->
  BitVector n ->
  BitVector (n (*) m)
mergeWithMask (unpack -> old) (unpack -> new) (unpack -> mask) =
  pack (mux @(Vec n) @(BitVector m) mask new old)

{- | Test that increaseBuswidth correctly converts 32-bit wishbone transactions
to 64-bit transactions without data corruption. The test verifies:
1. Address mapping: 32-bit addresses map to correct 64-bit addresses
2. Data placement: 32-bit writes go to the correct half of 64-bit words
3. Byte select mapping: byte enables are correctly positioned
4. Read data extraction: reads from the correct half of 64-bit words
-}
prop_increaseBuswidth_32_to_64 :: Property
prop_increaseBuswidth_32_to_64 = property $ do
  withClockResetEnable clk rst ena $
    wishbonePropWithModel
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

  -- Behavioral model: track memory state as 32-bit words
  -- The model simulates how 32-bit accesses map to 64-bit memory
  model ::
    WishboneMasterRequest aw (BitVector (n (*) 8)) ->
    WishboneS2M (BitVector (n (*) 8)) ->
    Map (BitVector aw) (BitVector (n (*) 8)) ->
    Either String (Map (BitVector aw) (BitVector (n (*) 8)))
  model _ resp _mem | resp . err = Left "Unexpected bus error"
  --
  model (Read addr _mask) resp mem
    | resp . acknowledge =
        let expected = fromMaybe 0 (Map.lookup addr mem)
         in if resp . readData == expected then Right mem else Left "Data mismatch on read"
  --
  model (Write addr mask dat) resp mem
    | resp . acknowledge =
        let
          old = fromMaybe 0 (Map.lookup addr mem)
          new = mergeWithMask old dat mask
         in
          Right (Map.insert addr new mem)
  --
  model _ _ mem = Right mem

  -- Generate wishbone requests
  genInputs :: Gen [WishboneMasterRequest AddressWidth32 (BitVector 32)]
  genInputs = Gen.list (Range.linear 0 32) $ do
    Gen.choice
      [ Read <$> genBoundedIntegral <*> genBoundedIntegral
      , Write <$> genBoundedIntegral <*> genBoundedIntegral <*> genBoundedIntegral
      ]

  genBoundedIntegral :: forall a. (Integral a, Bounded a) => Gen a
  genBoundedIntegral = Gen.integral Range.constantBounded

  -- DUT: increaseBuswidth followed by 64-bit memory
  dut :: Circuit (Wishbone System 'Standard AddressWidth32 (BitVector 32)) ()
  dut =
    withClockResetEnable clk rst ena $
      increaseBuswidth @System @'Standard @AddressWidth32 @4 @1
        |> dutMem

  -- 64-bit memory backend (2^4 = 16 locations of 64 bits each)
  -- Simple RAM implementation using Mealy machine
  dutMem :: Circuit (Wishbone System 'Standard AddressWidth64 (BitVector 64)) ()
  dutMem = withClockResetEnable clk rst ena $ Circuit go
   where
    go (m2s, ()) = (s2m, ())
     where
      s2m = mealy transition (repeat @(2 ^ AddressWidth64) 0) m2s

    transition ::
      Vec (2 ^ AddressWidth64) (BitVector 64) ->
      WishboneM2S AddressWidth64 8 (BitVector 64) ->
      (Vec (2 ^ AddressWidth64) (BitVector 64), WishboneS2M (BitVector 64))
    transition mem m2s = (mem', s2m)
     where
      mem'
        | m2s . busCycle && m2s . strobe && m2s . writeEnable =
            let
              addr = resize m2s . addr :: BitVector AddressWidth64
              old = mem !! addr
              new = mergeWithMask old m2s . writeData m2s . busSelect
             in
              replace addr new mem
        | otherwise = mem

      s2m =
        (emptyWishboneS2M :: WishboneS2M (BitVector 64))
          { acknowledge = m2s . busCycle && m2s . strobe
          , readData = mem !! addr
          }
      addr = resize m2s . addr :: BitVector AddressWidth64
