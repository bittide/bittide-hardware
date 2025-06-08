-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Tests.ClockControl.Freeze where

import BitPackC (ByteOrder (BigEndian), unpackOrErrorC)
import Bittide.ClockControl.Freeze (counter, freeze)
import Bittide.SharedTypes (Bytes)
import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)
import Hedgehog (Gen, Property)
import Hedgehog.Internal.Property (property)
import Protocols
import Protocols.Hedgehog (defExpectOptions)
import Protocols.MemoryMap
import Protocols.Wishbone
import Protocols.Wishbone.Standard.Hedgehog (
  WishboneMasterRequest (..),
  wishbonePropWithModel,
 )
import Test.Tasty
import Test.Tasty.Hedgehog (testPropertyNamed)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

type AddressWidth = 4

genWishboneTransfer ::
  ( KnownNat aw
  , KnownNat n
  ) =>
  Gen (BitVector aw) ->
  Gen (BitVector n) ->
  Gen (WishboneMasterRequest aw (BitVector n))
genWishboneTransfer genAddr genData =
  Gen.choice
    [ Read <$> genAddr <*> pure maxBound
    , Write <$> genAddr <*> pure maxBound <*> genData
    ]

data ModelState = ModelState
  { frozen :: Bool
  , lastSeen :: Maybe (Unsigned 32)
  , nFreezes :: Unsigned 32
  }
  deriving (Show, Eq)

initModelState :: ModelState
initModelState = ModelState{frozen = False, lastSeen = Nothing, nFreezes = 0}

{- | Test 'freeze' by spamming it with random read and write requests. The model
tests that:

 * All illegal instructions are ignored.
 * The freeze counter is (not) incremented when a freeze is (not) requested.
 * A read value stays stable after a freeze is requested.

It does this by connecting a bunch of counters to 'freeze', each with an
offset of one. This way, we can predict the value of all counters by just
reading one of them. The model will then check that the value of the register
that is read matches the expected value.
-}
prop_wb :: Property
prop_wb = property $ do
  -- Uncomment to see the memory map as a footnote
  -- footnote (ppShow ((\(SimOnly x) -> x) (getConstBwdAny dutMm)))

  withClockResetEnable clk rst ena
    $ wishbonePropWithModel
      @XilinxSystem
      defExpectOptions
      model
      dut
      genInputs
      initModelState
 where
  clk = clockGen
  rst = noReset
  ena = enableGen

  endian = BigEndian

  model ::
    WishboneMasterRequest AddressWidth (BitVector 32) ->
    WishboneS2M (BitVector 32) ->
    ModelState ->
    Either String ModelState
  model _ WishboneS2M{err = True} s = Right s
  model _ WishboneS2M{retry = True} s = Right s
  model _ WishboneS2M{acknowledge = False} s = Right s
  model (Read _ _) WishboneS2M{readData} s@ModelState{frozen = False}
    -- No freeze requested yet, registers are at their initial values
    | readData == 0 = Right s
    | otherwise = Left $ "Expected initial values (0), got " <> show readData
  model (Read 1 _) WishboneS2M{readData} s@ModelState{nFreezes = n}
    -- XXX: It takes a little while for the freeze counter to update after a
    --      freeze has been issued, so we also accept a freeze counter that is
    --      one less than the expected value. Remove this when we properly delay
    --      acknowledgement until the freeze counter is updated.
    | readDataU `elem` [n, n - 1] = Right s
    | otherwise =
        Left $ "Freeze counter mismatch: expected " <> show n <> ", got " <> show readDataU
   where
    readDataU = unpackOrErrorC endian (unpack readData)
  model (Read a _) WishboneS2M{readData} s@ModelState{lastSeen = Nothing}
    | a < 2 = Right s
    | otherwise =
        -- Record the value of the register that is being read. This can predict the
        -- value of all other registers (until a freeze is requested).
        Right s{lastSeen = Just (unpackOrErrorC endian (unpack readData) - fromIntegral a)}
  model (Read a _) WishboneS2M{readData} s@ModelState{lastSeen = Just l}
    | a < 2 = Right s
    | readDataU - fromIntegral a == l = Right s
    | otherwise =
        Left $ "Read value mismatch: expected " <> show l <> ", got " <> show readDataU
   where
    readDataU = unpackOrErrorC endian (unpack readData)
  model (Write _ _ ((`testBit` (32 - 8)) -> doFreeze)) _ s
    -- Only one writeable register in this device: freeze. We can therefore safely
    -- ignore the address and assume that register is written to. We know the bool
    -- is packed into the first byte. The relevant bit is in the LSB of the first
    -- byte, hence 32-8.
    | doFreeze =
        Right
          $ s
            { frozen = True
            , lastSeen = Nothing
            , nFreezes = s.nFreezes + 1
            }
    | otherwise =
        Right s

  genInputs :: Gen [WishboneMasterRequest AddressWidth (Bytes 4)]
  genInputs = Gen.list (Range.linear 0 500) (genWishboneTransfer genAddr genData)

  genData :: Gen (BitVector 32)
  genData = Gen.integral (Range.constant 0 maxBound)

  genAddr :: Gen (BitVector AddressWidth)
  genAddr = Gen.integral (Range.constant 0 maxBound)

  dutMm ::
    Circuit (ConstBwd MM, Wishbone XilinxSystem Standard AddressWidth (BitVector 32)) ()
  dutMm =
    let
      ?regByteOrder = endian
      ?busByteOrder = endian
     in
      circuit $ \(mm, wb) -> do
        freeze @4 @32 clk rst
          -< ( mm
             , wb
             , Fwd ebCounters
             , Fwd localCounter
             , Fwd syncPulseCounter
             , Fwd lastPulseCounter
             )

  -- Input registers that are spaced one apart. This allows us to predict the
  -- value of all counters, by just reading one. Note that this only works if
  -- the order of the registers in the implementation is the same as defined
  -- here.
  localCounter = counter clk rst ena 0
  syncPulseCounter = counter clk rst ena 1
  lastPulseCounter = counter clk rst ena 2
  ebCounters = bundle $ counter clk rst ena <$> iterateI (+ 1) 3

  dut :: Circuit (Wishbone XilinxSystem Standard AddressWidth (BitVector 32)) ()
  dut = unMemmap dutMm

tests :: TestTree
tests =
  testGroup
    "Freeze"
    [ testPropertyNamed "prop_wb" "prop_wb" prop_wb
    ]
