-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Tests.ClockControl.Freeze where

import Bittide.ClockControl.Freeze (counter, freeze)
import Bittide.SharedTypes (withByteOrderings)
import Clash.Class.BitPackC (ByteOrder (BigEndian), unpackOrErrorC)
import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)
import Hedgehog (Gen, Property)
import Hedgehog.Internal.Property (property)
import Protocols
import Protocols.Hedgehog (defExpectOptions, eoSampleMax)
import Protocols.MemoryMap
import Protocols.Wishbone
import Protocols.Wishbone.Standard.Hedgehog (
  WishboneMasterRequest (..),
  wishbonePropWithModel,
 )
import Test.Tasty
import Test.Tasty.Hedgehog (testPropertyNamed)

import Clash.Hedgehog.Sized.BitVector (genDefinedBitVector)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

type AddressWidth = 4

genWishboneTransfer ::
  (KnownNat aw) =>
  Gen (BitVector aw) ->
  Gen (WishboneMasterRequest aw 4)
genWishboneTransfer genAddr =
  Gen.choice
    [ Read <$> genAddr <*> pure maxBound
    , Write <$> genAddr <*> pure maxBound <*> genDefinedBitVector
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
      defExpectOptions{eoSampleMax = 10_000}
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
    WishboneMasterRequest AddressWidth 4 ->
    WishboneS2M 4 ->
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
    | a < 3 = Right s
    | otherwise =
        -- Record the value of the register that is being read. This can predict the
        -- value of all other registers (until a freeze is requested).
        Right s{lastSeen = Just (unpackOrErrorC endian (unpack readData) - fromIntegral a)}
  model (Read a _) WishboneS2M{readData} s@ModelState{lastSeen = Just l}
    | a < 3 = Right s
    | readDataU - fromIntegral a == l = Right s
    | otherwise =
        Left $ "Read value mismatch: expected " <> show l <> ", got " <> show readDataU
   where
    readDataU = unpackOrErrorC endian (unpack readData)
  model (Write _ _ _) _ s =
    -- Only one writeable register in this device: freeze. We can therefore safely
    -- ignore the address and assume that register is written to.
    Right
      $ s
        { frozen = True
        , lastSeen = Nothing
        , nFreezes = s.nFreezes + 1
        }

  genInputs :: Gen [WishboneMasterRequest AddressWidth 4]
  genInputs = Gen.list (Range.linear 0 500) (genWishboneTransfer genAddr)

  genAddr :: Gen (BitVector AddressWidth)
  genAddr = Gen.integral (Range.constant 0 maxBound)

  dutMm ::
    Circuit (ToConstBwd Mm, Wishbone XilinxSystem Standard AddressWidth 4) ()
  dutMm = withByteOrderings endian endian $ circuit $ \(mm, wb) -> do
    freeze @4 @32 clk rst
      -< ( (mm, wb)
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
  ebCounters = bundle $ counter clk rst ena <$> iterateI (+ 1) (3 :: Signed 32)

  dut :: Circuit (Wishbone XilinxSystem Standard AddressWidth 4) ()
  dut = unMemmap dutMm

tests :: TestTree
tests =
  testGroup
    "Freeze"
    [ testPropertyNamed "prop_wb" "prop_wb" prop_wb
    ]
