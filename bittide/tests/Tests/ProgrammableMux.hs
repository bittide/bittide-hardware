-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Tests.ProgrammableMux where

import Bittide.ElasticBuffer (sticky)
import Bittide.ProgrammableMux (programmableMux)
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
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.Hedgehog (testPropertyNamed)

import Clash.Hedgehog.Sized.BitVector (genDefinedBitVector)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Prelude

type AddressWidth = 4
type LinkCount = 2

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
  { cycleToSwitch :: Unsigned 64
  , armed :: Bool
  , triggered :: Bool
  }
  deriving (Show, Eq)

initModelState :: ModelState
initModelState = ModelState{cycleToSwitch = maxBound, armed = False, triggered = False}

{- | Test 'programmableMux' by issuing read and write requests. The model
tests that:

 * The cycle_to_switch register can be read and written correctly.
 * The arm register can be written (it's write-only).
 * After arming and reaching the switch cycle, the component stays in triggered state.
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
  -- Read from cycle_to_switch register (address 0, spans addresses 0-1 as it's 64-bit)
  model (Read a _) WishboneS2M{readData} s
    | a == 0 =
        let expected = resize (s.cycleToSwitch) :: Unsigned 32
            readDataU = unpackOrErrorC endian (unpack readData)
         in if readDataU == expected
              then Right s
              else
                Left
                  $ "cycle_to_switch low word mismatch: expected "
                  <> show expected
                  <> ", got "
                  <> show readDataU
    | a == 1 =
        let expected = resize (s.cycleToSwitch `shiftR` 32) :: Unsigned 32
            readDataU = unpackOrErrorC endian (unpack readData)
         in if readDataU == expected
              then Right s
              else
                Left
                  $ "cycle_to_switch high word mismatch: expected "
                  <> show expected
                  <> ", got "
                  <> show readDataU
    -- Reading from arm register (address 2) - it's write-only, so we don't check
    | a == 2 = Right s
    -- Other addresses
    | otherwise = Right s
  -- Write to cycle_to_switch register (low word at address 0)
  model (Write 0 _ writeData) _ s =
    let lowWord = unpackOrErrorC endian (unpack writeData) :: Unsigned 32
        highWord = resize (s.cycleToSwitch `shiftR` 32) :: Unsigned 32
        newValue = (shiftL (resize highWord) 32) .|. resize lowWord
     in Right s{cycleToSwitch = newValue}
  -- Write to cycle_to_switch register (high word at address 1)
  model (Write 1 _ writeData) _ s =
    let lowWord = resize s.cycleToSwitch :: Unsigned 32
        highWord = unpackOrErrorC endian (unpack writeData) :: Unsigned 32
        newValue = (shiftL (resize highWord) 32) .|. resize lowWord
     in Right s{cycleToSwitch = newValue}
  -- Write to arm register (address 2)
  model (Write 2 _ _) _ s = Right s{armed = True}
  -- Other writes
  model (Write _ _ _) _ s = Right s

  genInputs :: Gen [WishboneMasterRequest AddressWidth 4]
  genInputs = Gen.list (Range.linear 0 500) (genWishboneTransfer genAddr)

  genAddr :: Gen (BitVector AddressWidth)
  genAddr = Gen.integral (Range.constant 0 maxBound)

  dutMm ::
    Circuit (ToConstBwd Mm, Wishbone XilinxSystem Standard AddressWidth 4) ()
  dutMm = withByteOrderings endian endian $ circuit $ \(mm, wb) -> do
    (Fwd _peReset, Fwd _linkOut) <-
      withClockResetEnable
        clk
        rst
        ena
        (programmableMux @_ @AddressWidth @LinkCount localCounter)
        -< ((mm, wb), Fwd muLinks, Fwd peLinks)
    idC -< ()

  localCounter :: Signal XilinxSystem (Unsigned 64)
  localCounter = register clk rst ena 0 (localCounter + 1)

  muLinks :: Vec LinkCount (Signal XilinxSystem (BitVector 64))
  muLinks =
    let mkLink upper = (\cnt -> (upper `shiftL` 32) .|. resize (pack cnt)) <$> localCounter
     in mkLink 0xAAAA_AAAA :> mkLink 0xBBBB_BBBB :> Nil

  peLinks :: Vec LinkCount (Signal XilinxSystem (BitVector 64))
  peLinks =
    let mkLink upper = (\cnt -> (upper `shiftL` 32) .|. resize (pack cnt)) <$> localCounter
     in mkLink 0x1111_1111 :> mkLink 0x2222_2222 :> Nil

  dut :: Circuit (Wishbone XilinxSystem Standard AddressWidth 4) ()
  dut = unMemmap dutMm

{- | Test that the mux correctly switches between MU and PE links
and that peReset follows the expected behavior.
-}
test_outputs :: Assertion
test_outputs = do
  let
    switchCycle = 10 :: Unsigned 64
    testCycles = 30 :: Int

    clk = clockGen @XilinxSystem
    rst = resetGen @XilinxSystem
    ena = enableGen @XilinxSystem

    localCounter =
      withClockResetEnable clk rst ena
        $ register clk rst ena 0 (localCounter + 1)

    mkLink :: BitVector 32 -> Signal XilinxSystem (BitVector 64)
    mkLink upper = (\cnt -> (resize upper `shiftL` 32) .|. resize (pack cnt)) <$> localCounter
    muLinks = mkLink 0xAAAA_AAAA :> mkLink 0xBBBB_BBBB :> Nil
    peLinks = mkLink 0x1111_1111 :> mkLink 0x2222_2222 :> Nil

    -- Arm the mux to switch at switchCycle
    cycleToSwitchSig = pure switchCycle
    armSig = register clk rst ena False (pure True) -- Arms on cycle 1

    -- Manually compute trigger and peReset (matching the implementation)
    trigger = sticky clk rst $ armSig .&&. (localCounter .==. cycleToSwitchSig)
    peReset = fmap not trigger

    -- Manually compute mux output
    linkOut =
      withClockResetEnable clk rst ena
        $ (\t mu pe -> if t then pe else mu)
        <$> trigger
        <*> bundle muLinks
        <*> bundle peLinks

    -- Sample outputs
    counterSamples = Prelude.take testCycles $ sample_lazy localCounter
    peResetSamples = Prelude.take testCycles $ sample_lazy peReset
    linkOutSamples = Prelude.take testCycles $ sample_lazy linkOut

  -- Check each cycle
  Prelude.sequence_
    $ Prelude.zipWith3
      ( \(cycles :: Unsigned 64) peResetVal linkVec -> do
          -- Check peReset (trigger happens at switchCycle, becomes False one cycle later due to register)
          let expectedPeReset = cycles < switchCycle + 1
          (peResetVal, cycles) @?= (expectedPeReset, cycles)

          -- Check link outputs
          let
            triggered = cycles >= switchCycle + 1
            expectedUpper0 = if triggered then 0x1111_1111 else 0xAAAA_AAAA
            expectedUpper1 = if triggered then 0x2222_2222 else 0xBBBB_BBBB
            expectedLower = resize (pack cycles) :: BitVector 32
            expected0 = (expectedUpper0 `shiftL` 32) .|. resize expectedLower
            expected1 = (expectedUpper1 `shiftL` 32) .|. resize expectedLower

          (linkVec !! (0 :: Int)) @?= expected0
          (linkVec !! (1 :: Int)) @?= expected1
      )
      counterSamples
      peResetSamples
      linkOutSamples

tests :: TestTree
tests =
  testGroup
    "ProgrammableMux"
    [ testPropertyNamed "prop_wb" "prop_wb" prop_wb
    , testCase "test_outputs" test_outputs
    ]
