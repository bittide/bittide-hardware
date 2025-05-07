-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
-- It's a test, we'll see it :-)
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Tests.Protocols.MemoryMap.Registers.WishboneStandard where

import BitPackC (BitPackC (packC, unpackC))
import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)
import Control.DeepSeq (force)
import Data.Bifunctor (second)
import Data.Tuple.Extra (fst3, thd3)
import GHC.Stack (HasCallStack)
import Hedgehog (Gen, Property)
import Hedgehog.Internal.Property (property)
import Protocols
import Protocols.Hedgehog (defExpectOptions)
import Protocols.MemoryMap
import Protocols.MemoryMap.Registers.WishboneStandard
import Protocols.Wishbone
import Protocols.Wishbone.Standard.Hedgehog (
  WishboneMasterRequest (..),
  wishbonePropWithModel,
 )
import Test.Tasty
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.Hedgehog (testPropertyNamed)
import Text.Show.Pretty (ppShow)

import qualified Data.Map as Map
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Prelude as P

type Bytes n = BitVector (n * 8)

initFloat :: Float
initFloat = 3.0

initDouble :: Double
initDouble = 6.0

initU16 :: Unsigned 16
initU16 = 12

initBool :: Bool
initBool = True

initS16 :: Signed 16
initS16 = 500

initEmpty :: Signed 0
initEmpty = 0

initU32 :: Unsigned 32
initU32 = 122222

type AddressWidth = 4

{- | Initial state of 'deviceExample', represented as a map from address to bit
size and value.
-}
initState :: Map.Map (BitVector AddressWidth) (Int, BitVector 32)
initState =
  Map.fromList @(BitVector AddressWidth)
    [ (0, (32, packC initFloat))
    , (1, (32, truncateB $ packC initDouble))
    , (2, (32, truncateB $ shiftR (packC initDouble) 32))
    , (3, (16, extend $ packC initU16))
    , (4, (1, extend $ packC initBool))
    , -- Zero width registers take a single address due to BitPackC claiming
      -- they take a single byte to represent.
      (5, (0, 0))
    , -- XXX: Using an "odd" size here (e.g., 21) would require the model to
      --      account for endianness. Either that, or I'm not really understanding
      --      why it fails.
      (6, (16, extend $ packC initS16))
    , (7, (32, packC initFloat))
    , (8, (32, packC initFloat))
    , (9, (32, packC initU32))
    ]

{- | A simple device example that uses the Wishbone protocol to read and write
registers. The device has a number of registers with different access rights, widths
and types, including floating point, integer, and boolean values.
-}
deviceExample ::
  forall wordSize aw dom.
  ( HasCallStack
  , KnownDomain dom
  , KnownNat wordSize
  , KnownNat aw
  , 1 <= wordSize
  ) =>
  Clock dom ->
  Reset dom ->
  Circuit
    (ConstBwd MM, Wishbone dom 'Standard aw (Bytes wordSize))
    ()
deviceExample clk rst = circuit $ \(mm, wb) -> do
  [float, double, u16, bool, empty, s16, readOnly, writeOnly, prio] <-
    deviceWbC "example" -< (mm, wb)

  _f <- registerWbC clk rst (registerConfig "f") initFloat -< (float, Fwd noWrite)
  _d <- registerWbC clk rst (registerConfig "d") initDouble -< (double, Fwd noWrite)
  _u <- registerWbC clk rst (registerConfig "u") initU16 -< (u16, Fwd noWrite)
  _b <- registerWbC clk rst (registerConfig "b") initBool -< (bool, Fwd noWrite)
  _e <- registerWbC clk rst (registerConfig "e") initEmpty -< (empty, Fwd noWrite)
  _s <- registerWbC clk rst (registerConfig "s") initS16 -< (s16, Fwd noWrite)

  _ro <-
    registerWbC clk rst (registerConfig "ro"){access = ReadOnly} initFloat
      -< (readOnly, Fwd noWrite)
  _wo <-
    registerWbC clk rst (registerConfig "wo"){access = WriteOnly} initFloat
      -< (writeOnly, Fwd noWrite)

  (_a, Fwd prioOut) <-
    registerWbC clk rst (registerConfig "prio") initU32
      -< (prio, Fwd (overwrite <$> prioOut))

  idC
 where
  noWrite = pure Nothing

  overwrite = \case
    -- On bus writes 'the circuit' increments the written value by 1
    BusWrite newA -> Just (newA + 1)
    -- On bus reads, the circuit resets the register to its initial value
    BusRead _ -> Just initU32
    BusIdle -> Nothing

genWishboneTransfer ::
  ( KnownNat aw
  , KnownNat n
  ) =>
  Gen (BitVector aw) ->
  Gen (BitVector (DivRU n 8)) ->
  Gen (BitVector n) ->
  Gen (WishboneMasterRequest aw (BitVector n))
genWishboneTransfer genAddr genMask genData =
  Gen.choice
    [ Read <$> genAddr <*> genMask
    , Write <$> genAddr <*> genMask <*> genData
    ]

{- | Test 'deviceExample' (and therefore 'deviceWbC' and 'registerWbC') using
'wishbonePropWithModel'. This property generates a number of random Wishbone
transactions and checks that the device behaves as expected.

It currently tests:

  * Read/write transactions on both mapped and unmapped addresses
  * Read/write transactions with varying byte enable masks

It currently does NOT test:

  * "Oddly" shaped types (e.g. @Signed 21@)
  * 'deviceWithOffsetsWbC' with gaps between registers
  * Varying the size of the Wishbone bus
-}
prop_wb :: Property
prop_wb =
  property
    $ withClockResetEnable clk rst ena
    $ wishbonePropWithModel @XilinxSystem defExpectOptions model dut genInputs initState
 where
  clk = clockGen
  rst = resetGen
  ena = enableGen

  model ::
    WishboneMasterRequest AddressWidth (BitVector 32) ->
    WishboneS2M (BitVector 32) ->
    Map.Map (BitVector AddressWidth) (Int, BitVector 32) ->
    Either String (Map.Map (BitVector AddressWidth) (Int, BitVector 32))
  model instr WishboneS2M{err = True} s =
    -- Errors should only happen when we use an unmapped address (in the future
    -- we may want to to test other errors too).
    let
      errorAddress = case instr of
        Read a _ -> a
        Write a _ _ -> a

      isRead (Read _ _) = True
      isRead _ = False

      isWrite (Write{}) = True
      isWrite _ = False
     in
      case Map.lookup errorAddress s of
        Nothing ->
          -- Whenever an error occurs, the state should be unchanged.
          Right s
        Just (_, v)
          | isRead instr && errorAddress == 8 ->
              -- Expect an error when trying to read from the WriteOnly register on address 8.
              Right s
          | isWrite instr && errorAddress == 7 ->
              -- Expect an error when trying to write to the ReadOnly register on address 7.
              Right s
          | otherwise ->
              Left $ "Error on address: " <> show errorAddress <> ", value: " <> show v
  model _ WishboneS2M{retry = True} s = Right s
  model _ WishboneS2M{acknowledge = False} s = Right s
  model (Read a _) WishboneS2M{readData} s =
    -- XXX: Note that we IGNORE the byte enable mask when reading. The circuit
    --      does too.
    case Map.lookup a s of
      Nothing -> Left $ "Read from unmapped address: " <> show a
      Just (_, v)
        | v == readData && a == 9 -> Right $ Map.insert 9 (32, packC initU32) s
        | v == readData ->
            Right s
        | otherwise ->
            Left $ "a: " <> show a <> ", v: " <> show v <> ", readData: " <> show readData
  model (Write a m newDat) _ s
    | a == 9 =
        let
          inc :: BitVector 32 -> BitVector 32
          inc = packC . (+ (1 :: (Unsigned 32))) . unpackC
         in
          Right (Map.adjust (second inc . update) a s)
    | otherwise =
        Right (Map.adjust update a s)
   where
    update :: (Int, BitVector 32) -> (Int, BitVector 32)
    update (size, oldDat) = (size, truncatedMergedDat)
     where
      truncatedMergedDat = mergedDat .&. (2 P.^ size - 1)
      mergedDat = maskWriteData @4 @1 0 m newDat oldDat

  genInputs :: Gen [WishboneMasterRequest AddressWidth (Bytes 4)]
  genInputs = Gen.list (Range.linear 0 300) (genWishboneTransfer genAddr genMask genData)

  genMask :: Gen (BitVector 4)
  genMask = Gen.integral (Range.linear 0 maxBound)

  genData :: Gen (BitVector 32)
  genData = Gen.integral (Range.linear 0 maxBound)

  genAddr :: Gen (BitVector AddressWidth)
  genAddr =
    -- We do plus one to test unmapped addresses
    Gen.integral (Range.constant 0 (1 + P.maximum (Map.keys initState)))

  dut :: Circuit (Wishbone XilinxSystem Standard AddressWidth (BitVector 32)) ()
  dut = unMemmap $ deviceExample @4 @AddressWidth @XilinxSystem clk rst

{- FOURMOLU_DISABLE -}
case_maskWriteData :: Assertion
case_maskWriteData = do
  let
    stored   = 0x01234567_89abcdef_fedcba98_76543210 :: BitVector 128
    bus      = 0x_________13579bdf                   :: BitVector 32
    mask     = 0b_________1_1_0_1                    :: BitVector 4
    expected = 0x01234567_1357cddf_fedcba98_76543210 :: BitVector 128

  maskWriteData 2 mask bus stored @?= expected
{- FOURMOLU_ENABLE -}

{- | Test that the memory map can be generated without errors. Test for sensible
values in the memory map.
-}
case_memoryMap :: Assertion
case_memoryMap = do
  let
    unSimOnly (SimOnly x) = x
    device = deviceExample @4 @AddressWidth @XilinxSystem clockGen noReset
    memoryMap = unSimOnly (getConstBwdAny device)
    example = memoryMap.deviceDefs Map.! "example"

    -- Make sure the whole tree renders without errors
    !_tree = force (ppShow memoryMap)

  example.deviceName.name @?= "example"

  let [regF, regD, regU, regB, regE, regS, regRO, regWO, regPrio] = example.registers

  (fst3 regF).name @?= "f"
  (fst3 regD).name @?= "d"
  (fst3 regU).name @?= "u"
  (fst3 regB).name @?= "b"
  (fst3 regE).name @?= "e"
  (fst3 regS).name @?= "s"
  (fst3 regRO).name @?= "ro"
  (fst3 regWO).name @?= "wo"
  (fst3 regPrio).name @?= "prio"

  (thd3 regF).address @?= 0
  (thd3 regD).address @?= 1
  (thd3 regU).address @?= 3
  (thd3 regB).address @?= 4
  (thd3 regE).address @?= 5
  (thd3 regS).address @?= 6
  (thd3 regRO).address @?= 7
  (thd3 regWO).address @?= 8
  (thd3 regPrio).address @?= 9

tests :: TestTree
tests =
  testGroup
    "WishboneStandard"
    [ testCase "case_maskWriteData" case_maskWriteData
    , testCase "case_memoryMap" case_memoryMap
    , testPropertyNamed "prop_wb" "prop_wb" prop_wb
    ]
