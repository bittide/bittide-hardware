-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
-- It's a test, we'll see it :-)
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Tests.Protocols.MemoryMap.Registers.WishboneStandard where

import Clash.Class.BitPackC (BitPackC, ByteOrder (BigEndian))
import Clash.Class.BitPackC.Padding (SizeInWordsC, maybeUnpackWordC, packWordC)
import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)
import Control.DeepSeq (force)
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)
import Hedgehog (Gen, Property)
import Hedgehog.Internal.Property (property)
import Protocols
import Protocols.Hedgehog (defExpectOptions, eoSampleMax)
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

initU32 :: Unsigned 32
initU32 = 122222

type AddressWidth = 4

wordSize :: SNat 4
wordSize = SNat

-- For these tests we use @BigEndian@ to make the (somewhat grimy..) logic in
-- these tests work.
regByteOrder :: ByteOrder
regByteOrder = BigEndian

busByteOrder :: ByteOrder
busByteOrder = BigEndian

myPaddedPackC :: (BitPackC a) => a -> Vec (SizeInWordsC 4 a) (Bytes 4)
myPaddedPackC = packWordC regByteOrder

myPaddedUnpackC :: (BitPackC a) => Vec (SizeInWordsC 4 a) (Bytes 4) -> a
myPaddedUnpackC =
  fromMaybe (errorX "myPaddedUnpackC: fail to unpack")
    . maybeUnpackWordC regByteOrder

{- | Initial state of 'deviceExample', represented as a map from address to bit
size and value.
-}
initState :: Map.Map (BitVector AddressWidth) (BitVector 32)
initState =
  Map.fromList @(BitVector AddressWidth)
    -- TODO: zero-width registers
    [ (0, myPaddedPackC initFloat !! nil)
    , (1, myPaddedPackC initDouble !! nil)
    , (2, myPaddedPackC initDouble !! succ nil)
    , (3, myPaddedPackC initU32 !! nil)
    , (4, myPaddedPackC initFloat !! nil)
    , (5, myPaddedPackC initFloat !! nil)
    , (6, myPaddedPackC initU32 !! nil)
    ]
 where
  nil = 0 :: Int

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
  , ?regByteOrder :: ByteOrder
  , ?busByteOrder :: ByteOrder
  ) =>
  Clock dom ->
  Reset dom ->
  Circuit
    (ConstBwd MM, Wishbone dom 'Standard aw (Bytes wordSize))
    ()
deviceExample clk rst = circuit $ \(mm, wb) -> do
  [float, double, u32, readOnly, writeOnly, prio] <-
    deviceWbC "example" -< (mm, wb)

  registerWbC_ clk rst (registerConfig "f") initFloat -< (float, Fwd noWrite)
  registerWbC_ clk rst (registerConfig "d") initDouble -< (double, Fwd noWrite)
  registerWbC_ clk rst (registerConfig "u") initU32 -< (u32, Fwd noWrite)

  registerWbC_ clk rst (registerConfig "ro"){access = ReadOnly} initFloat
    -< (readOnly, Fwd noWrite)
  registerWbC_ clk rst (registerConfig "wo"){access = WriteOnly} initFloat
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
  * Read/write transactions to RO/WO registers

It currently does NOT test:

  * "Oddly" shaped types (e.g. @Signed 21@)
  * 'deviceWithOffsetsWbC' with gaps between registers
  * Varying the size of the Wishbone bus
-}
prop_wb :: Property
prop_wb =
  property
    $ withClockResetEnable clk rst ena
    $ wishbonePropWithModel @XilinxSystem
      defExpectOptions{eoSampleMax = 10_000}
      model
      dut
      genInputs
      initState
 where
  clk = clockGen
  rst = resetGen
  ena = enableGen

  model ::
    WishboneMasterRequest AddressWidth (BitVector 32) ->
    WishboneS2M (BitVector 32) ->
    Map.Map (BitVector AddressWidth) (BitVector 32) ->
    Either String (Map.Map (BitVector AddressWidth) (BitVector 32))
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
        Just v
          | isRead instr && errorAddress == woAddress ->
              -- Expect an error when trying to read from the WriteOnly register
              Right s
          | isWrite instr && errorAddress == roAddress ->
              -- Expect an error when trying to write to the ReadOnly register
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
      Just v
        | v == readData && a == prioAddress ->
            Right $ Map.insert prioAddress (head $ myPaddedPackC initU32) s
        | v == readData ->
            Right s
        | otherwise ->
            Left $ "a: " <> show a <> ", v: " <> show v <> ", readData: " <> show readData
  model (Write a m newDat) _ s
    | a == prioAddress =
        let
          inc :: BitVector 32 -> BitVector 32
          inc = head . myPaddedPackC . (+ (1 :: (Unsigned 32))) . myPaddedUnpackC . pure
         in
          Right (Map.adjust (inc . update) a s)
    | otherwise =
        Right (Map.adjust update a s)
   where
    update :: BitVector 32 -> BitVector 32
    update oldDat = head $ maskWriteData @4 @1 0 m newDat (oldDat :> Nil)

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
  dut =
    let
      ?regByteOrder = regByteOrder
      ?busByteOrder = busByteOrder
     in
      unMemmap $ deviceExample @4 @AddressWidth @XilinxSystem clk rst

  example = memoryMap.deviceDefs Map.! "example"
  [_regF, _regD, _regU, regRO, regWO, regPrio] = example.registers
  woAddress = fromIntegral $ regWO.value.address `div` 4
  roAddress = fromIntegral $ regRO.value.address `div` 4
  prioAddress = fromIntegral $ regPrio.value.address `div` 4

{- FOURMOLU_DISABLE -}
case_maskWriteData :: Assertion
case_maskWriteData = do
  let
    stored   = (0x01234567 :> 0x89abcdef :> 0xfedcba98 :> 0x76543210 :> Nil)
    bus      = 0x_______________13579bdf
    mask     = 0b_______________1_1_0_1
    expected = (0x01234567 :> 0x1357cddf :> 0xfedcba98 :> 0x76543210 :> Nil)

  maskWriteData @4 @4 1 mask bus stored @?= expected
{- FOURMOLU_ENABLE -}

unSimOnly :: SimOnly a -> a
unSimOnly (SimOnly x) = x

memoryMap :: MemoryMap
memoryMap =
  unSimOnly
    $ getConstBwdAny
    $ let
        ?regByteOrder = regByteOrder
        ?busByteOrder = busByteOrder
       in
        deviceExample @4 @AddressWidth @XilinxSystem clockGen noReset

{- | Test that the memory map can be generated without errors. Test for sensible
values in the memory map.
-}
case_memoryMap :: Assertion
case_memoryMap = do
  let !_tree = force (ppShow memoryMap)

  let example = memoryMap.deviceDefs Map.! "example"
  example.deviceName.name @?= "example"

  let [regF, regD, regU, regRO, regWO, regPrio] = example.registers

  regF.name.name @?= "f"
  regD.name.name @?= "d"
  regU.name.name @?= "u"
  regRO.name.name @?= "ro"
  regWO.name.name @?= "wo"
  regPrio.name.name @?= "prio"

  regF.value.address @?= 0
  regD.value.address @?= 4
  regU.value.address @?= 12
  regRO.value.address @?= 16
  regWO.value.address @?= 20
  regPrio.value.address @?= 24

tests :: TestTree
tests =
  testGroup
    "WishboneStandard"
    [ testCase "case_maskWriteData" case_maskWriteData
    , testCase "case_memoryMap" case_memoryMap
    , testPropertyNamed "prop_wb" "prop_wb" prop_wb
    ]
