-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
-- It's a test, we'll see it :-)
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Tests.Protocols.MemoryMap.Registers.WishboneStandard where

import Clash.Explicit.Prelude

import Clash.Class.BitPackC (ByteOrder (..))
import Clash.Class.BitPackC.Padding (packWordCI, unpackWordOrErrorCI)
import Clash.Hedgehog.Sized.Vector (genVec)
import Clash.Prelude (withClockResetEnable)
import Control.DeepSeq (force)
import Data.String.Interpolate (i)
import GHC.Stack (HasCallStack)
import Hedgehog (Gen, Property)
import Hedgehog.Internal.Property (property)
import Protocols
import Protocols.Hedgehog (defExpectOptions, eoResetCycles, eoSampleMax)
import Protocols.MemoryMap
import Protocols.MemoryMap.Registers.WishboneStandard
import Protocols.MemoryMap.Registers.WishboneStandard.Internal
import Protocols.Wishbone
import Protocols.Wishbone.Standard.Hedgehog (
  WishboneMasterRequest (..),
  driveStandard,
  wishbonePropWithModel,
 )
import System.Directory (createDirectoryIfMissing)
import Test.Tasty
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.Hedgehog (testPropertyNamed)
import Text.Show.Pretty (ppShow)

import qualified Clash.Prelude as CP
import qualified Clash.Shockwaves as Shockwaves
import qualified Clash.Shockwaves.Trace as T
import qualified Clash.Shockwaves.Trace.CRE as T
import qualified Data.Map as Map
import qualified Data.Text.IO as TIO
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Protocols.Hedgehog as PH
import qualified Protocols.ReqResp as ReqResp
import qualified Prelude as P

type Bytes n = BitVector (n * 8)

initFloat :: Float
initFloat = 3.0

initDouble :: Double
initDouble = 6.0

initU32 :: Unsigned 32
initU32 = 122222

type AddressWidth = 4

smallInt :: Gen Int
smallInt = Gen.integral (Range.linear 0 10)

genStalls :: (KnownNat n) => Gen (Vec n ((StallAck, [Int])))
genStalls = do
  numStalls <- smallInt
  genVec (PH.genStalls smallInt numStalls PH.Stall)

{- | Whether to trace signals in 'deviceExample'. Disabled for Hedgehog tests, enabled for
the replay test.
-}
data Trace = Trace | NoTrace

traceToBool :: Trace -> Bool
traceToBool Trace = True
traceToBool NoTrace = False

{- | Initial state of 'deviceExample', represented as a map from address to bit
size and value.
-}
initState :: (?byteOrder :: ByteOrder) => Map.Map (BitVector AddressWidth) (BitVector 32)
initState =
  Map.fromList @(BitVector AddressWidth)
    -- TODO: zero-width registers
    [ (0, packWordCI initFloat !! nil)
    , (1, packWordCI initDouble !! nil)
    , (2, packWordCI initDouble !! succ nil)
    , (3, packWordCI initU32 !! nil)
    , (4, packWordCI initFloat !! nil)
    , (5, packWordCI initFloat !! nil)
    , (6, packWordCI initU32 !! nil)
    , (7, packWordCI initU32 !! nil)
    , (8, packWordCI initU32 !! nil)
    , (9, packWordCI False !! nil)
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
  ) =>
  Trace ->
  Clock dom ->
  Reset dom ->
  Circuit
    (ToConstBwd Mm, Wishbone dom 'Standard aw wordSize)
    ()
deviceExample trace clk rst = circuit $ \(mm, wb) -> do
  [float, double, u32, readOnly, writeOnly, prio, prioPreferCircuit, delayed, delayedError] <-
    deviceWb (deviceConfig "example"){trace = traceToBool trace} -< (mm, wb)

  registerWb_ clk rst (registerConfig "f") initFloat -< (float, Fwd noWrite)
  registerWb_ clk rst (registerConfig "d") initDouble -< (double, Fwd noWrite)
  registerWb_ clk rst (registerConfig "u") initU32 -< (u32, Fwd noWrite)

  registerWb_ clk rst (registerConfig "ro"){access = ReadOnly} initFloat
    -< (readOnly, Fwd noWrite)
  registerWb_ clk rst (registerConfig "wo"){access = WriteOnly} initFloat
    -< (writeOnly, Fwd noWrite)

  (_a0, Fwd prioOut) <-
    registerWb clk rst (registerConfig "prio") initU32
      -< (prio, Fwd (fmap overwrite <$> prioOut))

  -- A register called 'prio_prefer_circuit', which is like 'prio' but expects to
  -- immediately read back circuit writes -- in this case resetting to the initial
  -- value.
  (_a1, Fwd prioPreferCircuitOut) <-
    registerWb clk rst (registerConfig "prio_prefer_circuit"){busRead = PreferCircuit} initU32
      -< (prioPreferCircuit, Fwd (fmap overwrite <$> prioPreferCircuitOut))

  -- Insert one register called 'delayed'. We want to test the following properties:
  --
  --   1. Whether we can delay a write
  --   2. Whether circuit writes get ignored while a bus write gets delayed
  --   3. Whether circuit writes do *not* get ignored while acknowledging a bus
  --      transaction
  --
  -- To do this, we delay a write according to the logic in 'goDelay'. We test the
  -- properties as follows:
  --
  --   1. If we couldn't delay, we'd expect odd values in 'delayed' (triggering
  --      an error in 'delayed_error') due to either a bus write randomly trying to
  --      write an odd number or by it being inserted by the last case of 'goDelay'.
  --
  --   2. If circuit writes wouldn't get ignored, the last case of 'goDelay' would
  --      write an odd value into the register (triggering an error).
  --
  --   3. If circuit writes get ignored while acknowledging, a bus write would
  --      randomly write odd values to the register.
  (Fwd delayedActual, delayedDf) <-
    registerWbDf clk rst (registerConfig "delayed") initU32
      -< (delayed, delayedWrite)

  delayedWrite <-
    Circuit (unbundle . fmap goDelay . bundle . (,delayBy) . fst)
      -< delayedDf

  let unexpectedDelayedValue = sticky (testBit <$> delayedActual <*> pure 0)
  registerWb_ clk rst (registerConfig "delayed_error"){access = ReadOnly} False
    -< (delayedError, Fwd (Just <$> unexpectedDelayedValue))

  idC
 where
  ena = enableGen
  noWrite = pure Nothing

  sticky input = let s = register clk rst ena False (s .||. input) in s

  goDelay ::
    (Maybe (BusActivity (Unsigned 32)), Unsigned 4) ->
    (Ack, Maybe (Unsigned 32))
  goDelay = \case
    (Nothing, d) ->
      -- If there is no bus activity, we "randomize" whether we ack -- it shouldn't
      -- matter what we do. We also don't write anything to the register.
      (Ack (testBit d 0), Nothing)
    (Just (BusWrite n), 0) ->
      -- A bus write gets multiplied by 2, to make sure the register contains an even
      -- number to not trigger 'delayedError'.
      (Ack True, Just (n * 2))
    (Just (BusRead _), 0) ->
      -- A bus read gets acknowledged, but nothing gets written
      (Ack True, Nothing)
    (Just _, _) ->
      -- Delay bus reads and writes. Circuit writes should *not* be accepted, so we try
      -- and write an odd number. If this success, 'delayedError' will be triggered.
      (Ack False, Just 1)

  -- Delay a bus write by a "random" number of clock cycles
  delayBy :: Signal dom (Unsigned 4)
  delayBy = register clk rst ena 0 (delayBy - 1)

  overwrite = \case
    -- On bus writes 'the circuit' increments the written value by 1
    BusWrite newA -> (newA + 1)
    -- On bus reads, the circuit resets the register to its initial value
    BusRead _ -> initU32

genWishboneTransfer ::
  ( KnownNat aw
  , KnownNat n
  ) =>
  Gen (BitVector aw) ->
  Gen (BitVector n) ->
  Gen (BitVector (n * 8)) ->
  Gen (WishboneMasterRequest aw n)
genWishboneTransfer genAddr genMask genData =
  Gen.choice
    [ Read <$> genAddr <*> genMask
    , Write <$> genAddr <*> genMask <*> genData
    ]

prop_wbBigEndian :: Property
prop_wbBigEndian =
  let ?regByteOrder = BigEndian
      ?byteOrder = BigEndian
   in prop_wb

prop_wbLittleEndian :: Property
prop_wbLittleEndian =
  let ?regByteOrder = LittleEndian
      ?byteOrder = LittleEndian
   in prop_wb

{- | Test 'deviceExample' (and therefore 'deviceWb' and 'registerWb') using
'wishbonePropWithModel'. This property generates a number of random Wishbone
transactions and checks that the device behaves as expected.

It currently tests:

  * Read/write transactions on both mapped and unmapped addresses
  * Read/write transactions with varying byte enable masks
  * Read/write transactions to RO/WO registers

It currently does NOT test:

  * "Oddly" shaped types (e.g. @Signed 21@)
  * 'deviceWithOffsetsWb' with gaps between registers
  * Varying the size of the Wishbone bus
-}
prop_wb :: (?regByteOrder :: ByteOrder, ?byteOrder :: ByteOrder) => Property
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

  -- An error occurred on the bus, determine whether it's expected. It's okay if:
  --
  --  * There is no register mapped to the address
  --  * We try to read from a WO register
  --  * We try to write to an RO register
  modelError ::
    WishboneMasterRequest AddressWidth 4 ->
    Map.Map (BitVector AddressWidth) (BitVector 32) ->
    Maybe String
  modelError instr s = do
    v <- Map.lookup addr s
    if
      | isRead instr && addr == woAddress -> Nothing
      | isWrite instr && addr `elem` roAddresses -> Nothing
      | otherwise -> Just [i|Unexpected error on address #{addr}, value: #{v}|]
   where
    addr = toAddr instr

  modelRead ::
    BitVector AddressWidth ->
    Map.Map (BitVector AddressWidth) (BitVector 32) ->
    BitVector 32 ->
    BitVector 32 ->
    Either String (Map.Map (BitVector AddressWidth) (BitVector 32))
  modelRead addr s v readData
    | v /= 0 && addr == delayedErrorAddress =
        Left [i|delayed error! v: #{v}, readData: #{readData}|]
    | addr `elem` [prioAddress, prioPreferCircuitAddress] =
        Right $ Map.insert addr (head $ packWordCI initU32) s
    | v == readData =
        Right s
    | otherwise =
        Left [i|addr: #{toInteger addr}, stored: #{v}, bus: #{readData}|]

  modelWrite ::
    BitVector AddressWidth ->
    BitVector 4 ->
    BitVector 32 ->
    Map.Map (BitVector AddressWidth) (BitVector 32) ->
    Map.Map (BitVector AddressWidth) (BitVector 32)
  modelWrite addr m newDat s
    | addr == delayedAddress =
        let
          double :: BitVector 32 -> BitVector 32
          double = head . packWordCI . (* (2 :: (Unsigned 32))) . unpackWordOrErrorCI . pure
         in
          Map.adjust (double . update) addr s
    | addr `elem` [prioAddress, prioPreferCircuitAddress] =
        let
          inc :: BitVector 32 -> BitVector 32
          inc = head . packWordCI . (+ (1 :: (Unsigned 32))) . unpackWordOrErrorCI . pure
         in
          Map.adjust (inc . update) addr s
    | otherwise =
        Map.adjust update addr s
   where
    update :: BitVector 32 -> BitVector 32
    update oldDat = head $ maskWriteData @4 @1 0 m newDat (oldDat :> Nil)

  model ::
    WishboneMasterRequest AddressWidth 4 ->
    WishboneS2M 4 ->
    Map.Map (BitVector AddressWidth) (BitVector 32) ->
    Either String (Map.Map (BitVector AddressWidth) (BitVector 32))
  model instr WishboneS2M{err = True} s
    | Just errorMsg <- modelError instr s = Left errorMsg
    | otherwise = Right s
  model _ WishboneS2M{retry = True} s = Right s
  model _ WishboneS2M{acknowledge = False} s = Right s
  model instr WishboneS2M{readData} s =
    case Map.lookup (toAddr instr) s of
      Nothing -> Left [i|Write/read from unmapped address, should have been err=True: #{toAddr instr}|]
      Just v ->
        case instr of
          Read a _ -> modelRead a s v readData
          Write a m newDat -> Right (modelWrite a m newDat s)

  toAddr instr =
    case instr of
      Read a _ -> a
      Write a _ _ -> a

  isRead (Read _ _) = True
  isRead _ = False

  isWrite (Write{}) = True
  isWrite _ = False

  genInputs :: Gen [WishboneMasterRequest AddressWidth 4]
  genInputs = Gen.list (Range.linear 0 300) (genWishboneTransfer genAddr genMask genData)

  genMask :: Gen (BitVector 4)
  genMask = Gen.integral (Range.linear 0 maxBound)

  genData :: Gen (BitVector 32)
  genData = Gen.integral (Range.linear 0 maxBound)

  genAddr :: Gen (BitVector AddressWidth)
  genAddr =
    -- We do plus one to test unmapped addresses
    Gen.integral (Range.constant 0 (1 + P.maximum (Map.keys initState)))

  dut :: Circuit (Wishbone XilinxSystem Standard AddressWidth 4) ()
  dut = unMemmap $ deviceExample @4 @AddressWidth @XilinxSystem NoTrace clk rst

  roAddresses = [roAddress, delayedErrorAddress]

  example = memoryMap.deviceDefs Map.! "example"
  [ _regF
    , _regD
    , _regU
    , regRO
    , regWO
    , regPrio
    , regPrioPreferCircuit
    , regDelayed
    , regDelayedError
    ] = example.registers
  woAddress = fromIntegral $ regWO.value.address `div` 4
  roAddress = fromIntegral $ regRO.value.address `div` 4
  prioAddress = fromIntegral $ regPrio.value.address `div` 4
  prioPreferCircuitAddress = fromIntegral $ regPrioPreferCircuit.value.address `div` 4
  delayedAddress = fromIntegral $ regDelayed.value.address `div` 4
  delayedErrorAddress = fromIntegral $ regDelayedError.value.address `div` 4

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
  let
    -- XXX: It really shouldn't matter what byte order we pick, but setting it to an error
    --      actually produces an error. Investigate?
    ?regByteOrder = LittleEndian
   in
    unSimOnly
      $ getConstBwdAny
      $ deviceExample @4 @AddressWidth @XilinxSystem NoTrace clockGen noReset

{- | Test that the memory map can be generated without errors. Test for sensible
values in the memory map.
-}
case_memoryMap :: Assertion
case_memoryMap = do
  let !_tree = force (ppShow memoryMap)

  let example = memoryMap.deviceDefs Map.! "example"
  example.deviceName.name @?= "example"

  let [ regF
        , regD
        , regU
        , regRO
        , regWO
        , regPrio
        , regPrioPreferCircuit
        , regDelayed
        , regDelayedError
        ] = example.registers

  regF.name.name @?= "f"
  regD.name.name @?= "d"
  regU.name.name @?= "u"
  regRO.name.name @?= "ro"
  regWO.name.name @?= "wo"
  regPrio.name.name @?= "prio"
  regPrioPreferCircuit.name.name @?= "prio_prefer_circuit"
  regDelayed.name.name @?= "delayed"
  regDelayedError.name.name @?= "delayed_error"

  regF.value.address @?= 0
  regD.value.address @?= 4
  regU.value.address @?= 12
  regRO.value.address @?= 16
  regWO.value.address @?= 20
  regPrio.value.address @?= 24
  regPrioPreferCircuit.value.address @?= 28
  regDelayed.value.address @?= 32
  regDelayedError.value.address @?= 36

-- | Test the addressableBytesWb circuit using wishbonePropWithModel
prop_addressableBytesWb :: Property
prop_addressableBytesWb = property $ do
  stalls <- H.forAll $ genStalls
  let
    dut :: Circuit (Wishbone XilinxSystem Standard AddressWidth 4) ()
    dut =
      circuit $ \wb -> do
        mm <- ignoreMM
        [wb0] <- deviceWb (deviceConfig "test") -< (mm, wb)
        reqresp <- CP.withReset rst ReqResp.forceResetSanity <| addressableBytesWb memConf -< wb0
        (reads, writes0) <- ReqResp.partitionEithers <| stallC def stalls -< reqresp
        writes1 <- ReqResp.requests <| ReqResp.dropResponse 0 -< writes0
        _vecUnit <- ram -< (reads, writes1)
        idC -< ()
     where
      ram = withClockResetEnable clk rst enableGen (ReqResp.fromBlockRamWithMask prim)
      prim = blockRamByteAddressable clk ena depth
      memConf = registerConfig "buffer"

  withClockResetEnable clk rst ena
    $ wishbonePropWithModel @XilinxSystem
      defExpectOptions{eoSampleMax = 1_000}
      model
      dut
      genInputs
      mempty
 where
  clk = clockGen
  rst = resetGen
  ena = enableGen
  depth = d8
  maxAddress = snatToNum depth - 1

  model ::
    WishboneMasterRequest AddressWidth 4 ->
    WishboneS2M 4 ->
    Map.Map (BitVector AddressWidth) (BitVector 32) ->
    Either String (Map.Map (BitVector AddressWidth) (BitVector 32))
  model req@(Write address mask newData) response mapState
    | address <= maxAddress && response.acknowledge = Right newMapState
    | address <= maxAddress =
        Left $ "Write to valid address not acknowledged: " <> show (req, response)
    | response.err = Right mapState
    | otherwise = Left $ "Write to invalid address not erroring: " <> show (req, response)
   where
    newMapState = Map.insert address updatedData mapState
    updatedData =
      pack
        $ mux (unpack mask :: Vec 4 Bool) (unpack newData :: Vec 4 (BitVector 8)) (unpack oldData)
    oldData = Map.findWithDefault 0 address mapState
  model req@(Read address mask) response mapState
    | address <= maxAddress && response.acknowledge && dataValid = Right mapState
    | address <= maxAddress && response.acknowledge =
        Left $ "Read data mismatch: " <> show (req, response, expectedVec, actaulVec)
    | address <= maxAddress =
        Left $ "Read from valid address not acknowledged: " <> show (req, response)
    | response.err = Right mapState
    | otherwise = Left $ "Read from invalid address not erroring: " <> show (req, response)
   where
    mapData = Map.findWithDefault 0 address mapState
    maskVec = unpack mask
    expectedVec = unpack mapData :: Vec 4 (BitVector 8)
    actaulVec = unpack response.readData :: Vec 4 (BitVector 8)
    dataValid = all (\(m, exp', act) -> not m || exp' == act) (zip3 maskVec expectedVec actaulVec)

  genInputs :: Gen [WishboneMasterRequest AddressWidth 4]
  genInputs = Gen.list (Range.linear 0 100) (genWishboneTransfer genAddr genMask genData)

  genMask :: Gen (BitVector 4)
  genMask = Gen.integral Range.linearBounded

  genData :: Gen (BitVector 32)
  genData = Gen.integral Range.linearBounded

  genAddr :: Gen (BitVector AddressWidth)
  genAddr = Gen.integral Range.linearBounded

---------------------
-- Copied from Bittide
blockRamByteAddressable ::
  forall dom memDepth wordSize.
  (KnownDomain dom, KnownNat memDepth, KnownNat wordSize, 1 <= memDepth) =>
  Clock dom ->
  Enable dom ->
  SNat memDepth ->
  -- | Read address.
  Signal dom (Index memDepth) ->
  -- | Write operation.
  Signal dom (Maybe (Index memDepth, Bytes wordSize)) ->
  -- | Byte enables that determine which nBytes get replaced.
  Signal dom (BitVector wordSize) ->
  -- | Data at read address (1 cycle delay).
  Signal dom (Bytes wordSize)
blockRamByteAddressable clk ena memDepth readAddr newEntry byteSelect = fmap pack readBytes
 where
  writeBytes = unbundle $ splitWriteInBytes <$> newEntry <*> byteSelect
  readBytes = bundle $ ram <$> writeBytes
  ram = blockRam clk ena (replicate memDepth 0) readAddr

splitWriteInBytes ::
  forall maxIndex wordSize.
  (KnownNat wordSize) =>
  -- | Incoming write operation.
  Maybe (Index maxIndex, Bytes wordSize) ->
  -- | Incoming byte enables.
  BitVector wordSize ->
  -- | Per byte write operation.
  Vec wordSize (Maybe (Index maxIndex, BitVector 8))
splitWriteInBytes (Just (addr, writeData)) byteSelect = mux byteEnable justs nothings
 where
  byteEnable = unpack byteSelect :: Vec wordSize Bool
  justs = fmap (Just . (addr,)) (unpack writeData :: Vec wordSize (BitVector 8))
  nothings = repeat Nothing
splitWriteInBytes Nothing _ = repeat Nothing

---------------------

{- | Test case that replays hardcoded Wishbone transactions and generates a VCD file. This
is not here to actually test anything, but to make it easy to replay a Hedgehog failure
with a VCD file to debug it.
-}
case_replay :: Assertion
case_replay = do
  let
    clk = clockGen @XilinxSystem
    rst = noReset @XilinxSystem
    ena = enableGen @XilinxSystem

    -- Hardcoded wishbone transactions: (request, stall cycles)
    hardcodedTransactions :: [(WishboneMasterRequest AddressWidth 4, Int)]
    hardcodedTransactions =
      [ -- Read from float register with 1 stall cycle
        (Read 0 maxBound, 1)
      , -- Write to float register
        (Write 0 maxBound (pack (3.14 :: Float)), 0)
      ]

    dut :: Circuit (Wishbone XilinxSystem Standard AddressWidth 4) ()
    dut =
      let
        ?regByteOrder = LittleEndian
       in
        unMemmap $ deviceExample @4 @AddressWidth @XilinxSystem Trace clk rst

    driver = driveStandard defExpectOptions{eoResetCycles = 0} hardcodedTransactions

    tracingCircuit ::
      Circuit
        (Wishbone XilinxSystem Standard AddressWidth 4)
        ( Wishbone XilinxSystem Standard AddressWidth 4
        , CSignal XilinxSystem Bool
        )
    tracingCircuit = Circuit $ \(wbM2S, (wbS2M, _)) ->
      let
        tracedClock = T.traceClock "clk" clk
        tracedReset = T.traceReset "rst" rst
        tracedEnable = T.traceEnable "ena" ena
        tracedS2M = T.traceSignal "s2m" wbS2M
        tracedM2S = T.traceSignal "m2s" wbM2S
       in
        ()
          `seq` tracedClock
          `seq` tracedReset
          `seq` tracedEnable
          `seq` tracedS2M
          `seq` tracedM2S
          `seq` (wbS2M, (wbM2S, hasBusActivity <$> bundle (wbM2S, wbS2M)))

    replayDut :: Circuit () (CSignal XilinxSystem Bool)
    replayDut = circuit $ do
      wb0 <- driver
      (wb1, busActivity) <- tracingCircuit -< wb0
      dut -< wb1
      idC -< busActivity

    simResult :: Signal XilinxSystem Bool
    simResult =
      withClockResetEnable clk rst ena
        $ snd
        $ toSignals replayDut ((), ())

    numSimulateCycles :: Int
    numSimulateCycles = 32

  vcdResult <- T.dumpVCD (0, numSimulateCycles) simResult ["s2m", "m2s"]

  case vcdResult of
    Left msg ->
      error $ "VCD dump failed: " <> msg
    Right (vcdContents, json) -> do
      createDirectoryIfMissing True "vcd"
      TIO.writeFile ("vcd/" <> show 'case_replay <> ".vcd") vcdContents
      Shockwaves.writeFileJSON ("vcd/" <> show 'case_replay <> ".json") json

tests :: TestTree
tests =
  testGroup
    "WishboneStandard"
    [ testCase "case_maskWriteData" case_maskWriteData
    , testCase "case_memoryMap" case_memoryMap
    , testCase "case_replay" case_replay
    , testPropertyNamed "prop_addressableBytesWb" "prop_addressableBytesWb" prop_addressableBytesWb
    , testPropertyNamed "prop_wbBigEndian" "prop_wbBigEndian" prop_wbBigEndian
    , testPropertyNamed "prop_wbLittleEndian" "prop_wbLittleEndian" prop_wbLittleEndian
    ]
