-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

module Tests.Wishbone (tests, simpleSlave, simpleSlave') where

import Clash.Prelude hiding (sample)

import Clash.Hedgehog.Sized.Vector
import Clash.Sized.Vector (unsafeFromList)
import Data.Bifunctor
import Data.Constraint (Dict (Dict))
import Data.Constraint.Nat.Lemmas (cancelMulDiv, divWithRemainder)
import Data.String
import Hedgehog
import Hedgehog.Range as Range
import Protocols
import Protocols.Hedgehog
import Protocols.MemoryMap (ignoreMM)
import Protocols.Idle (forceResetSanityGeneric)
import Protocols.Wishbone
import Protocols.Wishbone.Standard.Hedgehog (validatorCircuit)
import Test.Tasty
import Test.Tasty.Hedgehog

import Bittide.SharedTypes
import Bittide.Wishbone as WB
import Tests.Shared

import qualified Data.List as L
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen

tests :: TestTree
tests =
  testGroup
    "Tests.Wishbone"
    [ testPropertyNamed "Reading readData from slaves." "readingSlaves" readingSlaves
    , testPropertyNamed "Writing and reading from slaves." "writingSlaves" writingSlaves
    , testPropertyNamed
        "Send and receive bytes via uartInterfaceWb"
        "uartInterfaceWbCircuitTest"
        uartInterfaceWbCircuitTest
    , testPropertyNamed
        "Test the Df based wishbone master"
        "prop_dfWishboneMaster"
        prop_dfWishboneMaster
    ]

data UartMachineState = ReadStatus | ReadByte | WriteByte | OutputByte (BitVector 8)
  deriving (Generic, NFDataX, Show)

{- | A `Circuit` that transforms incoming `Df` transactions into `Wishbone` transactions
that are compatible with `Bittide.Wishbone.wbToDf`.
-}

-- .It implements the following state machine:
-- 1. Check if there is incoming data available in the `wbToDf` receive fifo, if so, skip to 4.
-- 2. Check if there is space in the `wbToDf` transmit fifo. If so, skip to 6.
-- 3. Go to 1.
-- 4. Read in the incoming byte and send it to the outgoing Df interface.
-- 5. Go to 1
-- 6. If there is no data available at the incoming Df interface, go to 1.
-- 7. Write data from incoming Df interface to `wbToDf`.
uartMachine ::
  forall dom addrW.
  ( HiddenClockResetEnable dom
  , KnownNat addrW
  ) =>
  Circuit
    (Df dom (BitVector 8))
    (Wishbone dom 'Standard addrW Byte, Df dom (BitVector 8))
uartMachine = Circuit (second unbundle . mealyB go ReadStatus . second bundle)
 where
  go ReadStatus (_, ~(WishboneS2M{..}, _)) = (nextState, (Ack False, (wbOut, Nothing)))
   where
    (rxEmpty, txFull) = unpack (resize readData)
    nextState = case (acknowledge, err, (rxEmpty, txFull)) of
      (True, False, (False, _)) -> ReadByte
      (True, False, (True, False)) -> WriteByte
      _ -> ReadStatus
    wbOut = (emptyWishboneM2S @30){addr = 1, busCycle = True, strobe = True}
  go ReadByte (_, ~(WishboneS2M{..}, _)) = (nextState, (Ack False, (wbOut, Nothing)))
   where
    nextState = case (acknowledge, err) of
      (True, False) -> OutputByte (resize readData)
      _ -> ReadByte
    wbOut = (emptyWishboneM2S @30){addr = 0, busCycle = True, strobe = True}
  go (OutputByte byte) (_, ~(_, Ack dfAck)) =
    (nextState, (Ack False, (emptyWishboneM2S, Just byte)))
   where
    nextState = if dfAck then ReadStatus else (OutputByte byte)
  go (WriteByte) (Just dfData, ~(WishboneS2M{..}, _)) = (nextState, (Ack dfAck, (wbOut, Nothing)))
   where
    (nextState, dfAck) = if acknowledge && not err then (ReadStatus, True) else (WriteByte, False)
    wbOut =
      (emptyWishboneM2S @30 @())
        { addr = 0
        , busCycle = True
        , strobe = True
        , writeEnable = True
        , busSelect = 1
        , writeData = resize dfData
        }
  go WriteByte (Nothing, _) = (ReadStatus, (Ack False, (emptyWishboneM2S, Nothing)))

-- | Check if we can combine `uartInterfaceWb` in loopback mode and `uartMachine` to create `id`.
uartInterfaceWbCircuitTest :: Property
uartInterfaceWbCircuitTest = do
  let
    dataGen = Gen.list (Range.linear 0 32) $ genDefinedBitVector @8
    dut :: (HiddenClockResetEnable System) => Circuit (Df System Byte) (Df System Byte)
    dut = circuit $ \dfIn -> do
      (wb, dfOut) <- uartMachine -< dfIn
      mm <- ignoreMM
      (uartTx, _status) <- uartInterfaceWb @System @32 d2 d2 uartSim -< (mm, (wb, uartTx))
      idC -< dfOut
    expectOptions =
      defExpectOptions
        { eoResetCycles = 15
        , eoDriveEarly = True
        }
  idWithModel expectOptions dataGen id (wcre dut)

{- | Generates a 'MemoryMap' for 'singleMasterInterconnect' for a specific number
of slaves.
-}
genConfig ::
  forall nSlaves.
  (1 <= nSlaves) =>
  SNat nSlaves ->
  Gen (MemoryMap nSlaves)
genConfig nSlaves@SNat =
  unsafeFromList . L.take (snatToNum nSlaves) <$> Gen.shuffle [0 ..]

{- | Creates a memory map with 'simpleSlave' devices and a list of read addresses and checks
if the correct 'simpleSlave' responds to the read operation. Reading outside of a 'simpleSlave' its
range should return an error.
-}
readingSlaves :: Property
readingSlaves = property $ do
  devices <- forAll $ Gen.enum 2 16
  case TN.someNatVal (devices - 1) of
    SomeNat (succSNat . snatProxy -> devices0@SNat) ->
      case compareSNat (clogBaseSNat d2 devices0) d30 of
        SNatLE -> runTest devices0
        _ -> errorX "readingSlaves: number of devices can't be represented with 30 bits."
 where
  runTest devices = do
    config <- forAll $ genConfig @_ devices
    nrOfReads <- forAll $ Gen.enum 1 32
    let nrOfReadsRange = Range.singleton nrOfReads
    readAddresses <- forAll $ Gen.list nrOfReadsRange (genDefinedBitVector @30)
    ranges <- forAll $ genVec genDefinedBitVector
    let
      topEntityInput = (wbRead <$> readAddresses) <> [emptyWishboneM2S]
      simOut = simulateN (L.length topEntityInput) (topEntity config ranges) topEntityInput
      realTransactions = wbToTransaction topEntityInput simOut
      expectedOutput = fmap (getExpected config ranges) topEntityInput
      expectedTransactions = wbToTransaction topEntityInput expectedOutput
    footnote . fromString $ "expectedTransactions: " <> showX expectedTransactions
    footnote . fromString $ "realTransactions: " <> showX realTransactions
    footnote . fromString $ "simOut: " <> showX simOut
    footnote . fromString $ "simIn: " <> showX topEntityInput
    footnote . fromString $ "reads: " <> show readAddresses
    footnote . fromString $ "ranges: " <> show ranges
    footnote . fromString $ "config: " <> show config
    realTransactions === expectedTransactions

  topEntity config ranges masterIn = toMaster
   where
    slaves =
      withClockResetEnable @System clockGen resetGen enableGen
        $ simpleSlave
        <$> ranges
        <*> config
        <*> unbundle toSlaves
    (toMaster, toSlaves) =
      withClockResetEnable
        clockGen
        resetGen
        enableGen
        singleMasterInterconnect'
        config
        masterIn
        $ bundle slaves

  getExpected ::
    forall nSlaves.
    ( KnownNat nSlaves
    , 1 <= nSlaves
    , BitSize (Unsigned (CLog 2 nSlaves)) <= 30
    , BitSize (Unsigned (CLog 2 nSlaves)) <= 30
    ) =>
    Vec nSlaves (Unsigned (CLog 2 nSlaves)) ->
    Vec nSlaves (BitVector (30 - BitSize (Unsigned (CLog 2 nSlaves)))) ->
    WishboneM2S 30 (Regs (Unsigned (CLog 2 nSlaves)) 8) (Unsigned (CLog 2 nSlaves)) ->
    WishboneS2M (Unsigned (CLog 2 nSlaves))
  getExpected config ranges WishboneM2S{..}
    | not commAttempt = emptyWishboneS2M
    | Nothing <- maybeIndex = emptyWishboneS2M{err = True}
    | Just index <- maybeIndex, not (inRange index) = emptyWishboneS2M{err = True}
    | otherwise =
        (emptyWishboneS2M @(Unsigned (CLog 2 nSlaves)))
          { acknowledge = True
          , readData = unpack indexBV
          }
   where
    commAttempt = busCycle && strobe
    maybeIndex = elemIndex (unpack indexBV) config
    (indexBV :: BitVector (CLog 2 nSlaves), restAddr) = split addr
    inRange index = restAddr <= (ranges !! index)

{- | Creates a memory map with 'simpleSlave' devices and a list of write addresses and checks
that if we 'simpleSlave' responds to the read operation. Reading outside of a 'simpleSlave' its
range should return an err.
-}
writingSlaves :: Property
writingSlaves = property $ do
  devices <- forAll $ Gen.enum 1 16
  case TN.someNatVal (devices - 1) of
    SomeNat (succSNat . snatProxy -> devices0) ->
      case compareSNat (clogBaseSNat d2 devices0) d30 of
        SNatLE -> runTest devices0
        _ -> errorX "readingSlaves: number of devices can't be represented with 30 bits."
 where
  runTest devices = do
    config <- forAll $ genConfig @_ devices
    nrOfWrites <- forAll $ Gen.enum 1 32
    let nrOfWritesRange = Range.singleton nrOfWrites
    writeAddresses <- forAll $ Gen.list nrOfWritesRange genDefinedBitVector
    ranges <- forAll $ genVec genDefinedBitVector
    let
      topEntityInput = L.concatMap wbWriteThenRead writeAddresses <> [emptyWishboneM2S]
      simLength = L.length topEntityInput
      simOut = simulateN simLength (topEntity config ranges) topEntityInput
      realTransactions = wbToTransaction topEntityInput simOut
      expectedOutput = fmap (getExpected config ranges) topEntityInput
      expectedTransactions = wbToTransaction topEntityInput expectedOutput
    footnote . fromString $ "expectedTransactions: " <> showX expectedTransactions
    footnote . fromString $ "realTransactions: " <> showX realTransactions
    footnote . fromString $ "simOut: " <> showX simOut
    footnote . fromString $ "simIn: " <> showX topEntityInput
    footnote . fromString $ "writes: " <> show writeAddresses
    footnote . fromString $ "ranges: " <> show ranges
    footnote . fromString $ "config: " <> show config
    realTransactions === expectedTransactions

  topEntity config ranges masterIn = toMaster
   where
    slaves =
      withClockResetEnable @System clockGen resetGen enableGen
        $ simpleSlave
        <$> ranges
        <*> ranges
        <*> unbundle toSlaves
    (toMaster, toSlaves) =
      withClockResetEnable
        clockGen
        resetGen
        enableGen
        singleMasterInterconnect'
        config
        masterIn
        $ bundle slaves

  wbWriteThenRead a = [wbWrite a (resize a), wbRead a]

  getExpected ::
    forall nSlaves.
    ( KnownNat nSlaves
    , 1 <= nSlaves
    , BitSize (Unsigned (CLog 2 nSlaves)) <= 30
    , BitSize (Unsigned (CLog 2 nSlaves)) <= 30
    ) =>
    Vec nSlaves (Unsigned (CLog 2 nSlaves)) ->
    Vec nSlaves (BitVector (30 - BitSize (Unsigned (CLog 2 nSlaves)))) ->
    WishboneM2S
      30
      (DivRU (30 - BitSize (Unsigned (CLog 2 nSlaves))) 8)
      (BitVector (30 - BitSize (Unsigned (CLog 2 nSlaves)))) ->
    WishboneS2M (BitVector (30 - BitSize (Unsigned (CLog 2 nSlaves))))
  getExpected config ranges WishboneM2S{..}
    | not commAttempt = emptyWishboneS2M
    | Nothing <- maybeIndex = emptyWishboneS2M{err = True}
    | Just index <- maybeIndex, not (inRange index) = emptyWishboneS2M{err = True}
    | writeEnable = emptyWishboneS2M{acknowledge = True}
    | otherwise =
        (emptyWishboneS2M @(Unsigned (CLog 2 nSlaves)))
          { acknowledge = True
          , readData = restAddr
          }
   where
    commAttempt = busCycle && strobe
    maybeIndex = elemIndex (unpack indexBV) config
    (indexBV :: BitVector (CLog 2 nSlaves), restAddr) = split addr
    inRange index = restAddr <= (ranges !! index)

-- | transforms an address to a 'WishboneM2S' read operation.
wbRead ::
  forall addressWidth a.
  (KnownNat addressWidth, NFDataX a, KnownNat (BitSize a)) =>
  BitVector addressWidth ->
  WishboneM2S addressWidth (Regs a 8) a
wbRead address = case cancelMulDiv @(Regs a 8) @8 of
  Dict ->
    (emptyWishboneM2S @addressWidth)
      { addr = address
      , strobe = True
      , busCycle = True
      , busSelect = maxBound
      }

{- | transforms an address to a 'WishboneM2S' write operation that writes the given address
to the given address.
-}
wbWrite ::
  forall addressWidth a.
  (KnownNat addressWidth, NFDataX a, KnownNat (BitSize a)) =>
  BitVector addressWidth ->
  a ->
  WishboneM2S addressWidth (Regs a 8) a
wbWrite address a =
  (emptyWishboneM2S @addressWidth @a)
    { addr = address
    , strobe = True
    , busCycle = True
    , writeData = a
    , writeEnable = True
    , busSelect = maxBound
    }

prop_dfWishboneMaster :: Property
prop_dfWishboneMaster =
  idWithModel
    @(Df System (WishboneRequest 32 4))
    @(Df System (WishboneResponse 4))
    defExpectOptions
    gen
    (model initReg)
    impl
 where
  gen :: Gen [WishboneRequest 32 4]
  gen = Gen.list (Range.linear 0 100) genWishboneTransfer
  initReg = unpack 0xDEADBEEF
  range = 0x5555555

  model :: Vec 4 Byte -> [WishboneRequest 32 4] -> [WishboneResponse 4]
  model _ [] = []
  model reg0 (req : reqs) = resp : model reg1 reqs
   where
    (resp, reg1) = case req of
      WB.ReadRequest addr sel
        | addr <= range ->
            (WB.ReadSuccess $ mux (unpack sel) (map Just reg0) (repeat Nothing), reg0)
      WB.ReadRequest _ _ -> (WB.ReadError, reg0)
      WB.WriteRequest addr _ dat
        | addr <= range ->
            (WB.WriteSuccess, dat)
      WB.WriteRequest{} -> (WB.WriteError, reg0)

  impl = withClockResetEnable clockGen resetGen enableGen $ circuit $ \reqs -> do
    (wb, resps) <- dfWishboneMaster -< reqs
    simpleSlave' @System range initReg -< wb
    idC -< resps

simpleSlave' ::
  forall dom aw a.
  (HiddenClockResetEnable dom, KnownNat aw, NFDataX a, KnownNat (BitSize a), ShowX a) =>
  BitVector aw ->
  a ->
  Circuit (Wishbone dom 'Standard aw a) ()
simpleSlave' range readDataInit =
  forceResetSanityGeneric
    |> Circuit (\(wbIn, ()) -> (mealy go readDataInit wbIn, ()))
 where
  go readData1 WishboneM2S{..} =
    (readData2, (emptyWishboneS2M @a){readData, acknowledge, err})
   where
    masterActive = busCycle && strobe
    addrInRange = addr <= range
    acknowledge = masterActive && addrInRange
    err = masterActive && not addrInRange
    writeOp = acknowledge && writeEnable
    readData2
      | writeOp = writeData
      | otherwise = readData1
    readData
      | writeOp = writeData
      | otherwise = readData1

{- | Simple wishbone slave that responds to addresses [0..range], it responds by returning
a stored value (initialized by readData0), which can be overwritten by the wishbone bus.
any read/write attempt to an address outside of the supplied range sets the err signal.
-}
simpleSlave ::
  forall dom aw a.
  (HiddenClockResetEnable dom, KnownNat aw, BitPack a, NFDataX a, ShowX a) =>
  BitVector aw ->
  a ->
  Signal dom (WishboneM2S aw (Regs a 8) a) ->
  Signal dom (WishboneS2M a)
simpleSlave range readDataInit wbIn =
  case divWithRemainder @(Regs a 8) @8 @7 of
    Dict -> fst $ toSignals slaveCircuit (wbIn, ())
 where
  slaveCircuit = validatorCircuit |> simpleSlave' range readDataInit

genWishboneTransfer ::
  (KnownNat addrW, KnownNat nBytes) =>
  Gen (WB.WishboneRequest addrW nBytes)
genWishboneTransfer =
  Gen.choice
    [ WB.ReadRequest <$> genDefinedBitVector <*> genDefinedBitVector
    , WB.WriteRequest
        <$> genDefinedBitVector
        <*> genDefinedBitVector
        <*> genVec genDefinedBitVector
    ]
