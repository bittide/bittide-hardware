-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Tests.Wishbone (tests, simpleSlave, simpleSlave', wbRead) where

import Clash.Prelude hiding (sample)

import Clash.Explicit.Prelude (noReset)
import Clash.Hedgehog.Sized.BitVector
import Clash.Hedgehog.Sized.Vector
import Clash.Sized.Vector (unsafeFromList)
import Data.Maybe (fromJust)
import Data.String
import Data.String.Interpolate (i)
import Hedgehog
import Hedgehog.Range as Range
import Protocols
import Protocols.Hedgehog
import Protocols.Idle (forceResetSanityGeneric)
import Protocols.MemoryMap
import Protocols.Wishbone
import Protocols.Wishbone.Standard.Hedgehog (validatorCircuit, wishbonePropWithModel)
import Test.Tasty
import Test.Tasty.Hedgehog

import Bittide.SharedTypes
import Bittide.Wishbone as WB

import Tests.Shared
import Tests.Wishbone.Utils (genWishboneRequest)

import qualified Data.List as L
import qualified Data.Map as Map
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen
import qualified Protocols.Df as Df
import qualified Protocols.Hedgehog as PH
import qualified Protocols.Wishbone.Standard.Hedgehog as Wb

tests :: TestTree
tests =
  testGroup
    "Tests.Wishbone"
    [ testPropertyNamed "Reading readData from slaves." "readingSlaves" readingSlaves
    , testPropertyNamed "Writing and reading from slaves." "writingSlaves" writingSlaves
    , testPropertyNamed
        "Loopback uartInterfaceWb through fifo with random stalls"
        "uartInterfaceWbTest"
        uartInterfaceWbTest
    , testPropertyNamed
        "Test the Df based wishbone master"
        "prop_dfWishboneMaster"
        prop_dfWishboneMaster
    ]

type AddressWidth = 4

smallInt :: Gen Int
smallInt = Gen.integral (Range.linear 0 10)

genStalls' :: (KnownNat n) => Gen (Vec n ((StallAck, [Int])))
genStalls' = do
  numStalls <- smallInt
  genVec (PH.genStalls smallInt numStalls PH.Stall)

uartInterfaceWbTest :: Property
uartInterfaceWbTest = property $ do
  stallsBefore <- forAll $ genStalls'
  stallsAfter <- forAll $ genStalls'
  let
    deviceName = "Uart"
    defs = (((getMMAny dutMm).deviceDefs) Map.! deviceName)
    dataLoc = L.find (\loc -> loc.name.name == "data") defs.registers
    dataAddr = fromIntegral (fromJust dataLoc).value.address `div` natToNum @AddressWidth

    bytes = [0 .. 7]
    writeOps = Wb.Write dataAddr 1 <$> bytes
    readOps = L.replicate 8 $ Wb.Read dataAddr 1
    allOps = writeOps <> readOps

    dutMm :: Circuit (ToConstBwd Mm, Wishbone XilinxSystem Standard AddressWidth 4) ()
    dutMm =
      withLittleEndian $ withClockResetEnable clk rst ena $ circuit $ \(mm, wb) -> do
        -- The receive buffer needs to be large enough, otherwise bytes will be dropped.
        (uartOut, _uartStatus) <- uartInterfaceWb d2 d16 uartBytes -< ((mm, wb), uartIn)
        uartIn <-
          stallC simConfig stallsAfter
            <| Df.fifo d16
            <| stallC simConfig stallsBefore
            -< uartOut
        idC -< ()

    model (Wb.Write _ _ _) resp s
      | resp.acknowledge = Right s
      | otherwise = Left [i|Expected acknowledge for valid write, but got: #{resp}|]
    model (Wb.Read _ _) resp (expected : rest)
      | resp.acknowledge && resp.readData == expected = Right rest
      | otherwise = Left [i|Expected: #{expected}, but got: #{resp}|]
    model (Wb.Read _ _) resp [] = Left [i|Expected more read responses, but got: #{resp} |]

  withClockResetEnable clk rst ena
    $ wishbonePropWithModel eOpts model (unMemmap dutMm) (pure allOps) bytes
 where
  eOpts = defExpectOptions
  simConfig = def

  clk = clockGen
  rst = noReset
  ena = enableGen

{- | Generates a 'MemoryMap' for 'singleMasterInterconnect' for a specific number
of slaves.
-}
genConfig ::
  forall nSlaves pfxWidth.
  (1 <= nSlaves, KnownNat pfxWidth) =>
  SNat nSlaves ->
  Gen (WB.MemoryMap nSlaves pfxWidth)
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
  runTest ::
    forall nSlaves.
    ( KnownNat nSlaves
    , 1 <= nSlaves
    , CLog 2 nSlaves <= 30
    ) =>
    SNat nSlaves ->
    PropertyT IO ()
  runTest devices = do
    config <- forAll $ genConfig devices
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
        <*> fmap (pack . resize) config
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
    , CLog 2 nSlaves <= 30
    ) =>
    Vec nSlaves (Unsigned (CLog 2 nSlaves)) ->
    Vec nSlaves (BitVector (30 - BitSize (Unsigned (CLog 2 nSlaves)))) ->
    WishboneM2S 30 4 ->
    WishboneS2M 4
  getExpected config ranges m@WishboneM2S{}
    | not commAttempt = emptyWishboneS2M
    | Nothing <- maybeIndex = emptyWishboneS2M{err = True}
    | Just index <- maybeIndex, not (inRange index) = emptyWishboneS2M{err = True}
    | otherwise =
        (emptyWishboneS2M @4)
          { acknowledge = True
          , readData = unpack $ resize indexBV
          }
   where
    commAttempt = m.busCycle && m.strobe
    maybeIndex = elemIndex (unpack indexBV) config
    (indexBV :: BitVector (CLog 2 nSlaves), restAddr) = split m.addr
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
  runTest ::
    forall nSlaves.
    ( KnownNat nSlaves
    , 1 <= nSlaves
    , CLog 2 nSlaves <= 30
    ) =>
    SNat nSlaves ->
    PropertyT IO ()
  runTest devices = do
    config <- forAll $ genConfig devices
    nrOfWrites <- forAll $ Gen.enum 1 32
    let nrOfWritesRange = Range.singleton nrOfWrites
    writeAddresses <- forAll $ Gen.list nrOfWritesRange (genDefinedBitVector @30)
    ranges <- forAll $ genVec genDefinedBitVector
    let
      topEntityInput = L.concatMap (wbWriteThenRead devices) writeAddresses <> [emptyWishboneM2S]
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
        <*> fmap resize ranges
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

  wbWriteThenRead (SNat :: SNat devices) a = [wbWrite a (resize (restAddr)), wbRead a]
   where
    (_indexBV :: BitVector (CLog 2 devices), restAddr :: BitVector (30 - CLog 2 devices)) = split a
  getExpected ::
    forall nSlaves.
    ( KnownNat nSlaves
    , 1 <= nSlaves
    , CLog 2 nSlaves <= 30
    ) =>
    Vec nSlaves (Unsigned (CLog 2 nSlaves)) ->
    Vec nSlaves (BitVector (30 - BitSize (Unsigned (CLog 2 nSlaves)))) ->
    WishboneM2S 30 4 ->
    WishboneS2M 4
  getExpected config ranges m@WishboneM2S{}
    | not commAttempt = emptyWishboneS2M
    | Nothing <- maybeIndex = emptyWishboneS2M{err = True}
    | Just index <- maybeIndex, not (inRange index) = emptyWishboneS2M{err = True}
    | m.writeEnable = emptyWishboneS2M{acknowledge = True}
    | otherwise =
        (emptyWishboneS2M @4)
          { acknowledge = True
          , readData = resize restAddr
          }
   where
    commAttempt = m.busCycle && m.strobe
    maybeIndex = elemIndex (unpack indexBV) config
    (indexBV :: BitVector (CLog 2 nSlaves), restAddr) = split m.addr
    inRange index = restAddr <= (ranges !! index)

-- | transforms an address to a 'WishboneM2S' read operation.
wbRead ::
  forall addressWidth nBytes.
  (KnownNat addressWidth, KnownNat nBytes) =>
  BitVector addressWidth ->
  WishboneM2S addressWidth nBytes
wbRead address =
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
  forall addressWidth nBytes.
  (KnownNat addressWidth, KnownNat nBytes) =>
  BitVector addressWidth ->
  BitVector (nBytes * 8) ->
  WishboneM2S addressWidth nBytes
wbWrite address a =
  (emptyWishboneM2S @addressWidth @nBytes)
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
  gen = Gen.list (Range.linear 0 100) genWishboneRequest
  initReg = 0xDEADBEEF
  range = 0x5555555

  model :: Bytes 4 -> [WishboneRequest 32 4] -> [WishboneResponse 4]
  model _ [] = []
  model reg0 (req : reqs) = resp : model reg1 reqs
   where
    (resp, reg1) = case req of
      WB.ReadRequest addr sel
        | addr <= range ->
            (WB.ReadSuccess $ mux (unpack sel) (map Just $ unpack reg0) (repeat Nothing), reg0)
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
  forall dom aw nBytes.
  (HiddenClockResetEnable dom, KnownNat aw, KnownNat nBytes) =>
  BitVector aw ->
  BitVector (nBytes * 8) ->
  Circuit (Wishbone dom 'Standard aw nBytes) ()
simpleSlave' range readDataInit =
  forceResetSanityGeneric
    |> Circuit (\(wbIn, ()) -> (mealy go readDataInit wbIn, ()))
 where
  go readData1 m@WishboneM2S{} =
    (readData2, (emptyWishboneS2M @nBytes){readData, acknowledge, err})
   where
    masterActive = m.busCycle && m.strobe
    addrInRange = m.addr <= range
    acknowledge = masterActive && addrInRange
    err = masterActive && not addrInRange
    writeOp = acknowledge && m.writeEnable
    readData2
      | writeOp = m.writeData
      | otherwise = readData1
    readData
      | writeOp = m.writeData
      | otherwise = readData1

{- | Simple wishbone slave that responds to addresses [0..range], it responds by returning
a stored value (initialized by readData0), which can be overwritten by the wishbone bus.
any read/write attempt to an address outside of the supplied range sets the err signal.
-}
simpleSlave ::
  forall dom aw nBytes.
  (HiddenClockResetEnable dom, KnownNat aw, KnownNat nBytes) =>
  BitVector aw ->
  BitVector (nBytes * 8) ->
  Signal dom (WishboneM2S aw nBytes) ->
  Signal dom (WishboneS2M nBytes)
simpleSlave range readDataInit wbIn = fst $ toSignals slaveCircuit (wbIn, ())
 where
  slaveCircuit = validatorCircuit |> simpleSlave' range readDataInit
