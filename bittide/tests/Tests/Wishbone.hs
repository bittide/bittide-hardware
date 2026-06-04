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
import Protocols.Experimental.Hedgehog
import Protocols.Experimental.Simulate (StallAck, stallC)
import Protocols.Experimental.Wishbone
import Protocols.Experimental.Wishbone.Standard.Hedgehog (validatorCircuit, wishbonePropWithModel)
import Protocols.Idle (forceResetSanityGeneric)
import Protocols.MemoryMap
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
import qualified Protocols.Experimental.Hedgehog as PH
import qualified Protocols.Experimental.Wishbone.Standard.Hedgehog as Wb

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

maxInFlight :: SNat 16
maxInFlight = SNat

uartInterfaceWbTest :: Property
uartInterfaceWbTest = property $ do
  stallsBefore <- forAll $ genStalls'
  stallsAfter <- forAll $ genStalls'

  let
    deviceName = "Uart"
    defs = (((getMMAny dutMm).deviceDefs) Map.! deviceName)
    dataLoc = L.find (\loc -> loc.name.name == "data") defs.registers
    dataAddr = fromIntegral (fromJust dataLoc).value.address `div` natToNum @AddressWidth

    dutMm :: Circuit (ToConstBwd Mm, Wishbone XilinxSystem Standard AddressWidth 4) ()
    dutMm =
      withLittleEndian $ withClockResetEnable clk rst ena $ circuit $ \(mm, wb) -> do
        -- The receive buffer is sized to hold the worst-case in-flight count produced by
        -- 'genWriteReadOps' (currently 'maxInFlight'). 'unsafeToDf' at the rxFifo input
        -- silently drops bytes if the FIFO is full, so this must not be smaller than the
        -- peak in-flight count.
        (uartOut, _uartStatus) <- uartInterfaceWb d2 maxInFlight uartBytes -< ((mm, wb), uartIn)
        uartIn <-
          stallC simConfig stallsAfter
            <| Df.fifo d16
            <| stallC simConfig stallsBefore
            -< uartOut
        idC -< ()

    -- The model tracks the FIFO of in-flight bytes (written but not yet
    -- read). Writes enqueue; reads dequeue and check.
    model ::
      Wb.WishboneMasterRequest 4 4 -> WishboneS2M 4 -> [BitVector 32] -> Either String [BitVector 32]
    model (Wb.Write _ _ b) resp s
      | resp.acknowledge = Right (s <> [b])
      | otherwise = Left [i|Expected acknowledge for valid write, but got: #{resp}|]
    model (Wb.Read _ _) resp (expected : rest)
      | resp.acknowledge && resp.readData == expected = Right rest
      | otherwise = Left [i|Expected: #{expected}, but got: #{resp}|]
    model (Wb.Read _ _) resp [] = Left [i|Expected more read responses, but got: #{resp} |]

  -- Generate interleaving writes and reads in a single 'Gen' so that Hedgehog's shrinker
  -- preserves the invariant that reads never outrun writes in flight.
  allOps <- forAll $ genWriteReadOps dataAddr

  withClockResetEnable clk rst ena
    $ wishbonePropWithModel eOpts model (unMemmap dutMm) (pure allOps) []
 where
  -- The randomized schedule can take well over the default 1000 cycles to
  -- complete (longer schedules + random stalls). Bump the sample limit so
  -- the test isn't truncated mid-transaction.
  eOpts = defExpectOptions{eoSampleMax = 100_000}
  simConfig = def

  clk = clockGen
  rst = noReset
  ena = enableGen

{- | Generate a list of Wishbone 'Write' and 'Read' requests targeting the given address.
The interleaving is chosen so that at every prefix the number of reads does not exceed the
number of writes already issued (otherwise a read would stall the bus forever waiting for
a byte that never arrives).
-}
genWriteReadOps ::
  BitVector 4 -> Gen [Wb.WishboneMasterRequest 4 4]
genWriteReadOps dataAddr = do
  -- The list length drives test cost. Keep it moderate; the inner generator below balances
  -- writes vs reads.
  n <- Gen.integral (Range.linear (1 :: Int) (snatToNum maxInFlight))
  ops <- go (0 :: Int) n
  -- Drain any in-flight writes with reads so the model has matching reads.
  drain <- genDrain (countInFlight ops)
  pure (ops <> drain)
 where
  -- @go inFlight remaining@: writes already issued but not yet read back, and how many
  -- more ops to emit.
  go _ 0 = pure []
  go inFlight remaining
    | inFlight == 0 = do
        b <- genDefinedBitVector @8
        (Wb.Write dataAddr 1 (resize b) :) <$> go 1 (remaining - 1)
    | otherwise = do
        op <- Gen.element [OpWrite, OpRead]
        case op of
          OpWrite -> do
            b <- genDefinedBitVector @8
            (Wb.Write dataAddr 1 (resize b) :) <$> go (inFlight + 1) (remaining - 1)
          OpRead -> (Wb.Read dataAddr 1 :) <$> go (inFlight - 1) (remaining - 1)

  -- Append between 0 and inFlight read ops to drain any pending writes.
  genDrain inFlight = do
    k <- Gen.integral (Range.linear 0 inFlight)
    pure (L.replicate k (Wb.Read dataAddr 1))

  -- Count writes minus reads in the sequence (the in-flight count after it).
  countInFlight = L.foldl' step (0 :: Int)
   where
    step n (Wb.Write{}) = n + 1
    step n (Wb.Read{}) = n - 1

data Op = OpWrite | OpRead deriving (Show)

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
          , readData = fromJustX . maybeUnpack $ resize indexBV
          }
   where
    commAttempt = m.busCycle && m.strobe
    maybeIndex = elemIndex (fromJustX (maybeUnpack indexBV)) config
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
    maybeIndex = elemIndex (fromJustX (maybeUnpack indexBV)) config
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
            ( WB.ReadSuccess
                $ mux (fromJustX (maybeUnpack sel)) (map Just $ fromJustX (maybeUnpack reg0)) (repeat Nothing)
            , reg0
            )
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
