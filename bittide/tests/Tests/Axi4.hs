-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Tests.Axi4 where

import Clash.Explicit.Prelude (noReset)
import Clash.Prelude

import Clash.Hedgehog.Sized.Unsigned

import Bittide.Axi4
import Bittide.Extra.Maybe
import Bittide.SharedTypes
import Bittide.DoubleBufferedRam
import Data.Either
import Data.Maybe
import Data.Proxy
import Hedgehog

import Protocols
import Protocols.Axi4.Stream
import Protocols.Hedgehog
import Protocols.Internal
import Protocols.Wishbone as WB
import Test.Tasty
import Test.Tasty.Hedgehog
import Tests.Axi4.Generators
import Tests.Axi4.Properties
import Tests.Axi4.Types
import Tests.Shared

import qualified Data.List as L
import qualified Data.String.Interpolate as SI
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Protocols.Axi4.Common as Axi
import qualified Protocols.Axi4.ReadAddress as Axi
import qualified Protocols.Axi4.ReadData as Axi
import qualified Protocols.Axi4.WriteAddress as Axi
import qualified Protocols.Axi4.WriteData as Axi
import qualified Protocols.Axi4.WriteResponse as Axi
import qualified Protocols.Wishbone.Standard.Hedgehog as WB
import qualified Protocols.MemoryMap as MM
import qualified Prelude as P

tests :: TestTree
tests =
  testGroup
    "Tests.Axi4"
    [ testPropertyNamed
        "Read Axi4 Stream packets via Wishbone"
        "prop_wbAxisRxBufferReadStreams"
        prop_wbAxisRxBufferReadStreams
    , testPropertyNamed
        "Various operation on Axi4StreamM2S: splitAxi4Stream combineAxi4Stream packAxi4Stream"
        "prop_axiOperations"
        prop_axiOperations
    , testPropertyNamed
        "Packet conversion utilies"
        "prop_packetConversions"
        prop_packetConversions
    , testPropertyNamed
        "Axi4StreamPacketFifo"
        "prop_axi4StreamPacketFifo"
        prop_axi4StreamPacketFifo
    , testPropertyNamed
        "Axi4StreamPacketFifo produces uninterrupted packets"
        "prop_axi4StreamPacketFifo_Uninterrupted"
        prop_axi4StreamPacketFifo_Uninterrupted
    , testPropertyNamed
        "axiStreamToByteStream"
        "prop_axiStreamToByteStream"
        prop_axiStreamToByteStream
    , testPropertyNamed
        "axiStreamFromByteStream "
        "prop_axiStreamFromByteStream"
        prop_axiStreamFromByteStream
    , testPropertyNamed "packAxi4Stream" "prop_packAxi4Stream" prop_packAxi4Stream
    , testPropertyNamed "prop_axiPacking" "prop_axiPacking" prop_axiPacking
    , testPropertyNamed "prop_wishboneS2Axi4" "prop_wishboneS2Axi4" prop_wishboneS2Axi4
    , testPropertyNamed
        "prop_wishboneS2Axi4_NoStall"
        "prop_wishboneS2Axi4_NoStall"
        prop_wishboneS2Axi4_NoStall
    ]

-- This test only checks that the data and position bytes are not changed by the component,
-- _tdest, _tid and _tuser are not checked.
prop_axiStreamToByteStream :: Property
prop_axiStreamToByteStream =
  propWithModel defExpectOptions gen model impl prop
 where
  impl = wcre @System axiStreamToByteStream

  model = L.concatMap (catMaybes . packetToAxiStream d1) . axiStreamToPackets

  packetGen =
    catMaybes
      <$> genRandomAxiPacket
        d4
        d0
        d0
        [NullByte, DataByte, PositionByte]
        (Range.linear 0 16)
        (pure ())
  gen = L.concat <$> Gen.list (Range.linear 0 3) packetGen

  prop expected sampled = do
    let bytetypes = fmap getPacketByteTypes $ rights $ separatePackets sampled
    footnote $ "expected: " <> show expected
    footnote $ "sampled: " <> show sampled
    footnote $ "bytetypes: " <> show bytetypes

    -- None of the packets start with null bytes
    assert $ not (any hasLeadingNullBytes bytetypes)

    -- All packets are packed
    assert $ all isPackedAxi4StreamPacket bytetypes

    -- The extracted packets are the same for both the expected and sampled data
    axiStreamToPackets expected === axiStreamToPackets sampled

prop_axi4StreamPacketFifo :: Property
prop_axi4StreamPacketFifo =
  idWithModel defExpectOptions gen id impl
 where
  impl = wcre @System $ axiStreamPacketFifo d8 d64

  packetGen =
    catMaybes
      <$> genRandomAxiPacket
        d4
        d0
        d0
        [NullByte, DataByte, PositionByte]
        (Range.linear 0 32)
        (pure ())

  gen = L.concat <$> Gen.list (Range.linear 0 10) packetGen

{- | Generate a 'axiStreamFromByteStream' component with variable output bus width
and test if a stream of multiple generated 'Packet's can be routed through it
without being changed.
-}
prop_axi4StreamPacketFifo_Uninterrupted :: Property
prop_axi4StreamPacketFifo_Uninterrupted = property $ do
  busWidth <- forAll $ Gen.integral $ Range.linear 1 8
  extraFifoDepth <- forAll $ Gen.integral $ Range.linear 2 64
  case ( TN.someNatVal $ fromIntegral busWidth
       , TN.someNatVal $ fromIntegral extraFifoDepth
       ) of
    ( SomeNat (Proxy :: Proxy busWidth)
      , SomeNat (Proxy :: Proxy extraFifoDepth)
      ) -> do
        let packetGen =
              genRandomAxiPacket
                (SNat @busWidth)
                d0
                d0
                [NullByte, DataByte, PositionByte]
                (Range.linear 0 (extraFifoDepth - 2))
                (pure ())
        inputData <- forAll (L.concat <$> Gen.list (Range.linear 0 10) packetGen)
        let
          conf = SimulationConfig 0 100 True
          simOut =
            withClockResetEnable @System clockGen noReset enableGen
              $ sampleC conf
              $ axiStreamPacketFifo d2 (SNat @(2 + extraFifoDepth))
              <| driveC conf inputData

        footnote $ "inputData: " <> show inputData
        footnote $ "simOut: " <> show simOut
        assert $ unInterruptedAxi4Packets simOut

{- | Verify that the 'axiStreamFromByteStream' component does not change the content of the stream
when converting 1 byte wide transfers to 4 byte wide transfers.
-}
prop_axiStreamFromByteStream :: Property
prop_axiStreamFromByteStream = propWithModel defExpectOptions gen model impl prop
 where
  impl = wcre @System $ axiUserMapC (const ()) <| axiStreamFromByteStream
  model = L.concatMap (catMaybes . packetToAxiStream d4) . axiStreamToPackets

  packetGen =
    catMaybes
      <$> genRandomAxiPacket
        d1
        d0
        d0
        [NullByte, DataByte, PositionByte]
        (Range.linear 0 16)
        (pure ())

  gen = L.concat <$> Gen.list (Range.linear 0 3) packetGen

  prop expected sampled = do
    let bytetypes = fmap getPacketByteTypes $ rights $ separatePackets sampled
    footnote $ "expected: " <> show expected
    footnote $ "sampled: " <> show sampled
    footnote $ "bytetypes: " <> show bytetypes

    -- None of the packets start with null bytes
    assert $ not (any hasLeadingNullBytes bytetypes)

    -- All packets are packed
    assert $ all isPackedAxi4StreamPacket bytetypes

    -- The extracted packets are the same for both the expected and sampled data
    axiStreamToPackets expected === axiStreamToPackets sampled

prop_packAxi4Stream :: Property
prop_packAxi4Stream = property $ do
  -- A transaction can only contain null bytes and be packed if _tlast is True
  axiWithNulls <-
    forAll $ genAxisM2S d8 d0 d0 [NullByte, DataByte, PositionByte] [True] $ pure ()
  axiWithoutNulls <-
    forAll $ genAxisM2S d8 d0 d0 [DataByte, PositionByte] [True, False] $ pure ()
  let
    resultWithNulls = packAxi4Stream axiWithNulls
    resultWithoutNulls = packAxi4Stream axiWithoutNulls
  footnote $ "resultWithNulls: " <> show resultWithNulls
  footnote $ "resultWithoutNulls: " <> show resultWithoutNulls
  assert (isPackedTransfer resultWithNulls)
  assert (isPackedTransfer resultWithoutNulls)

{- | Extract the data and strobe bytes. 'Nothing' if the corresponding keep bit
is low, 'Just' if the keep bit is high.
-}
catKeepBytes ::
  (KnownNat (DataWidth conf)) =>
  Axi4StreamM2S conf userType ->
  Vec (DataWidth conf) (Maybe (Unsigned 8, Bool))
catKeepBytes Axi4StreamM2S{..} = orNothing <$> _tkeep <*> zip _tdata _tstrb

prop_axiOperations :: Property
prop_axiOperations = property $ do
  axi <-
    forAll $ genAxisM2S d4 d0 d0 [NullByte, DataByte, PositionByte] [True, False] $ pure ()
  let
    keepBytesA = catMaybes $ toList $ catKeepBytes axi
    keepBytesB = catMaybes $ toList $ catKeepBytes (packAxi4Stream axi)
    splitConcatA = splitAxi4Stream @4 @4 (combineAxi4Stream @4 @4 (Just axi) Nothing)
    splitConcatB = splitAxi4Stream @4 @4 (combineAxi4Stream @4 @4 Nothing (Just axi))
  keepBytesA === keepBytesB
  -- Differentiate between empty and non-empty transfers
  if all not (_tkeep axi) && not (_tlast axi)
    then
      ( do
          (Nothing, Nothing) === splitConcatA
          Nothing === uncurry (<|>) splitConcatA
          Nothing === uncurry (flip (<|>)) splitConcatA
      )
    else do
      (Just axi, Nothing) === splitConcatA
      Just axi === uncurry (<|>) splitConcatA
      Just axi === uncurry (flip (<|>)) splitConcatA
      -- TODO: Overhaul of `Axi4Stream` representation for correct `Eq` instance
      assert (maybe False (eqAxi4Stream axi) (uncurry (flip (<|>)) splitConcatB))

prop_axiPacking :: Property
prop_axiPacking = propWithModel defExpectOptions gen model impl prop
 where
  impl = wcre @System axiPacking
  model = id --
  packetGen =
    catMaybes
      <$> genRandomAxiPacket
        d1
        d0
        d0
        [NullByte, DataByte, PositionByte]
        (Range.linear 0 32)
        (pure ())

  gen = L.concat <$> Gen.list (Range.linear 0 3) packetGen

  prop expected sampled = do
    let bytetypes = fmap getPacketByteTypes $ rights $ separatePackets sampled
    footnote $ "expected: " <> show expected
    footnote $ "sampled: " <> show sampled
    footnote $ "bytetypes: " <> show bytetypes

    -- None of the packets start with null bytes
    assert $ not (any hasLeadingNullBytes bytetypes)

    -- All packets are packed
    assert $ all isPackedAxi4StreamPacket bytetypes

    -- The extracted packets are the same for both the expected and sampled data
    axiStreamToPackets expected === axiStreamToPackets sampled

prop_wbAxisRxBufferReadStreams :: Property
prop_wbAxisRxBufferReadStreams = property $ do
  let packetGen =
        Gen.filter (byteTypeFilter conditions . catMaybes)
          $ genRandomAxiPacket
            d4
            d0
            d0
            [NullByte, DataByte]
            (Range.linear 0 16)
            (pure ())
  inputData <- forAll (L.concat <$> Gen.list (Range.linear 0 3) packetGen)
  extraBufferBytes <- forAll $ Gen.integral (Range.linear 31 31)
  case TN.someNatVal extraBufferBytes of
    SomeNat (Proxy :: Proxy extraBufferBytes) -> do
      let transfers =
            catMaybes
              $ wcre
              $ sampleC conf
              $ tb (SNat @(1 + extraBufferBytes))
              <| driveC conf inputData
      footnote $ "transfers: " <> show transfers
      footnote $ "inputData: " <> show inputData
      axiStreamToPackets (catMaybes inputData) === axiStreamToPackets transfers
 where
  conditions =
    [ isPackedAxi4StreamPacket
    , not . hasLeadingNullBytes
    ]
  conf = SimulationConfig 0 500 False
  tb ::
    (1 <= bufferBytes, HiddenClockResetEnable System) =>
    SNat bufferBytes ->
    Circuit
      (Axi4Stream System ('Axi4StreamConfig 4 0 0) ())
      (Axi4Stream System ('Axi4StreamConfig 4 0 0) ())
  tb bufferBytes = circuit $ \axiIn0 -> do
    axiIn1 <- axiUserMapC (const False) -< axiIn0
    _status <- wbAxisRxBufferCircuit @System @30 bufferBytes -< (wb, axiIn1)
    (wb, axiOut) <- rxReadMasterC bufferBytes
    idC -< axiOut

prop_packetConversions :: Property
prop_packetConversions = property $ do
  packets <-
    forAll
      $ Gen.list (Range.linear 1 4)
      $ Gen.list (Range.linear 1 128)
      $ genUnsigned Range.constantBounded
  let
    transfers = fmap (packetToAxiStream d4) packets
  footnote $ "transfers:" <> show transfers
  packets === axiStreamToPackets (L.concatMap catMaybes transfers)

prop_wishboneS2Axi4 :: Property
prop_wishboneS2Axi4 = property $ do
  requestsAndStalls <- forAll $ Gen.list (Range.linear 0 10) gen
  let
    eOpts = defExpectOptions
    manager = WB.driveStandard @System eOpts requestsAndStalls
    subordinate :: Circuit (Wishbone System 'Standard 32 (BitVector 32)) ()
    subordinate = withClockResetEnable clockGen resetGen enableGen $ MM.unMemmap $ wbStorage "prop_wishboneS2Axi4_subordinate" (NonReloadable $ Vec $ replicate d5 0)

    samples = WB.sampleUnfiltered eOpts manager subordinate
    transactions = fmap (\(m, s) -> (WB.m2sToRequest m, s)) $ P.filter hasBusActivity samples

  footnoteShow samples
  let
    model _ _ [] = Left "No more transactions"
    model m2sActual s2mActual ((m2sExpected, s2mExpected) : rest)
      | m2sActual /= m2sExpected = Left m2sMismatch
      | WB.eqWishboneS2M m2sActual s2mActual s2mExpected = Right rest
      | otherwise = Left s2mMismatch
     where
      m2sMismatch = [SI.i|Mismatch in Wishbone requests: #{m2sActual} /= #{m2sExpected}|]
      s2mMismatch = [SI.i|Mismatch in Wishbone responses: #{s2mActual} /= #{s2mExpected}|]

    impl = withClockResetEnable clockGen resetGen enableGen $ circuit $ \wbIn0 -> do
      wbIn1 <- Circuit (mapWb bitCoerce bitCoerce) -< wbIn0
      (ra, wa, wd) <- wbToAxi4MemoryMapped -< (wbIn1, rd, wr)
      (wbOut, rd, wr) <- axiMemoryMappedToWb -< (ra, wa, wd)
      subordinate <| Circuit (mapWb bitCoerce bitCoerce) -< wbOut

    isRead (WB.Read _ _) = True
    isRead _ = False
    isWrite (WB.Write _ _ _) = True
    isWrite _ = False

    findRead addr ((WB.Write _ _ _, _) : rest) = findRead addr rest
    findRead targetAddr ((WB.Read currentAddr _, s2m):rest) =
      (s2m.acknowledge && targetAddr == currentAddr) || findRead targetAddr rest
    findRead _ [] = False

    findReadAfterWrite ((WB.Read _ _, _): rest) = findReadAfterWrite rest
    findReadAfterWrite ((WB.Write addr _ _, s2m):rest) =
      (s2m.acknowledge && findRead addr rest) || findReadAfterWrite rest
    findReadAfterWrite _ = False

  classify "At least one ack" $ any (\(_, s2m) -> s2m.acknowledge) transactions
  classify "At least one read" $ any (isRead . fst) transactions
  classify "At least one write" $ any (isWrite . fst) transactions
  classify "At least one error" $ any (\(_, s2m) -> s2m.err) transactions
  classify "All acks" $ all (\(_, s2m) -> s2m.acknowledge) transactions
  classify "Read after write" $ findReadAfterWrite transactions

  withClockResetEnable clockGen resetGen enableGen
    $ WB.wishbonePropWithModel
      eOpts
      model
      impl
      (pure $ fmap fst requestsAndStalls)
      transactions
 where
  mapWb ::
    (BitSize a ~ BitSize b, KnownNat addrW, KnownNat (BitSize b), ShowX a, ShowX b) =>
    (a -> b) ->
    (b -> a) ->
    (Fwd (Wishbone dom mode addrW a), Bwd (Wishbone dom mode addrW b)) ->
    (Bwd (Wishbone dom mode addrW a), Fwd (Wishbone dom mode addrW b))
  mapWb f g ~(fwd, bwd) =
    ( fmap (\wb -> wb{readData = g wb.readData}) bwd
    , fmap (\wb -> wb{writeData = f wb.writeData}) fwd
    )

  gen = do
    req <- WB.genWishboneTransfer (Range.linear 0 10) genDefinedBitVector
    stall <- Gen.int (Range.linear 0 10)
    pure (req, stall)

{- | Checks that the `wishboneS2Axi4` component produces a response for each request
when connected to the `axiDummySub` component.
-}
prop_wishboneS2Axi4_NoStall :: Property
prop_wishboneS2Axi4_NoStall = property $ do
  let
    gen = do
      req <- WB.genWishboneTransfer Range.constantBounded $ unpack <$> genDefinedBitVector
      stall <- Gen.int (Range.constant 0 100)
      pure (req, stall)

  requestsAndStalls <- forAll $ Gen.list (Range.linear 1 100) gen
  let
    eOpts = defExpectOptions{eoSampleMax = 10_000}
    manager = WB.driveStandard @System eOpts requestsAndStalls

    subordinate :: Circuit (Wishbone System 'Standard 32 (Vec 4 Byte)) ()
    subordinate = withClockResetEnable clockGen resetGen enableGen $ circuit $ \wb -> do
      (ra0, wa0, wd0) <- wbToAxi4MemoryMapped -< (wb, rd0, wr0)
      (rd0, wr0) <- axiDummySub -< (ra0, wa0, wd0)
      idC -< ()

    transactions = WB.sample eOpts manager subordinate
  footnoteShow transactions
  L.length transactions === L.length requestsAndStalls

type DummyReadAddressConf addrWidth =
  'Axi.Axi4ReadAddressConfig
    'False
    'False
    0
    addrWidth
    'False
    'False
    'False
    'False
    'False
    'False
type DummyWriteAddressConf addrWidth =
  'Axi.Axi4WriteAddressConfig
    'False
    'False
    0
    addrWidth
    'False
    'False
    'False
    'False
    'False
    'False
type DummyWriteDataConf nBytes = 'Axi.Axi4WriteDataConfig 'True nBytes
type DummyReadDataConf = 'Axi.Axi4ReadDataConfig 'True 0
type DummyWriteResponseConf = 'Axi.Axi4WriteResponseConfig 'True 0

{- | An AXI4 peripheral that will produce a ReadData transaction for each ReadAddress
and a WriteResponse transaction for each combination of WriteAddress and WriteData.
The component does not store or process any data.
-}
axiDummySub ::
  ( HiddenClockResetEnable dom
  , KnownNat addrWidth
  , KnownNat nBytes
  ) =>
  Circuit
    ( Axi.Axi4ReadAddress dom (DummyReadAddressConf addrWidth) ()
    , Axi.Axi4WriteAddress dom (DummyWriteAddressConf addrWidth) ()
    , Axi.Axi4WriteData dom (DummyWriteDataConf nBytes) ()
    )
    ( Axi.Axi4ReadData dom DummyReadDataConf () (Vec nBytes (BitVector 8))
    , Axi.Axi4WriteResponse dom DummyWriteResponseConf ()
    )
axiDummySub = Circuit go
 where
  go ~(~(raFwd, waFwd, wdFwd), ~(rdBwd, wrBwd)) = ((raBwd, waBwd, wdBwd), (rdFwd, wrFwd))
   where
    ~(raBwd, rdFwd) = mealyB readHandler False (raFwd, rdBwd)
    ~(waBwd, wdBwd, wrFwd) = mealyB writeHandler (False, False) (waFwd, wdFwd, wrBwd)

  readHandler busy ~(raFwd, rdBwd) = (nextBusy, (raBwd, rdFwd))
   where
    raBwd = Axi.S2M_ReadAddress (not busy)
    rdFwd
      | busy =
          Axi.S2M_ReadData
            { _rid = 0
            , _rdata = repeat 0
            , _rresp = Protocols.Internal.toKeepType Axi.ROkay
            , _rlast = True
            , _ruser = ()
            }
      | otherwise = Axi.S2M_NoReadData
    nextBusy = (busy && not rdBwd._rready) || raFwd /= Axi.M2S_NoReadAddress

  writeHandler (gotAddr, gotData) ~(waFwd, wdFwd, wrBwd) = (nextState, (waBwd, wdBwd, wrFwd))
   where
    waBwd = Axi.S2M_WriteAddress (not gotAddr)
    wdBwd = Axi.S2M_WriteData (not gotData)

    wrFwd
      | gotAddr && gotData =
          Axi.S2M_WriteResponse
            { _bid = 0
            , _bresp = Protocols.Internal.toKeepType Axi.ROkay
            , _buser = ()
            }
      | otherwise = Axi.S2M_NoWriteResponse

    nextState
      | gotAddr && gotData && wrBwd._bready = (False, False)
      | otherwise =
          (gotAddr || waFwd /= Axi.M2S_NoWriteAddress, gotData || wdFwd /= Axi.M2S_NoWriteData)
