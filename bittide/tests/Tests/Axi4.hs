-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}

module Tests.Axi4 where

import Clash.Explicit.Prelude (noReset)
import Clash.Prelude

import Clash.Hedgehog.Sized.Unsigned
import Data.Either
import Data.Maybe
import Data.Proxy
import Hedgehog
import Protocols
import Protocols.Axi4.Stream
import Test.Tasty
import Test.Tasty.Hedgehog

import Bittide.Axi4
import Bittide.Extra.Maybe
import Protocols.Hedgehog
import Tests.Axi4.Generators
import Tests.Axi4.Properties
import Tests.Axi4.Types
import Tests.Shared

import qualified Data.List as L
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

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
    (wb, axiOut) <- rxReadMasterC bufferBytes -< ()
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

{- | Force all invalid bytes to zero. This is useful for a poor man's version
of '=='.
-}
forceKeepLowZero :: Axi4StreamM2S conf userType -> Axi4StreamM2S conf userType
forceKeepLowZero a = a{_tdata = zipWith (\k d -> if k then d else 0) (_tkeep a) (_tdata a)}
