-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}

module Tests.Axi4 where

import Clash.Prelude
import Clash.Explicit.Prelude (noReset)

import Clash.Hedgehog.Sized.Unsigned
import Clash.Sized.Vector(unsafeFromList)
import Data.Maybe
import Data.Proxy
import Hedgehog
import Protocols
import Protocols.Axi4.Stream
import Test.Tasty
import Test.Tasty.Hedgehog

import Bittide.Axi4
import Bittide.Axi4.Internal
import Bittide.Extra.Maybe
import Protocols.Hedgehog
import Tests.Axi4.Generators
import Tests.Shared

import qualified Data.List as L
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

tests :: TestTree
tests = testGroup "Tests.Axi4"
  [ testPropertyNamed
      "Read Axi4 Stream packets via Wishbone" "wbAxisRxBufferReadStreams"
      wbAxisRxBufferReadStreams
  , testPropertyNamed "Various operation on Axi4StreamM2S: splitAxi4Stream combineAxi4Stream packAxi4Stream" "axiOperations" axiOperations
  , testPropertyNamed "Packet conversion utilies" "packetConversions" packetConversions
  , testPropertyNamed "axisToByteStream as identity" "prop_axisToByteStream_id" prop_axisToByteStream_id
  , testPropertyNamed "axisToByteStream produces dense streams" "prop_axisToByteStream_dense" prop_axisToByteStream_dense
  , testPropertyNamed "Axi4StreamPacketFifo as identity" "prop_axi4StreamPacketFifo_id" prop_axi4StreamPacketFifo_id
  , testPropertyNamed "Axi4StreamPacketFifo produces uninterrupted packets" "axi4StreamPacketFifoUninterrupted" axi4StreamPacketFifoUninterrupted
  -- , testPropertyNamed "axisFromByteStream as identity" "prop_axisFromByteStream_id" prop_axisFromByteStream_id
  -- , testPropertyNamed "axisFromByteStream produces dense streams" "prop_axisFromByteStream_dense" prop_axisFromByteStream_dense
  ]

type Packet = [Unsigned 8]

-- | Checks if an Axi4Stream transaction is contigious, i.e. there are no gaps in the data.
-- A transaction can contain null bytes, but only if _tlast is set and they are not followed
-- by data bytes or position bytes.
isDenseAxi4Stream :: KnownNat (DataWidth conf) => Axi4StreamM2S conf userType -> Bool
isDenseAxi4Stream Axi4StreamM2S{..}
  | _tlast = not $ hasGaps _tkeep
  | otherwise = and _tkeep
 where
  rising = snd . mapAccumL (\prevKeep keep -> (keep, not prevKeep && keep)) True
  hasGaps = or .  rising

prop_axisToByteStream_id :: Property
prop_axisToByteStream_id = propWithModel defExpectOptions gen model impl prop
 where
  impl = wcre @System axisToByteStream

  model = fmap extendAxi . L.filter (\a -> or (_tkeep a :<_tlast a))

  packetGen = catMaybes <$> genAxiPacket d1 d0 d0 Sparse
    [Null, Data, Position, Reserved] (Range.linear 0 16) (pure ())
  gen = L.concat <$> Gen.list (Range.linear 0 3) packetGen

  prop as bs = axiStreamToPackets as === axiStreamToPackets bs

prop_axisToByteStream_dense :: Property
prop_axisToByteStream_dense = propWithModel defExpectOptions gen model impl prop
 where
  impl = wcre @System axisToByteStream

  model = fmap extendAxi . L.filter (\a -> or (_tkeep a :<_tlast a))

  packetGen = catMaybes <$> genAxiPacket d1 d0 d0 Sparse
    [Null, Data, Position, Reserved] (Range.linear 0 16) (pure ())

  gen = L.concat <$> Gen.list (Range.linear 0 3) packetGen

  prop _ bs = assert $ all isDenseAxi4Stream bs

prop_axi4StreamPacketFifo_id :: Property
prop_axi4StreamPacketFifo_id = propWithModel defExpectOptions gen id impl prop
 where
  impl = wcre @System $ axiStreamPacketFifo d2 d8

  packetGen = catMaybes <$> genAxiPacket d4 d0 d0 Sparse
    [Null, Data, Position, Reserved] (Range.linear 0 32) (pure ())

  gen = L.concat <$> Gen.list (Range.linear 0 10) packetGen

  prop _ bs = axiStreamToPackets bs === axiStreamToPackets bs

-- | Check if a list of Axi4StreamM2S transactions form an uninterrupted stream.
-- When a packet transmission is started, all elements should be Just until the
-- last transaction of the packet is reached.
unInterruptedAxi4Packets :: [Maybe (Axi4StreamM2S conf userType)] -> Bool
unInterruptedAxi4Packets xs = case break (maybe False _tlast) (dropWhile isNothing xs) of
  (packet, lastTransaction : rest) ->
    all isJust (packet <> [lastTransaction]) && unInterruptedAxi4Packets rest
  (ys, []) -> all isJust ys

-- | Generate a 'axisFromByteStream' component with variable output bus width
-- and test if a stream of multiple generated 'Packet's can be routed through it
-- without being changed.
axi4StreamPacketFifoUninterrupted :: Property
axi4StreamPacketFifoUninterrupted = property $ do
  busWidth <- forAll $ Gen.integral $ Range.linear 1 8
  extraFifoDepth <- forAll $ Gen.integral $ Range.linear 2 64
  case
    ( TN.someNatVal $ fromIntegral busWidth
     ,TN.someNatVal $ fromIntegral extraFifoDepth
     ) of
    ( SomeNat (Proxy :: Proxy busWidth)
     ,SomeNat (Proxy :: Proxy extraFifoDepth)
     ) -> do
      let packetGen = genAxiPacket (SNat @busWidth) d0 d0 Sparse
            [Null, Data, Position, Reserved] (Range.linear 0 (extraFifoDepth - 2)) (pure ())
      inputData <- forAll (L.concat <$> Gen.list (Range.linear 0 10) packetGen)
      let
        conf = SimulationConfig 0 100 True
        simOut = withClockResetEnable @System clockGen noReset enableGen
          $ sampleC conf $ axiStreamPacketFifo d2 (SNat @(2 + extraFifoDepth)) <| driveC conf inputData

      footnote $ "inputData: " <> show inputData
      footnote $ "simOut: " <> show simOut
      assert $ unInterruptedAxi4Packets simOut

-- | Verify that the 'axisFromByteStream' component does not change the content of the stream
-- when converting 1 byte wide transactions to 4 byte wide transactions.
prop_axisFromByteStream_id :: Property
prop_axisFromByteStream_id = propWithModel defExpectOptions gen model impl prop
 where
  impl = wcre @System $ axiUserMapC (const ()) <| axisFromByteStream
  model = L.concatMap (catMaybes . packetToAxiStream d4) . axiStreamToPackets

  packetGen = catMaybes <$> genAxiPacket d1 d0 d0 Sparse
    [Null, Data, Position, Reserved] (Range.linear 0 16) (pure ())

  gen = L.concat <$> Gen.list (Range.linear 0 3) packetGen

  prop as (fmap (axiUserMap (const ())) -> bs) = axiStreamToPackets as === axiStreamToPackets bs

-- | Verify that the 'axisFromByteStream' component produces dense streams when converting
-- 1 byte wide transactions to 4 byte wide transactions.
prop_axisFromByteStream_dense :: Property
prop_axisFromByteStream_dense = propWithModel defExpectOptions gen model impl prop
 where
  impl = wcre @System $ axiUserMapC (const ()) <| axisFromByteStream

  model = L.concatMap (catMaybes . packetToAxiStream d4) . axiStreamToPackets

  packetGen = catMaybes <$> genAxiPacket d1 d0 d0 Sparse
    [Null, Data, Position, Reserved] (Range.linear 0 16) (pure ())

  gen = L.concat <$> Gen.list (Range.linear 0 3) packetGen
  prop _ bs = do
    footnote $ "bs: " <> show bs
    assert $ all isDenseAxi4Stream bs

catKeepBytes ::
  KnownNat (DataWidth conf) =>
  Axi4StreamM2S conf userType ->
  Vec (DataWidth conf) (Maybe (Unsigned 8, Bool))
catKeepBytes Axi4StreamM2S{..} = orNothing <$> _tkeep <*> zip _tdata _tstrb

axiOperations :: Property
axiOperations = property $ do
  axi <- forAll $ genAxisM2S d4 d0 d0 [Null, Data, Position] [True, False] $ pure ()
  let
    keepBytesA = catMaybes $ toList $ catKeepBytes axi
    keepBytesB = catMaybes $ toList $ catKeepBytes (packAxi4Stream axi)
    splitConcatA = splitAxi4Stream @4 @4 (combineAxi4Stream @4 @4 (Just axi) Nothing)
    splitConcatB = splitAxi4Stream @4 @4 (combineAxi4Stream @4 @4 Nothing (Just axi))
  keepBytesA === keepBytesB
  -- Differentiate between empty and non-empty transactions
  if all not (_tkeep axi) && not (_tlast axi)
    then ( do
      (Nothing, Nothing) === splitConcatA
      Nothing === uncurry (<|>) splitConcatA
      Nothing === uncurry (flip (<|>)) splitConcatA
      )
    else
      do
      (Just axi, Nothing) === splitConcatA
      Just axi === uncurry (<|>) splitConcatA
      Just axi === uncurry (flip (<|>)) splitConcatA
      -- TODO: Overhaul of `Axi4Stream` representation for correct `Eq` instance
      assert (maybe False (eqAxi4Stream axi) (uncurry (flip (<|>)) splitConcatB))

prop_axiPacking_id :: Property
prop_axiPacking_id = propWithModel defExpectOptions gen model impl prop
 where
  impl = wcre @System axiPacking
  model = L.concatMap (catMaybes . packetToAxiStream d8) . axiStreamToPackets

  packetGen = catMaybes <$> genAxiPacket d8 d0 d0 Sparse
    [Null, Data, Position, Reserved] (Range.linear 0 16) (pure ())

  gen = L.concat <$> Gen.list (Range.linear 0 3) packetGen

  prop _ bs = axiStreamToPackets bs === axiStreamToPackets bs

prop_axiPacking_dense :: Property
prop_axiPacking_dense = propWithModel defExpectOptions gen model impl prop
 where
  impl = wcre @System axiPacking
  model = id --

  packetGen = catMaybes <$> genAxiPacket d8 d0 d0 Sparse
    [Null, Data, Position, Reserved] (Range.linear 0 16) (pure ())

  gen = L.concat <$> Gen.list (Range.linear 0 3) packetGen

  prop _ bs = assert $ all isDenseAxi4Stream bs

wbAxisRxBufferReadStreams :: Property
wbAxisRxBufferReadStreams = property $ do
  let packetGen = genAxiPacket d4 d0 d0 Dense
        [Null, Data] (Range.linear 0 16) (pure ())
  inputData <- forAll $ fmap L.concat $ Gen.list (Range.linear 0 3) packetGen
  extraBufferBytes <- forAll $ Gen.integral (Range.linear 31 31)
  case (TN.someNatVal extraBufferBytes) of
    SomeNat (Proxy :: Proxy extraBufferBytes) -> do
      let transactions = catMaybes $ wcre
            $ sampleC conf $ tb (SNat @(1 + extraBufferBytes)) <| driveC conf inputData
      footnote $ "transactions: " <> show transactions
      footnote $ "inputData: " <> show inputData
      axiStreamToPackets (catMaybes inputData) === axiStreamToPackets transactions
 where
  conf = SimulationConfig 0 500 False
  tb :: (1 <= bufferBytes, HiddenClockResetEnable System) =>
    SNat bufferBytes ->
    Circuit
      (Axi4Stream System ('Axi4StreamConfig 4 0 0 ) ())
      (Axi4Stream System ('Axi4StreamConfig 4 0 0 ) ())
  tb bufferBytes = circuit $ \axiIn0 -> do
    axiIn1 <- axiUserMapC (const False) -< axiIn0
    _status <- wbAxisRxBufferCircuit @System @32 bufferBytes -< (wb, axiIn1)
    (wb, axiOut) <- rxReadMasterC bufferBytes -< ()
    idC -< axiOut

packetConversions :: Property
packetConversions = property $ do
  packets <- forAll $ Gen.list (Range.linear 1 4) $ Gen.list (Range.linear 1 128) $ genUnsigned Range.constantBounded
  let
    transactions = fmap (packetToAxiStream d4) packets
  footnote $ "transactions:" <> show transactions
  packets === axiStreamToPackets (L.concatMap catMaybes transactions)

axiStreamToPackets ::
  KnownNat nBytes =>
  [Axi4StreamM2S ('Axi4StreamConfig nBytes 0 0) ()] ->
  [Packet]
axiStreamToPackets = L.reverse . snd . L.foldl go ([], [])
 where
  go (partialPacket, packets) Axi4StreamM2S{..}
    | _tlast = ([], L.reverse newPartial : packets)
    | otherwise   = (newPartial, packets)
   where
    newPartial = L.reverse (catMaybes (toList $ orNothing <$> _tkeep <*> _tdata)) <> partialPacket

-- Transform a 'Packet' into a list of Axi Stream operations.
packetToAxiStream ::
  forall nBytes .
  SNat nBytes ->
  Packet ->
  [Maybe (Axi4StreamM2S ('Axi4StreamConfig nBytes 0 0) ())]
packetToAxiStream w@SNat !bs
  | bs /= [] = Just axis : packetToAxiStream w rest
  | otherwise  = []
  where
  busWidth = natToNum @nBytes
  (firstWords, rest) = L.splitAt busWidth bs
  word = L.take busWidth (firstWords <> L.repeat 0)
  axis = Axi4StreamM2S
    { _tdata = unsafeFromList word
    , _tkeep = keeps
    , _tstrb = repeat True
    , _tlast = null rest
    , _tid   = 0
    , _tdest = 0
    , _tuser = deepErrorX ""
    }
  keeps = unsafeFromList $
       L.replicate (L.length bs) True <> L.repeat False

forceKeepLowZero :: Axi4StreamM2S conf userType -> Axi4StreamM2S conf userType
forceKeepLowZero a = a{_tdata = zipWith (\k d -> if k then d else 0) (_tkeep a) (_tdata a)}
