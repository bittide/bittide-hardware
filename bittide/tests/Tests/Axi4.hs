-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}

module Tests.Axi4 where

import Clash.Prelude
import Clash.Explicit.Prelude (noReset)

import Clash.Hedgehog.Sized.Unsigned
import Clash.Hedgehog.Sized.Vector (genVec)
import Clash.Sized.Vector(unsafeFromList)
import Data.Maybe
import Data.Proxy
import Hedgehog
import Protocols
import Protocols.Axi4.Stream
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Data.Text.IO
import Bittide.Axi4
import Bittide.Extra.Maybe

import qualified Data.List as L
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Protocols.DfConv as DfConv

wbAxisRxBufferTb = sampleC conf $ withClockResetEnable clockGen resetGen enableGen $ tb <| driveC conf inputData
 where
  inputData = [ Nothing
              , Just
                Axi4StreamM2S
                  { _tdata = 0 :> 0 :> 0 :> 0 :> Nil
                  , _tkeep = False :> False :> False :> False :> Nil
                  , _tstrb = False :> False :> False :> False :> Nil
                  , _tlast = True
                  , _tid = 0
                  , _tdest = 0
                  , _tuser = ()
                  }
            ] <> L.repeat Nothing
  nBytes = d32
  conf = SimulationConfig 0 200 False
  tb :: HiddenClockResetEnable System =>
    Circuit
      (Axi4Stream System ('Axi4StreamConfig 4 0 0 ) ())
      (Axi4Stream System ('Axi4StreamConfig 4 0 0 ) ())
  tb = circuit $ \axiIn0 -> do
    axiIn1 <- addUser <| dropUser <| axisFromByteStream <| axisToByteStream -< axiIn0
    _status <- wbAxisRxBufferCircuit @System @32 nBytes -< (wb, axiIn1)
    (wb, axiOut) <- rxReadMasterC nBytes -< ()
    idC -< axiOut

  proxyA = Proxy @(Axi4Stream System ('Axi4StreamConfig 4 0 0) Bool)
  proxyB = Proxy @(Axi4Stream System ('Axi4StreamConfig 4 0 0) ())
  proxyC = Proxy @(Axi4Stream System ('Axi4StreamConfig 4 0 0) (Vec 4 ()))
  addUser = withClockResetEnable clockGen resetGen enableGen $ DfConv.map proxyB proxyA (\axi -> axi{_tuser = False})
  dropUser = withClockResetEnable clockGen resetGen enableGen $ DfConv.map proxyC proxyB (\axi -> axi{_tuser = ()})


tests :: TestTree
tests = testGroup "Tests.Axi4"
  [ testPropertyNamed
      "Axi4Stream up scaling does not affect packet content"
      "axisFromByteStreamUnchangedPackets"
      axisFromByteStreamUnchangedPackets
  , testPropertyNamed
      "Axi4Stream down scaling does not affect packet content"
      "axisToByteStreamUnchangedPackets"
      axisToByteStreamUnchangedPackets
  , testPropertyNamed
    "Packing Axi4 streams does not affect packet content"
    "axiPackingId"
    axiPackingId
  , testPropertyNamed
      "Axi4StreamPacketFifo does not affect packet content"
      "axi4StreamPacketFifoUnchangedPackets"
      axi4StreamPacketFifoUnchangedPackets
  , testPropertyNamed
      "Axi4StreamPacketFifo tightly packs transactions if the packet fits in the fifo"
      "axi4StreamPacketFifoPackedStream"
      axi4StreamPacketFifoPackedStream
  , testPropertyNamed
      "Read Axi4 Stream packets via Wishbone" "wbAxisRxBufferReadStreams"
      wbAxisRxBufferReadStreams
  , testPropertyNamed "Various operation on Axi4StreamM2S: splitAxi4Stream combineAxi4Stream packAxi4Stream" "axiOperations" axiOperations
  , testPropertyNamed "Packet conversion utilies" "packetConversions" packetConversions
  ]

type Packet = [Unsigned 8]

genAxisM2S ::
  KnownAxi4StreamConfig conf =>
  Gen userType ->
  Gen (Axi4StreamM2S conf userType)
genAxisM2S genUser = do
  _tkeep <- genVec Gen.bool
  _tdata <- genVec $ genUnsigned Range.constantBounded
  _tstrb <- zipWith (&&) _tkeep <$> genVec Gen.bool
  _tlast <- (|| not (or _tkeep)) <$> Gen.bool
  _tid   <- genUnsigned Range.constantBounded
  _tdest <- genUnsigned Range.constantBounded
  _tuser <- genUser

  pure $ Axi4StreamM2S{..}

-- | Generate a list of Axi4StreamM2S transactions that form multiple packets, each packet
-- will end with a transaction with _tlast set to True.
genAxiPackets ::
  KnownAxi4StreamConfig conf =>
  -- | Number of packets to generate
  Range Int ->
  -- | Range of lengths for each packet, excluding the last transaction
  Range Int ->
  -- | Generator for user data
  Gen userType ->
  -- | Generator for the list of transactions
  Gen [Maybe (Axi4StreamM2S conf userType)]
genAxiPackets nPacketRanges packetRanges genUser = do
  packets <- Gen.list nPacketRanges $ genAxiPacket packetRanges genUser
  pure (L.concat packets)

-- | Generate a list of Axi4StreamM2S transactions that form a single packet
genAxiPacket ::
  KnownAxi4StreamConfig conf =>
  -- | Range for the length of the packet, excluding the last transaction
  Range Int ->
  -- | Generator for user data
  Gen userType ->
  -- | Generator for a list of transactions representing a packet
  Gen [Maybe (Axi4StreamM2S conf userType)]
genAxiPacket range genUser = do
  packetInit <- Gen.list range (Gen.maybe ((\a -> a{_tlast = False}) <$> genAxisM2S genUser))
  packetLast <- genAxisM2S genUser
  pure (packetInit <> [Just (packetLast{_tlast = True})])

-- | Checks if an Axi4Stream transaction is contigious, i.e. there are no gaps in the data.
-- A transaction can contain null bytes, but only if _tlast is set and they are not followed
-- by data bytes or position bytes.
isContigiousAxi4Stream :: KnownNat (DataWidth conf) => Axi4StreamM2S conf userType -> Bool
isContigiousAxi4Stream Axi4StreamM2S{..}
  | _tlast = not $ hasGaps _tkeep
  | otherwise = and _tkeep
 where
  rising = snd . mapAccumL (\prevKeep keep -> (keep, not prevKeep && keep)) True
  hasGaps = or .  rising

-- | Generate a 'axisFromByteStream' component with variable output bus width
-- and test if a stream of multiple generated 'Packet's can be routed through it
-- without being changed.
axisToByteStreamUnchangedPackets :: Property
axisToByteStreamUnchangedPackets = property $ do
  busWidth <- forAll @_ @Integer $ Gen.enum 0 8
  case TN.someNatVal $ fromIntegral busWidth of
    SomeNat (Proxy :: Proxy busWidth) -> do
      inputData <- forAll $ (genAxiPackets @('Axi4StreamConfig busWidth 0 0))
        (Range.linear 0 3) (Range.linear 1 16) (pure ())
      let
        conf = SimulationConfig 0 400 True
        transactions = catMaybes $ withClockResetEnable @System clockGen noReset enableGen
          $ sampleC conf $ (axisToByteStream @_ @busWidth) <| driveC conf inputData
      footnote $ "inputData: " <> show inputData
      footnote $ "transactions: " <> show transactions
      axiStreamToPackets (catMaybes inputData) === axiStreamToPackets transactions
      assert (all isContigiousAxi4Stream transactions)

-- | Check if a list of Axi4StreamM2S transactions form an uninterrupted stream.
-- When a packet transmission is started, all elements should be Just until the
-- last transaction of the packet is reached.
unInterruptedAxi4Stream :: [Maybe (Axi4StreamM2S conf userType)] -> Bool
unInterruptedAxi4Stream xs = case break (maybe False _tlast) (dropWhile isNothing xs) of
  (packet, lastTransaction : rest) ->
    all isJust (packet <> [lastTransaction]) && unInterruptedAxi4Stream rest
  (ys, []) -> all isJust ys

-- | Generate a 'axisFromByteStream' component with variable output bus width
-- and test if a stream of multiple generated 'Packet's can be routed through it
-- without being changed.
axi4StreamPacketFifoPackedStream :: Property
axi4StreamPacketFifoPackedStream = property $ do
  busWidth <- forAll @_ @Integer $ Gen.enum 0 8
  extraFifoDepth <- forAll $ Gen.enum 0 12
  case
    ( TN.someNatVal $ fromIntegral busWidth
     ,TN.someNatVal $ fromIntegral extraFifoDepth
     ) of
    ( SomeNat (Proxy :: Proxy busWidth)
     ,SomeNat (Proxy :: Proxy extraFifoDepth)
     ) -> do
      inputData <- forAll $ (genAxiPackets @('Axi4StreamConfig busWidth 0 0))
        (Range.linear 0 4) (Range.linear 1 (1 + extraFifoDepth)) (pure ())
      let
        conf = SimulationConfig 0 300 True
        simOut = withClockResetEnable @System clockGen noReset enableGen
          $ sampleC conf $ axiStreamPacketFifo (SNat @(2 + extraFifoDepth)) <| driveC conf inputData

      footnote $ "inputData: " <> show inputData
      footnote $ "simOut: " <> show simOut
      assert $ unInterruptedAxi4Stream simOut

-- | Generate a 'axisFromByteStream' component with variable output bus width
-- and test if a stream of multiple generated 'Packet's can be routed through it
-- without being changed.
axi4StreamPacketFifoUnchangedPackets :: Property
axi4StreamPacketFifoUnchangedPackets = property $ do
  busWidth <- forAll $ Gen.enum 0 6
  extraFifoDepth <- forAll $ Gen.enum 0 8
  case
    ( TN.someNatVal $ fromIntegral busWidth
     ,TN.someNatVal $ fromIntegral extraFifoDepth
     ) of
    ( SomeNat (Proxy :: Proxy busWidth)
     ,SomeNat (Proxy :: Proxy extraFifoDepth)
     ) -> do
      inputData <- forAll $ (genAxiPackets @('Axi4StreamConfig busWidth 0 0)) (Range.linear 0 3)
        (Range.linear 1 16) (pure ())
      let
        conf = SimulationConfig 0 300 True
        transactions = catMaybes $ withClockResetEnable @System clockGen noReset enableGen
          $ sampleC conf $ axiStreamPacketFifo (SNat @(2 + extraFifoDepth)) <| driveC conf inputData
      footnote $ "inputData: " <> show inputData
      footnote $ "transactions: " <> show transactions
      axiStreamToPackets (catMaybes inputData) === axiStreamToPackets transactions

-- | Generate a 'axisFromByteStream' component with variable output bus width
-- and test if a stream of multiple generated 'Packet's can be routed through it
-- without being changed.
axisFromByteStreamUnchangedPackets :: Property
axisFromByteStreamUnchangedPackets = property $ do
  busWidth <- forAll @_ @Integer $ Gen.enum 0 8
  case TN.someNatVal $ fromIntegral busWidth of
    SomeNat (Proxy :: Proxy busWidth) -> do
      inputData <- forAll $ (genAxiPackets @('Axi4StreamConfig 1 0 0))
        (Range.linear 0 3) (Range.linear 1 16) (pure ())
      let
        conf = SimulationConfig 0 300 True
        transactions = catMaybes $ withClockResetEnable @System clockGen noReset enableGen
          $ sampleC conf $ dropUserAxi4Stream <| (axisFromByteStream @_ @busWidth <| driveC conf inputData)
      footnote $ "inputData: " <> show inputData
      footnote $ "transactions: " <> show transactions
      axiStreamToPackets (catMaybes inputData) === axiStreamToPackets transactions
      assert (all isContigiousAxi4Stream transactions)

catKeepBytes ::
  KnownNat (DataWidth conf) =>
  Axi4StreamM2S conf userType ->
  Vec (DataWidth conf) (Maybe (Unsigned 8, Bool))
catKeepBytes Axi4StreamM2S{..} = orNothing <$> _tkeep <*> zip _tdata _tstrb

axiOperations :: Property
axiOperations = property $ do
  axi <- forAll $ genAxisM2S @('Axi4StreamConfig 4 0 0) $ pure ()
  let
    keepBytesA = catMaybes $ toList $ catKeepBytes axi
    keepBytesB = catMaybes $ toList $ catKeepBytes (packAxi4Stream axi)
    splitConcatA = splitAxi4Stream @4 @4 (combineAxi4Stream @4 @4 (Just axi) Nothing)
    splitConcatB = splitAxi4Stream @4 @4 (combineAxi4Stream @4 @4 Nothing (Just axi))
  keepBytesA === keepBytesB
  (Just axi, Nothing) === splitConcatA
  Just axi === uncurry (<|>) splitConcatA
  Just axi === uncurry (flip (<|>)) splitConcatA
  -- TODO: Overhaul of `Axi4Stream` representation for correct `Eq` instance
  assert (maybe False (eqAxi4Stream axi) (uncurry (flip (<|>)) splitConcatB))

axiPackingId :: Property
axiPackingId = property $ do
  extraWidth <- forAll $ Gen.integral (Range.linear 0 7)
  case TN.someNatVal extraWidth of
    SomeNat (Proxy :: Proxy extraWidth) -> do
      inputData <- forAll $ genAxiPackets @('Axi4StreamConfig (1 + extraWidth) 0 0)
        (Range.linear 0 4) (Range.linear 0 32) (pure ())
      let
        transactions = catMaybes $ withClockResetEnable @System clockGen noReset enableGen
            $ sampleC conf $ axiPacking <| driveC conf inputData
        expected = axiStreamToPackets (fmap forceKeepLowZero (catMaybes inputData))
        actual = axiStreamToPackets (fmap forceKeepLowZero transactions)
      footnote $ "inputData: " <> show inputData
      footnote $ "transactions: " <> show transactions
      expected === actual
      assert (transactions == [] || all isContigiousAxi4Stream transactions)
 where
  conf = SimulationConfig 0 300 False

wbAxisRxBufferReadStreams :: Property
wbAxisRxBufferReadStreams = property $ do
  inputData <- forAll $ genAxiPackets @('Axi4StreamConfig 4 0 0)
    (Range.linear 0 3) (Range.linear 1 16) (pure ())
  extraBufferBytes <- forAll $ Gen.integral (Range.linear 3 16)
  case (TN.someNatVal extraBufferBytes) of
    SomeNat (Proxy :: Proxy extraBufferBytes) -> do
      let transactions = catMaybes $ withClockResetEnable clockGen resetGen enableGen
            $ sampleC conf $ tb (SNat @(1 + extraBufferBytes)) <| driveC conf inputData
      footnote $ "transactions: " <> show transactions
      footnote $ "inputData: " <> show inputData
      axiStreamToPackets (catMaybes inputData) === axiStreamToPackets transactions
 where
  conf = SimulationConfig 0 100 False
  tb :: (1 <= bufferBytes, HiddenClockResetEnable System) =>
    SNat bufferBytes ->
    Circuit
      (Axi4Stream System ('Axi4StreamConfig 4 0 0 ) ())
      (Axi4Stream System ('Axi4StreamConfig 4 0 0 ) ())
  tb nBytes = circuit $ \axiIn0 -> do
    axiIn1 <- addUser <| dropUser <| axisFromByteStream <| axisToByteStream  -< axiIn0
    _status <- wbAxisRxBufferCircuit @System @32 nBytes -< (wb, axiIn1)
    (wb, axiOut) <- rxReadMasterC nBytes -< ()
    idC -< axiOut

  proxyA = Proxy @(Axi4Stream System ('Axi4StreamConfig 4 0 0) Bool)
  proxyB = Proxy @(Axi4Stream System ('Axi4StreamConfig 4 0 0) ())
  proxyC = Proxy @(Axi4Stream System ('Axi4StreamConfig 4 0 0) (Vec 4 ()))
  addUser = withClockResetEnable clockGen resetGen enableGen $ DfConv.map proxyB proxyA (\axi -> axi{_tuser = False})
  dropUser = withClockResetEnable clockGen resetGen enableGen $ DfConv.map proxyC proxyB (\axi -> axi{_tuser = ()})

packetConversions :: Property
packetConversions = property $ do
  packets <- forAll $ Gen.list (Range.linear 1 4) $ Gen.list (Range.linear 1 128) $ genUnsigned Range.constantBounded
  let
    transactions = fmap (packetToAxiStream d4) packets
  footnote $ "transactions:" <> show transactions
  packets === axiStreamToPackets (catMaybes  $ L.concat transactions)

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
  word = L.take busWidth (firstWords <> L.replicate (busWidth - 1) 0)
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
       L.replicate (L.length bs) True
    <> L.replicate (busWidth - L.length bs) False

forceKeepLowZero :: Axi4StreamM2S conf userType -> Axi4StreamM2S conf userType
forceKeepLowZero a = a{_tdata = zipWith (\k d -> if k then d else 0) (_tkeep a) (_tdata a)}
