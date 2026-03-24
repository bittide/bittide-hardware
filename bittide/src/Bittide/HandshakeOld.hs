-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.HandshakeOld (
  HandshakeInput (..),
  HandshakeOutput (..),
  HandshakeInputFromTransceiver (..),
  TransceiverInputFromHandshake (..),
  wordToMetadata,
  metadataToWord,
  userDataHandshake,
) where

import Clash.Explicit.Prelude

import Bittide.ElasticBuffer (stickyE)
import Bittide.Handshake (Meta (..))

import qualified Bittide.Transceiver.Cdc as Cdc
import qualified Bittide.Transceiver.WordAlign as WordAlign

data HandshakeInput tx rx free = HandshakeInput
  { clock :: Clock free
  , reset :: Reset free
  , txClock :: Clock tx
  , rxClock :: Clock rx
  , wordFromUser :: Signal tx (BitVector 64)
  , txStart :: Signal tx Bool -- Should be wishbone
  , rxReady :: Signal rx Bool -- Should be wishbone
  , fromTransceiver :: HandshakeInputFromTransceiver tx rx
  }

data HandshakeInputN tx rx free n = HandshakeInputs
  { clock :: Clock free
  -- ^ See 'Input.clock'
  , reset :: Reset free
  -- ^ See 'Input.reset'
  , txClock :: Clock tx
  , rxClock :: Clock rx
  , txDatas :: Vec n (Signal tx (BitVector 64))
  -- ^ See 'Input.txData'
  , txStarts :: Vec n (Signal tx Bool)
  -- ^ See 'Input.txStart'
  , rxReadys :: Vec n (Signal rx Bool)
  -- ^ See 'Input.rxReady'
  , fromTransceivers :: Vec n (HandshakeInputFromTransceiver tx rx)
  }

data HandshakeOutput tx rx free = HandshakeOutput
  { wordToUser :: Signal rx (Maybe (BitVector 64))
  , rxLast :: Signal rx Bool
  , rxIsUserData :: Signal rx Bool
  , txIsUserData :: Signal tx Bool
  , debugLinkUp :: Signal free Bool
  , debugLinkReady :: Signal free Bool
  , neighborReceiveReady :: Signal free Bool
  , neighborReceiveReadyTx :: Signal tx Bool
  , neighborTransmitReady :: Signal free Bool
  , toTransceiver :: TransceiverInputFromHandshake tx rx free
  }

data HandshakeOutputN n tx rx free = HandshakeOutputN
  { txReadys :: Vec n (Signal tx Bool)
  -- ^ See 'Output.txReady'
  , txSamplings :: Vec n (Signal tx Bool)
  -- ^ See 'Output.txSampling'
  , rxDatas :: Vec n (Signal rx (Maybe (BitVector 64)))
  -- ^ See 'Output.rxData'
  , debugLinkUps :: Vec n (Signal free Bool)
  -- ^ See 'Output.debugLinkUp'
  , debugLinkReadys :: Vec n (Signal free Bool)
  -- ^ See 'Output.debugLinkReady'
  , neighborReceiveReadys :: Vec n (Signal free Bool)
  -- ^ See 'Output.neighborReceiveReady'
  , neighborTransmitReadys :: Vec n (Signal free Bool)
  -- ^ See 'Output.neighborTransmitReady'
  , toTransceivers :: Vec n (TransceiverInputFromHandshake tx rx free)
  }

data TransceiverInputFromHandshake tx rx free = TransceiverInputFromHandshake
  { wordToTransceiver :: Signal tx (BitVector 64)
  , rxLast :: Signal rx Bool
  , rxUserData :: Signal rx Bool
  , txUserData :: Signal tx Bool
  }

data HandshakeInputFromTransceiver tx rx = HandshakeInputFromTransceiver
  { txReset :: Reset tx
  , txResetDone :: Signal tx (BitVector 1)
  , rxReset :: Reset rx
  , rxResetDone :: Signal rx (BitVector 1)
  , linkHealthy :: Signal rx Bool -- link is healthy
  , wordFromTransceiver :: Signal rx (BitVector 64) -- Word from transceiver
  }

{- | Given a bittide word, extract the metadata. This function does not check that
the input is a metadata word, and will produce garbage if used on a user word.
-}
wordToMetadata ::
  BitVector 64 ->
  Meta
wordToMetadata word = unpack alignedMetaBits
 where
  (_alignData, payload) = WordAlign.splitMsbs @8 @8 word
  (alignedMetaBits, _) = WordAlign.splitMsbs @8 @7 payload

{- | Given a metadata, output it formatted into a bittide word. The word is
simply the metadata padded with 0s, such that one can later re-extract the
metadata with `wordToMetadata`.
-}
metadataToWord ::
  Meta ->
  BitVector 64
metadataToWord meta = word
 where
  padding = 0 :: BitVector 48
  metaWithPadding = WordAlign.joinMsbs @8 (pack meta) padding
  word = WordAlign.joinMsbs @8 0 metaWithPadding

{-
userDataHandshakeN ::
  Inputs tx rx ref free rxS n ->
  Outputs n tx rx txS free
userDataHandshakeN inputs = outputs
 where
   hInputVec =
    HandshakeInput
      <$> pure inputs.clock
      <*> pure inputs.reset
      <*> pure txClock
      <*> rxClock
      <*> inputs.txDatas
      <*> inputs.txStarts
      <*> inputs.rxReadys
      <*> pure undefined
-}

{- | Perform the metadata handshake with neighbor link so both sides switch over to
  user data when ready.
-}
userDataHandshake ::
  forall rx tx free.
  (KnownDomain rx, KnownDomain tx, KnownDomain free) =>
  HandshakeInput tx rx free ->
  HandshakeOutput tx rx free
userDataHandshake input = output
 where
  fromTransceiver = input.fromTransceiver
  output =
    HandshakeOutput
      { wordToUser
      , rxLast
      , rxIsUserData
      , txIsUserData
      , debugLinkUp
      , debugLinkReady
      , neighborReceiveReady
      , neighborReceiveReadyTx
      , neighborTransmitReady
      , toTransceiver = transceiverInputFromHandshake
      }

  transceiverInputFromHandshake =
    TransceiverInputFromHandshake
      wordToTransceiver
      rxLast
      rxIsUserData
      txIsUserData

  metadata = wordToMetadata <$> fromTransceiver.wordFromTransceiver
  validMeta = mux rxIsUserData (pure False) fromTransceiver.linkHealthy

  rxMeta = mux validMeta (Just <$> metadata) (pure Nothing)
  rxLast = maybe False (.lastMetadataWord) <$> rxMeta
  rxReadyNeighbor = maybe False (.readyToReceive) <$> rxMeta
  txReadyNeighbor = maybe False (.readyToTransmit) <$> rxMeta

  rxIsUserData = stickyE input.rxClock fromTransceiver.rxReset rxLast
  txIsUserData = stickyE input.txClock fromTransceiver.txReset txLast

  indicateRxReady =
    withLockRxTx
      (fromTransceiver.linkHealthy .&&. stickyE input.rxClock fromTransceiver.rxReset input.rxReady)

  rxReadyNeighborSticky = stickyE input.rxClock fromTransceiver.rxReset rxReadyNeighbor
  txReadyNeighborSticky = stickyE input.rxClock fromTransceiver.rxReset txReadyNeighbor
  txLast = indicateRxReady .&&. input.txStart .&&. withLockRxTx rxReadyNeighborSticky

  metaTx :: Signal tx Meta
  metaTx =
    Meta
      <$> indicateRxReady
      <*> (withLockRxTx fromTransceiver.linkHealthy .&&. input.txStart)
      <*> txLast
      -- Padding
      <*> pure 0

  wordToTransceiver =
    mux
      txIsUserData
      input.wordFromUser
      (metadataToWord <$> metaTx)
  wordToUser = mux rxIsUserData (Just <$> fromTransceiver.wordFromTransceiver) (pure Nothing)

  debugLinkUp =
    withLockTxFree txIsUserData
      .&&. withLockRxFree rxIsUserData

  debugLinkReady = debugLinkUp .||. withLockRxFree rxReadyNeighborSticky

  neighborReceiveReady = withLockRxFree rxReadyNeighborSticky
  neighborReceiveReadyTx = withLockRxTx rxReadyNeighborSticky
  neighborTransmitReady = withLockRxFree txReadyNeighborSticky

  -- Clock domain crossing functions
  withLockRxFree = Cdc.withLock input.rxClock (unpack <$> fromTransceiver.rxResetDone) input.clock input.reset
  withLockRxTx =
    Cdc.withLock
      input.rxClock
      (unpack <$> fromTransceiver.rxResetDone)
      input.txClock
      fromTransceiver.txReset
  withLockTxFree = Cdc.withLock input.txClock (unpack <$> fromTransceiver.txResetDone) input.clock input.reset
