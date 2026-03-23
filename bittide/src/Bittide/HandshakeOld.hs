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
  }

data HandshakeOutput tx rx free = HandshakeOutput
  { wordToTransceiver :: Signal tx (BitVector 64)
  , wordToUser :: Signal rx (Maybe (BitVector 64))
  , rxLast :: Signal rx Bool
  , rxUserData :: Signal rx Bool
  , txUserData :: Signal tx Bool
  , debugLinkUp :: Signal free Bool
  , debugLinkReady :: Signal free Bool
  , neighborReceiveReady :: Signal free Bool
  , neighborReceiveReadyTx :: Signal tx Bool
  , neighborTransmitReady :: Signal free Bool
  }

data TransceiverInputFromHandshake tx rx free = TransceiverInputFromHandshake
  { wordToTransceiver :: Signal tx (BitVector 64)
  , rxLast :: Signal rx Bool
  , rxUserData :: Signal rx Bool
  , txUserData :: Signal tx Bool
  }

data HandshakeInputFromTransceiver tx rx = HandshakeInputFromTransceiver
  { txReset :: Reset tx
  , reset_tx_done :: Signal tx (BitVector 1)
  , rxReset :: Reset rx
  , reset_rx_done :: Signal rx (BitVector 1)
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

{- | Perform the metadata handshake with neighbor link so both sides switch over to
  user data when ready.
-}
userDataHandshake ::
  forall rx tx free.
  (KnownDomain rx, KnownDomain tx, KnownDomain free) =>
  HandshakeInput tx rx free ->
  HandshakeInputFromTransceiver tx rx ->
  (HandshakeOutput tx rx free, TransceiverInputFromHandshake tx rx free)
userDataHandshake input fromTransceiver = (output, transceiverInputFromHandshake)
 where
  output =
    HandshakeOutput
      { wordToTransceiver
      , wordToUser
      , rxLast
      , rxUserData
      , txUserData
      , debugLinkUp
      , debugLinkReady
      , neighborReceiveReady
      , neighborReceiveReadyTx
      , neighborTransmitReady
      }

  transceiverInputFromHandshake =
    TransceiverInputFromHandshake
      wordToTransceiver
      rxLast
      rxUserData
      txUserData

  metadata = wordToMetadata <$> fromTransceiver.wordFromTransceiver
  validMeta = mux rxUserData (pure False) fromTransceiver.linkHealthy

  rxMeta = mux validMeta (Just <$> metadata) (pure Nothing)
  rxLast = maybe False (.lastMetadataWord) <$> rxMeta
  rxReadyNeighbor = maybe False (.readyToReceive) <$> rxMeta
  txReadyNeighbor = maybe False (.readyToTransmit) <$> rxMeta

  rxUserData = stickyE input.rxClock fromTransceiver.rxReset rxLast
  txUserData = stickyE input.txClock fromTransceiver.txReset txLast

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
      txUserData
      input.wordFromUser
      (metadataToWord <$> metaTx)
  wordToUser = mux rxUserData (Just <$> fromTransceiver.wordFromTransceiver) (pure Nothing)

  debugLinkUp =
    withLockTxFree txUserData
      .&&. withLockRxFree rxUserData

  debugLinkReady = debugLinkUp .||. withLockRxFree rxReadyNeighborSticky

  neighborReceiveReady = withLockRxFree rxReadyNeighborSticky
  neighborReceiveReadyTx = withLockRxTx rxReadyNeighborSticky
  neighborTransmitReady = withLockRxFree txReadyNeighborSticky

  -- Clock domain crossing functions
  withLockRxFree = Cdc.withLock input.rxClock (unpack <$> fromTransceiver.reset_rx_done) input.clock input.reset
  withLockRxTx =
    Cdc.withLock
      input.rxClock
      (unpack <$> fromTransceiver.reset_rx_done)
      input.txClock
      fromTransceiver.txReset
  withLockTxFree = Cdc.withLock input.txClock (unpack <$> fromTransceiver.reset_tx_done) input.clock input.reset
