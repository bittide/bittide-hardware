-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Handshake where

import Clash.Prelude
import Protocols

import Clash.Class.BitPackC (ByteOrder)
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (Access (..), Mm)
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access),
  deviceWb,
  registerConfig,
  registerWbI,
 )
import Protocols.Wishbone

magicConstant :: (KnownNat n) => BitVector (n * 8)
magicConstant = 0xdeadbeef

{- | Meta information send along with the PRBS and alignment symbols. See
"Bittide.Transceiver" documentation for more information.
-}
data Meta = Meta
  { readyToReceive :: Bool
  -- ^ Ready to receive user data
  , readyToTransmit :: Bool
  -- ^ Ready to transmit user data
  , lastPrbsWord :: Bool
  -- ^ Next word will be user data
  , padding :: Unsigned 5
  -- ^ Padding up to 1 byte
  }
  deriving (Generic, NFDataX, BitPack, Eq, Show)

{- | Given a @BitVector n@, where @n@ is a positive multiple of @8@,
convert it to a @Just metadata@ (or @Nothing@ if the BitVector does
not fit the metadata format).
-}
wordToMetadata ::
  (KnownNat n) =>
  BitVector ((n + 1) * 8) ->
  Maybe Meta
wordToMetadata word =
  let (header, meta) = split word
   in if (header == magicConstant)
        then Just $ unpack meta
        else Nothing

{- | Given a @Meta@, convert it to a @BitVector n@, where @n@ is a positive
multiple of @8@.
-}
metadataToWord ::
  (KnownNat n) =>
  Meta ->
  BitVector ((n + 1) * 8)
metadataToWord meta = magicConstant ++# (pack meta)

stickyTrue ::
  (HiddenClockResetEnable dom) =>
  Signal dom Bool ->
  Signal dom Bool
stickyTrue s = s'
 where
  s' = (register False s') .||. s

handshake ::
  (KnownNat n) =>
  (HiddenClockResetEnable dom) =>
  -- | From transceiver
  Signal dom (BitVector ((n + 1) * 8)) ->
  -- | From ugn capture
  Signal dom (BitVector ((n + 1) * 8)) ->
  -- | Read, Write enable regs
  Signal dom (Bool, Bool) ->
  -- | To UGN capture
  ( Signal dom (BitVector ((n + 1) * 8))
  , -- \| To transceiver
    Signal dom (BitVector ((n + 1) * 8))
  , -- \| Tuple of (txLast, rxLast), which indicates to ugnCapture when to send/receive the UGN.
    Signal dom (Bool, Bool)
  )
handshake rxWordIn txWordIn regs = (rxWordOut, txWordOut, bundle (txLast, rxLast))
 where
  neighborMetadata = wordToMetadata <$> rxWordIn

  (metadata, rxLastS) = unbundle $ handshakeStateMachine neighborMetadata regs

  txLast = isRising False $ stickyTrue $ metadata.lastPrbsWord
  rxLast = isRising False $ stickyTrue $ rxLastS

  handshakeFinished = not <$> (stickyTrue txLast)

  -- Words out
  txWordOut =
    mux
      (register False handshakeFinished)
      (metadataToWord <$> metadata)
      txWordIn
  rxWordOut = rxWordIn

{- | The following properties should be observed by the state machine
1) A txReady should only be sent after both rxReady and rxNeighborReady
-}
handshakeStateMachine ::
  (HiddenClockResetEnable dom) =>
  -- | Neighbor state
  Signal dom (Maybe Meta) ->
  -- | txEn, rxEn regs
  Signal dom (Bool, Bool) ->
  -- | New state of node
  Signal dom (Meta, Bool)
handshakeStateMachine neighborState regs = moore updateState id initState $ bundle (neighborState, regs)
 where
  initState = (Meta False False False 0, False)

  updateState hState (Nothing, _) = hState
  updateState
    -- \| Current state
    (Meta txReady rxReady txLast pad, rxLast)
    -- \| Neighbor state
    ( (Just (Meta neighborTxReady neighborRxReady neighborTxLast _))
      , -- \| Local registers
        (txEn, rxEn)
      ) =
      ((Meta newTxReady newRxReady newTxLast pad), newRxLast)
     where
      newTxReady = txReady || txEn
      newRxReady = rxReady || ((rxEn && txEn) && neighborTxReady)
      newTxLast = (neighborRxReady && rxEn) || txLast
      newRxLast = neighborTxLast || rxLast

handshakeWb ::
  forall dom addrW nBytes.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , KnownNat addrW
  , KnownNat nBytes
  , 1 <= nBytes
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  Circuit
    ( (ToConstBwd Mm, Wishbone dom 'Standard addrW nBytes)
    , -- \| Link from transceiver
      CSignal dom (BitVector 64)
    , -- \| Link from UGN capture
      CSignal dom (BitVector 64)
    )
    ( -- \| Link to transceiver
      CSignal dom (BitVector 64)
    , -- \| Link to UGN capture
      CSignal dom (BitVector 64)
    , -- \| `txLast` and `rxLast` signal for the UGN send/receive(?)
      CSignal dom (Bool, Bool)
    )
handshakeWb = circuit $ \(bus, (Fwd rxLinkIn), (Fwd txLinkIn)) -> do
  [txLastBus, rxLastBus] <- deviceWb "Handshake" -< bus

  (Fwd txLast, _txLastActivity) <-
    registerWbI ((registerConfig "tx_last"){access = WriteOnly}) False
      -< (txLastBus, Fwd (pure Nothing))
  (Fwd rxLast, _rxLastActivity) <-
    registerWbI ((registerConfig "rx_last"){access = WriteOnly}) False
      -< (rxLastBus, Fwd (pure Nothing))

  let txRxRegs = bundle (txLast, rxLast)
  let (txLinkOut, rxLinkOut, txRxLast) = handshake rxLinkIn txLinkIn txRxRegs

  idC -< (Fwd txLinkOut, Fwd rxLinkOut, Fwd txRxLast)
