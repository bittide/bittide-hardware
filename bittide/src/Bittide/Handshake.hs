-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Handshake where

import Clash.Prelude
import Protocols

import Bittide.ElasticBuffer (sticky)
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

{- The handshake itself is independent of the representation of a metadata word
in bit form. The representation itself is only relevant to `wordToMetadata` and
`metadataToWord`. Currently, we just prepend a magicConstant to the metadata byte
to make it easy to read, but in practice it could be anything.
-}
magicConstant :: (KnownNat n) => BitVector n
magicConstant = 0xdeadbeef

{- | Meta information send along with the PRBS and alignment symbols. See
"Bittide.Transceiver" documentation for more information.
-}
data Meta = Meta
  { readyToTransmit :: Bool
  -- ^ Ready to receive user data
  , readyToReceive :: Bool
  -- ^ Ready to transmit user data
  , lastMetadataWord :: Bool
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
  forall n.
  (KnownNat n, BitSize Meta <= n) =>
  BitVector n ->
  Maybe Meta
wordToMetadata word =
  let (header, meta) = split @_ @(n - 8) word
   in if (header == magicConstant)
        then Just $ unpack meta
        else Nothing

{- | Given a @Meta@, convert it to a @BitVector n@, where @n@ is a positive
multiple of @8@.
-}
metadataToWord ::
  forall n.
  (KnownNat n, BitSize Meta <= n) =>
  Meta ->
  BitVector n
metadataToWord meta = magicConstant @(n - 8) ++# (pack meta)

{-
stickyTrue ::
  (HiddenClockResetEnable dom) =>
  Signal dom Bool ->
  Signal dom Bool
stickyTrue s = sticky hasClock hasReset s
-}

handshake ::
  (KnownNat n, BitSize Meta <= n) =>
  (HiddenClockResetEnable dom) =>
  -- | From transceiver
  Signal dom (BitVector n) ->
  -- | From ugn capture
  Signal dom (BitVector n) ->
  -- | Read, Write enable regs
  Signal dom (Bool, Bool) ->
  ( -- \| To UGN capture
    Signal dom (BitVector n)
  , -- \| To transceiver
    Signal dom (BitVector n)
  , -- \| Tuple of (txLast, rxLast), which indicates to ugnCapture when to send/receive the UGN.
    Signal dom (Bool, Bool)
  )
handshake rxWordIn txWordIn regs = (rxWordOut, txWordOut, bundle (txLast, rxLast))
 where
  neighborMetadata = wordToMetadata <$> rxWordIn

  (metadata, rxLastS) = handshakeStateMachine neighborMetadata regs

  txLast = isRising False $ sticky $ metadata.lastMetadataWord
  rxLast = isRising False $ sticky $ rxLastS

  handshakeFinished = not <$> (sticky txLast)

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
  ( Signal dom Meta
  , Signal dom Bool
  )
handshakeStateMachine neighborState enableRegs = mooreB updateState id initState (neighborState, enableRegs)
 where
  initState = (Meta False False False 0, False)

  updateState hState (Nothing, _) = hState
  updateState
    -- \| Current state
    (meta, rxLast)
    -- \| Neighbor state
    (Just neighborMeta, (txEn, rxEn)) =
      ((Meta newTxReady newRxReady newTxLast 0), newRxLast)
     where
      newTxReady = meta.readyToTransmit || txEn
      newRxReady = meta.readyToReceive || ((rxEn && txEn) && neighborMeta.readyToTransmit)
      newTxLast = (neighborMeta.readyToReceive && rxEn) || meta.lastMetadataWord
      newRxLast = neighborMeta.lastMetadataWord || rxLast

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
    registerWbI
      ( (registerConfig "tx_en")
          { access = WriteOnly
          , description = "The CPU should write True once it has paused Elastic Buffer centering"
          }
      )
      False
      -< (txLastBus, Fwd (pure Nothing))
  (Fwd rxLast, _rxLastActivity) <-
    registerWbI
      ( (registerConfig "rx_en")
          { access = WriteOnly
          , description = "The CPU should write True once it is ready to receive UGN"
          }
      )
      False
      -< (rxLastBus, Fwd (pure Nothing))

  let txRxRegs = bundle (txLast, rxLast)
  let (txLinkOut, rxLinkOut, txRxLast) = handshake rxLinkIn txLinkIn txRxRegs

  idC -< (Fwd txLinkOut, Fwd rxLinkOut, Fwd txRxLast)
