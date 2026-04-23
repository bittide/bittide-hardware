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
  RegisterConfig (..),
  deviceConfig,
  deviceWbI,
  registerConfig,
  registerWbI,
 )
import Protocols.Wishbone

nextLfsr :: BitVector 8 -> BitVector 8
nextLfsr 0 = 1
nextLfsr current
  | lsb current == 1 = (current `shiftR` 1) `xor` 0xB8
  | otherwise = current `shiftR` 1

{- Magic 'BitVector' used to detect whether a handshake is currently active.
-}
magicConstant :: forall n. (KnownNat n) => BitVector n
magicConstant = resize $ pack num
 where
  num = iterateI nextLfsr 0xAB :: Vec (n `DivRU` 8) (BitVector 8)

{- | Meta information send along with 'magicConstant'. Used to negotiate a common start
word. In practice, this is used to allow hardware UGN capture and ring buffer alignment.
-}
data Meta a = Meta
  { readyToReceive :: Bool
  -- ^ Ready to receive user data
  , lastMetaWord :: Bool
  -- ^ Next transceiver word will be user data, i.e., it won't contain this 'Meta' anymore
  , softwareMeta :: a
  -- ^ Additional information transmitted to the software
  }
  deriving (Generic, NFDataX, BitPack, Eq, Show)

emptyMeta :: a -> Meta a
emptyMeta softwareMeta = Meta
  { softwareMeta
  , readyToReceive = False
  , lastMetaWord = False
  }

{- | Given a @BitVector n@, parse the least @BitSize Meta@ bits as 'Meta', but only if the
rest of the bits correspond to 'magicConstant'.
-}
wordToMeta ::
  forall a n.
  (KnownNat n, BitSize (Meta a) <= n) =>
  BitVector n ->
  Maybe (Meta a)
wordToMeta word
  | header == magicConstant = Just $ unpack meta
  | otherwise = Nothing
 where
  (header, meta) = split @_ @(n - BitSize Meta) word

{- | Given a @Meta@, convert it to a @BitVector n@. The resulting 'BitVector' will have
its LSBs set to @Meta@, with its MSBs set to 'magicConstant'.
-}
metaToWord ::
  forall a n.
  (KnownNat n, BitSize (Meta a) <= n) =>
  Meta a ->
  BitVector n
metaToWord meta = magicConstant @(n - BitSize Meta) ++# pack meta

data Input dom = Input
  { fromNeighbor :: Signal dom (BitVector 64)
  , fromCore :: Signal dom (BitVector 64)
  }

instance Protocol (Input dom) where
  type Fwd (Input dom) = Input dom
  type Bwd (Input dom) = ()

data Output dom = Output
  { toNeighbor :: Signal dom (BitVector 64)
  , toNeighborLast :: Signal dom Bool
  , toNeighborDone :: Signal dom Bool

  , toCore :: Signal dom (BitVector 64)
  , toCoreLast :: Signal dom Bool
  , toCoreDone :: Signal dom Bool
  }

instance Protocol (Output dom) where
  type Fwd (Output dom) = Output dom
  type Bwd (Output dom) = ()

handshake ::
  ( KnownNat n
  , BitSize (Meta a) <= n
  , HiddenClockResetEnable dom
  ) =>
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
  neighborMetadata = wordToMeta <$> rxWordIn

  (metadata, rxLastS) = handshakeStateMachine neighborMetadata regs

  lastMetaWordSticky = sticky $ metadata.lastMetaWord
  txLast = isRising False lastMetaWordSticky
  rxLast = isRising False $ sticky $ rxLastS

  handshakeFinished = not <$> lastMetaWordSticky

  -- Words out
  txWordOut =
    mux
      (register False handshakeFinished)
      (metaToWord <$> metadata)
      txWordIn
  rxWordOut = rxWordIn

mkMeta :: Meta a -> a -> Bool -> Meta a
mkMeta neighborMeta softwareMeta readyToReceive = Meta
  { softwareMeta
  , readyToReceive
    -- Note: we don't send the last word until we're ready to receive a word ourselves. We
    -- do this, because if we'd send the last word we'd have no way to communicate whether
    -- we're ready anymore.
  , lastMetaWord = neighborMeta.readyToReceive && readyToReceive
  }

data Mode
  -- | Register data from the core to the neighbor and vice versa.
  = PassThrough
  -- | Waiting for handshake to finish. When done, revert to 'PassThrough'.
  | Armed

handshakeWb ::
  forall dom addrW nBytes.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , KnownNat addrW
  , KnownNat nBytes
  , 1 <= nBytes
  , ?byteOrder :: ByteOrder
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
  [txLastBus, rxLastBus] <- deviceWbI (deviceConfig "Handshake") -< bus

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
