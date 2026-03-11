-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Handshake where

import Clash.Prelude
import Protocols

import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving
import Clash.Class.BitPackC (ByteOrder)
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (Mm)
import Protocols.MemoryMap.Registers.WishboneStandard (deviceWb, registerConfig, registerWbI)
import Protocols.Wishbone

type BittideWord = Vec 8 (BitVector 8)

magicConstant :: BitVector (7 * 8)
magicConstant = 0xdeadbeef

{- | Meta information send along with the PRBS and alignment symbols. See module
documentation for more information.
-}
data Meta = Meta
  { readyToReceive :: Bool
  -- ^ Ready to receive user data
  , readyToTransmit :: Bool
  -- ^ Ready to transmit user data
  , lastTransmit :: Bool
  -- ^ Next word will be user data
  }
  deriving (Generic, NFDataX, Eq, Show)

{-# ANN
  module
  ( DataReprAnn
      $(liftQ [t|Meta|])
      8
      -- Test comment
      [ ConstrRepr
          'Meta
          0b0001_1111 -- constructorMask
          0b0000_0000 -- constructorValue
          [ 0b1000_0000 -- field1Mask
          , 0b0100_0000 -- field2Mask
          , 0b0010_0000 -- field3Mask
          ]
      ]
  )
  #-}
deriveBitPack [t|Meta|]

metadataFromWord ::
  BittideWord ->
  Maybe Meta
metadataFromWord word =
  -- Can also use splitAt
  if (tail (reverse word) == reverse (unpack magicConstant))
    then Just $ unpack $ last word
    else Nothing

metadataToWord ::
  Meta ->
  BittideWord
metadataToWord meta = unpack $ magicConstant ++# (pack meta)

stickyTrue ::
  (HiddenClockResetEnable dom) =>
  Signal dom Bool ->
  Signal dom Bool
stickyTrue s = s'
 where
  s' = (register False s') .||. s

handshake ::
  (KnownDomain dom, HiddenClock dom) =>
  Reset dom ->
  Enable dom ->
  -- | From transceiver
  Signal dom BittideWord ->
  -- | From ugn capture
  Signal dom BittideWord ->
  -- | Read, Write enable regs
  Signal dom (Bool, Bool) ->
  -- | To UGN capture
  ( Signal dom BittideWord
  , -- \| To transceiver
    Signal dom BittideWord
  , -- \| To UGN send/receive
    Signal dom (Bool, Bool)
  )
handshake rst en rxWordIn txWordIn regs = (rxWordOut, txWordOut, bundle (txLast, rxLast))
 where
  neighborMetadata = metadataFromWord <$> rxWordIn

  (metadata, rxLastS) = unbundle $ withReset rst $ withEnable en $ handshakeStateMachine neighborMetadata regs

  newTxLast = withReset rst $ withEnable en $ stickyTrue $ isRising False metadata.lastTransmit
  newRxLast = withReset rst $ withEnable en $ stickyTrue $ isRising False rxLastS

  (txLast, rxLast) =
    ( withReset rst $ withEnable en $ isRising False newTxLast
    , withReset rst $ withEnable en $ isRising False newRxLast
    )

  -- Words out
  txWordOut =
    mux
      (withReset rst $ withEnable en $ register False $ not <$> newTxLast)
      (metadataToWord <$> metadata)
      txWordIn
  rxWordOut = rxWordIn

handshakeStateMachine ::
  (HiddenClockResetEnable dom) =>
  -- | Neighbor state
  Signal dom (Maybe Meta) ->
  -- | txEn, rxEn regs
  Signal dom (Bool, Bool) ->
  -- | New state of node
  Signal dom (Meta, Bool)
handshakeStateMachine neighborState regs = state
 where
  state = register initState $ updateState <$> state <*> neighborState <*> regs

  initState = (Meta False False False, False)

  updateState hState _neighborState@Nothing _regs = hState
  updateState
    -- \| Current state
    (Meta txReady rxReady txLast, rxLast)
    -- \| Neighbor state
    (Just (Meta neighborTxReady neighborRxReady neighborTxLast))
    -- \| Local registers
    (txEn, rxEn) =
      ((Meta newTxReady newRxReady newTxLast), newRxLast)
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
    , -- \| 'txLast' and 'rxLast' signals for the UGN send/receive(?)
      CSignal dom (Bool, Bool)
    )
handshakeWb = circuit $ \(bus, rxLinkIn, txLinkIn) -> do
  [rxLastBus, txLastBus] <- deviceWb "Handshake" -< bus

  -- Add registerWbI's for the 2 control registers. Don't forget to add access control and
  -- a description, see other usages of 'registerWb' as examples.
  (Fwd rxLast, _rxLastActivity) <-
    registerWbI (registerConfig "rx_last") False -< (rxLastBus, Fwd (pure Nothing))
  (Fwd txLast, _txLastActivity) <-
    registerWbI (registerConfig "tx_last") False -< (txLastBus, Fwd (pure Nothing))

  -- We deconstructed these 2 signals above with the pattern match so we can treat them
  -- as normal signals. We then need to reconstruct it either here, or as I did it in the
  -- 'idC' below.
  let out = bundle (rxLast, txLast)

  idC -< (txLinkIn, rxLinkIn, Fwd out)
