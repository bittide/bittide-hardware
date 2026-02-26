-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Handshake where

import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving
import Clash.Prelude

-- Properties:
-- It is impossible for one side to send "lastPrbs" signal without the other link first sending "ready to receive"
-- The link will never receive lastPrbs while the read register is set to false.
-- Transmit ready - aka "I have brought up my CPUs"
-- Receive ready - aka "I'm stopping centering my elastic buffers (because centering my eb is a lossy action), so pls send Ugn soon so I don't overflow/underflow"

-- * Handshaking should always wait for writeReady before sending readReady, because once readReady is sent it cannot center elastic buffers any more.

-- * The second is a lossy action ONLY WHEN ugncapture and clock sync are done independently
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

{-
withResetEnable ::
  HiddenClock dom =>
  Reset dom ->
  Enable dom ->
  HiddenClockReset
-}

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

  updateState state _neighborState@Nothing _regs = state
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
