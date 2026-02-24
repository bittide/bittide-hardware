-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}

module Bittide.Ringbuffer where

import Clash.Prelude

import Bittide.SharedTypes
import Clash.Class.BitPackC
import Data.Maybe
import Protocols
import Protocols.Extra
import Protocols.Idle
import Protocols.MemoryMap (Access (..))

import GHC.Stack (HasCallStack)
import qualified Protocols.Df as Df
import qualified Protocols.Df.Extra as Df
import qualified Protocols.ReqResp as ReqResp
import qualified Protocols.Wishbone.Extra as Wb

import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (..),
  addressableBytesWb,
  deviceWb,
  matchEndianness,
  registerConfig,
 )

partitionRamOp ::
  (NFDataX mask, NFDataX bv) =>
  RamOp n (mask, bv) ->
  Either (Index n) (Index n, mask, bv)
partitionRamOp = \case
  RamRead addr -> Left addr
  RamWrite addr (mask, bv) -> Right (addr, mask, bv)
  RamNoOp -> deepErrorX "RamNoOp should not be in a ReqResp stream"

{-# OPAQUE transmitRingbufferWb #-}
transmitRingbufferWb ::
  forall dom aw memDepth.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , KnownNat aw
  , 1 <= aw
  , 1 <= memDepth
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Primitive to use for the underlying memory.
  ( Enable dom ->
    Signal dom (Index memDepth) ->
    Signal dom (Maybe (Index memDepth, Bytes 8)) ->
    Signal dom (BitVector 8) ->
    Signal dom (Bytes 8)
  ) ->
  -- | Configuration for the ringbuffer.
  SNat memDepth ->
  Circuit
    (BitboneMm dom aw)
    (CSignal dom (BitVector 64))
transmitRingbufferWb primitive SNat = circuit $ \wb -> do
  [wb0] <- deviceWb "TransmitRingbuffer" <| fmapC (matchEndianness <| Wb.increaseBuswidth d1) -< wb
  reqresp <- addressableBytesWb @memDepth regConfig -< wb0
  (reads, writes0) <- ReqResp.partition partitionRamOp -< reqresp
  writes1 <- ReqResp.toDfs -< (writes0, writeAcks)
  writeAcks <- Df.pure 0
  idleSink -< reads
  readAddress <- Df.iterate (satSucc SatWrap) 0
  applyC (fmap $ fromMaybe 0) id <| Df.toMaybe <| ram -< (readAddress, writes1)
 where
  regConfig =
    (registerConfig "data")
      { access = WriteOnly
      , description = "Buffer that continuously transmits frames to the network"
      }
  ram = withClockResetEnable hasClock hasReset enableGen (Df.fromBlockRamWithMask primitive)

{-# OPAQUE receiveRingbufferWb #-}
receiveRingbufferWb ::
  forall dom aw memDepth.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , KnownNat aw
  , 1 <= aw
  , 1 <= memDepth
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Primitive to use for the underlying memory.
  ( Enable dom ->
    Signal dom (Index memDepth) ->
    Signal dom (Maybe (Index memDepth, Bytes 8)) ->
    Signal dom (Bytes 8)
  ) ->
  -- | Configuration for the ringbuffer.
  SNat memDepth ->
  Circuit
    ( (BitboneMm dom aw)
    , CSignal dom (BitVector 64)
    )
    ()
receiveRingbufferWb primitive SNat = circuit $ \(wb, Fwd frames) -> do
  let
    writeAddress = register (0 :: Index memDepth) $ fmap (satSucc SatWrap) writeAddress
    writes = fmap Just $ bundle (writeAddress, frames)
  [wb0] <-
    deviceWb "ReceiveRingbuffer"
      <| fmapC
        (matchEndianness <| Wb.increaseBuswidth d1)
      -< wb
  reqresp <- addressableBytesWb @memDepth regConfig -< wb0
  (reads, cpuWrites) <- ReqResp.partition partitionRamOp -< reqresp
  readAddress <- ReqResp.toDfs -< (reads, readData)
  idleSink -< cpuWrites
  readData <- ram -< (readAddress, Fwd writes)
  idC -< ()
 where
  regConfig =
    (registerConfig "data")
      { access = ReadOnly
      , description = "Buffer that continuously receives frames from the network"
      }
  ram = withClockResetEnable hasClock hasReset enableGen (Df.fromBlockRam primitive)
