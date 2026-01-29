-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}

module Bittide.Ringbuffer where

import Clash.Prelude

import Bittide.Extra.Maybe
import Bittide.SharedTypes
import Clash.Class.BitPackC
import Data.Maybe
import Protocols
import Protocols.Wishbone

import GHC.Stack (HasCallStack)
import qualified Protocols.Df as Df
import qualified Protocols.Df.Extra as Df
import Protocols.Idle
import qualified Protocols.ReqResp as ReqResp
import qualified Protocols.Wishbone.Extra as Wb

import Protocols.MemoryMap.Registers.WishboneStandard (
  addressableBytesWb,
  deviceWb,
  matchEndianness,
  registerConfig,
 )

{- | Wishbone interface helper for ringbuffers. It makes ringbuffers,
which operate on 64 bit frames, addressable via a 32 bit wishbone bus.
-}
wbInterface ::
  forall nBytes addrW addresses.
  ( KnownNat nBytes
  , KnownNat addresses
  , 1 <= addresses
  , KnownNat addrW
  ) =>
  -- | Wishbone (master -> slave) data.
  WishboneM2S addrW nBytes ->
  -- | Read data to be sent over the (slave -> master) port.
  Bytes nBytes ->
  -- | (slave -> master data, write address memory element, write data memory element)
  (WishboneS2M nBytes, Index addresses, Maybe (Bytes nBytes))
wbInterface WishboneM2S{..} readData =
  ( (emptyWishboneS2M @nBytes){readData, acknowledge, err}
  , wbAddr
  , writeOp
  )
 where
  masterActive = strobe && busCycle
  maxAddress = resize $ pack (maxBound :: Index addresses)
  err = masterActive && (addr > maxAddress)
  acknowledge = masterActive && not err
  wbAddr = unpack . resize $ pack addr
  writeOp = orNothing (strobe && writeEnable && not err) writeData

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
  [wb0] <-
    deviceWb "TransmitRingbuffer"
      <| Wb.mapMm
        ( Wb.trace "tx endian"
            <| matchEndianness
            <| Wb.trace "tx large"
            <| Wb.increaseBuswidth
            <| Wb.trace "tx master"
        )
      -< wb
  reqresp <- addressableBytesWb @memDepth regConfig -< wb0
  (reads, writes0) <- ReqResp.partitionEithers -< reqresp
  writes1 <- ReqResp.toDf -< (writes0, writeAcks)
  writeAcks <- Df.pure 0
  idleSink -< reads
  readAddress <- Df.iterate (satSucc SatWrap) 0
  applyC (fmap $ fromMaybe 0) id <| Df.toMaybe <| ram -< (readAddress, writes1)
 where
  regConfig = registerConfig "data"
  ram = withClockResetEnable hasClock hasReset enableGen (Df.fromBlockramWithMask primitive)
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
      <| Wb.mapMm
        ( Wb.trace "rx endian"
            <| matchEndianness
            <| Wb.trace "rx large"
            <| Wb.increaseBuswidth
            <| Wb.trace "rx master"
        )
      -< wb
  reqresp <- addressableBytesWb @memDepth regConfig -< wb0
  (reads, cpuWrites) <- ReqResp.partitionEithers -< reqresp
  readAddress <- ReqResp.toDf -< (reads, readData)
  idleSink -< cpuWrites
  readData <- ram -< (readAddress, Fwd writes)
  idC -< ()
 where
  regConfig = registerConfig "data"
  ram = withClockResetEnable hasClock hasReset enableGen (Df.fromBlockram primitive)
