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
  BusActivity,
  DeviceConfig (registered),
  RegisterConfig (..),
  addressableBytesWb,
  busActivityWrite,
  deviceConfig,
  deviceWbI,
  registerConfig,
  registerWbDfI,
  registerWbI,
 )

{-# OPAQUE transmitRingbuffer #-}
transmitRingbuffer ::
  forall dom aw memDepth.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , KnownNat aw
  , 1 <= aw
  , 1 <= memDepth
  , ?byteOrder :: ByteOrder
  ) =>
  -- | Primitive to use for the underlying memory.
  ( Signal dom (RamOp memDepth (BitVector 8, BitVector 64)) ->
    Signal dom (RamOp memDepth (BitVector 8, BitVector 64)) ->
    (Signal dom (BitVector 64), Signal dom (BitVector 64))
  ) ->
  -- | Configuration for the ringbuffer.
  SNat memDepth ->
  Circuit
    (BitboneMm dom aw)
    (CSignal dom (BitVector 64))
transmitRingbuffer primitive SNat = circuit $ \wb -> do
  [wb0, wb1] <-
    deviceWbI (deviceConfig "TransmitRingbuffer"){registered = False}
      <| fmapC (Wb.increaseBuswidth d1)
      -< wb
  reqresp <- addressableBytesWb @memDepth regConfig -< wb0
  (Fwd (transmitEnable, _)) <-
    registerWbI @_ @_ @8 transmitEnableConfig False -< (wb1, Fwd (pure Nothing))
  (reads0, writes0) <- ReqResp.partitionEithers -< reqresp

  reads1 <- applyC (fmap (fmap RamRead)) id <| ReqResp.toDfs -< (reads0, rightDat)
  writes1 <- applyC (fmap (fmap writeToRamOp)) id <| ReqResp.toDfs -< (writes0, writeAcks)
  writeAcks <- Df.pure 0
  cpuRamOp <- Df.roundrobinCollect Df.Parallel -< [reads1, writes1]

  let
    readRamOp = mux transmitEnable (fmap (Just . RamRead) readCounter) (pure Nothing)
    readCounter = register (0 :: Index memDepth) (fmap (satSucc SatWrap) readCounter)

  (leftDat, rightDat) <- ram -< (Fwd readRamOp, cpuRamOp)
  applyC (fmap $ fromMaybe 0) id <| Df.toMaybe -< leftDat
 where
  transmitEnableConfig =
    (registerConfig "enable")
      { access = ReadWrite
      , description = "Enable signal that controls transmission of frames to the network"
      }
  regConfig =
    (registerConfig "data")
      { access = ReadWrite
      , description = "Buffer that continuously transmits frames to the network"
      }
  -- ram = withClockResetEnable hasClock hasReset enableGen (Df.fromBlockRamWithMask primitive)
  ram = Df.fromDualPortedBramWithMask primitive hasClock hasClock
  writeToRamOp (addr, mask, bv) = RamWrite addr (mask, bv)

{-# OPAQUE receiveRingbuffer #-}
receiveRingbuffer ::
  forall dom aw memDepth.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , KnownNat aw
  , 1 <= aw
  , 1 <= memDepth
  , ?byteOrder :: ByteOrder
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
receiveRingbuffer primitive SNat = circuit $ \(wb, Fwd frames) -> do
  Fwd writeAddress <- writeCounter -< clearAtCount
  let writes = mux receiveEnable (fmap Just $ bundle (writeAddress, frames)) (pure Nothing)
  [wb0, wb1, wb2] <-
    deviceWbI (deviceConfig "ReceiveRingbuffer"){registered = False}
      <| fmapC
        (Wb.increaseBuswidth d1)
      -< wb
  (Fwd (receiveEnable, _)) <-
    registerWbI @_ @_ @8 receiveEnableConfig False -< (wb1, Fwd (pure Nothing))

  ((_clearAtCount, clearAtCount)) <-
    registerWbDfI @_ @_ @8 (registerConfig "clearAtCount") 0 -< (wb2, Fwd (pure Nothing))

  reqresp <- addressableBytesWb @memDepth regConfig -< wb0
  (reads, cpuWrites) <- ReqResp.partitionEithers -< reqresp
  readAddress <- ReqResp.toDfs -< (reads, readData)
  idleSink -< cpuWrites
  readData <- ram -< (readAddress, Fwd writes)
  idC -< ()
 where
  writeCounter :: Circuit (Df dom (BusActivity (Index memDepth))) (CSignal dom (Index memDepth))
  writeCounter = Circuit (unbundle . mealy goWriteAddress 0 . fst)
  goWriteAddress current0 clearAt = (next, (Ack clearAck, current0))
   where
    next = satSucc SatWrap current1
    (current1, clearAck)
      | Just clear <- busActivityWrite clearAt, clear == current0 = (0, True)
      | otherwise = (current0, False)

  receiveEnableConfig =
    (registerConfig "enable")
      { access = ReadWrite
      , description = "Enable signal for receiving frames from the network"
      }
  regConfig =
    (registerConfig "data")
      { access = ReadOnly
      , description = "Buffer that continuously receives frames from the network"
      }
  ram = withClockResetEnable hasClock hasReset enableGen (Df.fromBlockRam primitive)
