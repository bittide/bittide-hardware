-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}

module Bittide.RingBuffer where

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

{-# OPAQUE transmitRingBuffer #-}

{- | Memory mapped component that continuously transmits frames to the network. The frames are stored
in a addressable memory region that can be read and written by the CPU. Which frames are
transmitted is controlled by a free running counter. If the transmit enable signal is high,
the counter is used as read address for the memory and the frame read from that address is transmitted to the network.
If the transmit enable is low, the counter still increments, but the component transmits zeroes
instead of an actual frame. The free-running aspect of the counter ensures that even when
the transmit enable is low, the address relation between the 'transmitRingBuffer' and
'receiveRingBuffer' is maintained.
-}
transmitRingBuffer ::
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
  -- | Memory depth in number of elements
  SNat memDepth ->
  Circuit
    (BitboneMm dom aw)
    (CSignal dom (BitVector 64))
transmitRingBuffer primitive SNat = circuit $ \wb -> do
  [wb0, wb1] <-
    deviceWbI (deviceConfig "TransmitRingBuffer"){registered = False}
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
  ram = Df.fromDualPortedBramWithMask primitive hasClock hasClock
  writeToRamOp (addr, mask, bv) = RamWrite addr (mask, bv)

{-# OPAQUE receiveRingBuffer #-}

{- | Memory mapped component that continuously receives frames from the network and stores them in
an addressable memory region that can be read by the CPU. The address to which the incoming
frames are written is controlled by a free running counter that is incremented each cycle.
This component features a writeable register that can be used to reset the counter when it
reaches a specific value. This allows the CPU to align the free-running counter with the one in
the 'transmitRingBuffer', which is necessary to maintain the address relation between these
components such that packets are correctly received.
This component also contains a receive_enable register that controls whether incoming
frames are written to the buffer. If this enable is low, the counter will still increment
to maintain the address relation with the 'transmitRingBuffer', but the incoming frame will
be ignored.
-}
receiveRingBuffer ::
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
receiveRingBuffer primitive SNat = circuit $ \(wb, Fwd frames) -> do
  Fwd writeAddress <- writeCounter -< clearAtCount
  let writes = mux receiveEnable (fmap Just $ bundle (writeAddress, frames)) (pure Nothing)
  [wb0, wb1, wb2] <-
    deviceWbI (deviceConfig "ReceiveRingBuffer"){registered = False}
      <| fmapC
        (Wb.increaseBuswidth d1)
      -< wb
  (Fwd (receiveEnable, _)) <-
    registerWbI @_ @_ @8 receiveEnableConfig False -< (wb1, Fwd (pure Nothing))

  ((_clearAtCount, clearAtCount)) <-
    registerWbDfI @_ @_ @8 clearAtCountConfig 0 -< (wb2, Fwd (pure Nothing))

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

  clearAtCountConfig =
    (registerConfig "clear_at_count")
      { access = WriteOnly
      , description =
          "Value at which the free running counter is reset to zero. This can be used to align ourselves with our link partner's TransmitRingBuffer"
      }
  receiveEnableConfig =
    (registerConfig "enable")
      { access = ReadWrite
      , description =
          "Enable signal for receiving frames from the network. When this enable is high, incoming frames are written to the buffer to the address indicated by the free running counter. When this enable is low, incoming frames are ignored, but the counter still increments to maintain alignment."
      }
  regConfig =
    (registerConfig "data")
      { access = ReadOnly
      , description =
          "Buffer that continuously receives frames from the network, updates can be blocked by setting the receive enable register to false"
      }
  ram = withClockResetEnable hasClock hasReset enableGen (Df.fromBlockRam primitive)
