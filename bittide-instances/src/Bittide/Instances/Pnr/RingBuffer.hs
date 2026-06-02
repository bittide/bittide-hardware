-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module Bittide.Instances.Pnr.RingBuffer where

import Clash.Annotations.TH
import Clash.Prelude

import Protocols
import Protocols.Df.Extra (tdpbramRamOp)
import Protocols.Experimental.Wishbone

import Bittide.Instances.Domains (Basic350, Bittide)
import Bittide.Instances.Hacks (reducePins)
import Bittide.RingBuffer (receiveRingBuffer, transmitRingBuffer)
import Bittide.SharedTypes (withLittleEndian)
import Clash.Cores.Xilinx.BlockRam (tdpbram)

import qualified Clash.Explicit.Prelude as E

type BufferDepth = 4000
type AddressWidth = 30

transmitRingBufferExample ::
  (KnownDomain dom) =>
  "clk" ::: Clock dom ->
  "rst" ::: Reset dom ->
  "wbIn" ::: Signal dom (WishboneM2S AddressWidth 4) ->
  ( "wbOut" ::: Signal dom (WishboneS2M 4)
  , "txData" ::: Signal dom (BitVector 64)
  )
transmitRingBufferExample clk rst wbIn = (wbOut, txData)
 where
  txPrim = tdpbramRamOp tdpbram clk clk
  ((SimOnly _mm, wbOut), txData) =
    withLittleEndian
      $ toSignals
        (withClockResetEnable clk rst enableGen $ transmitRingBuffer txPrim (SNat @BufferDepth))
        (((), wbIn), ())

transmitRingBufferPnr ::
  "clk" ::: Clock Bittide ->
  "rst" ::: Reset Bittide ->
  "wbIn" ::: Signal Bittide (WishboneM2S AddressWidth 4) ->
  ( "wbOut" ::: Signal Bittide (WishboneS2M 4)
  , "txData" ::: Signal Bittide (BitVector 64)
  )
transmitRingBufferPnr = transmitRingBufferExample

makeTopEntity 'transmitRingBufferPnr

transmitRingBufferFast ::
  Clock Basic350 -> Reset Basic350 -> Signal Basic350 Bit -> Signal Basic350 Bit
transmitRingBufferFast clk rst = withClock clk $ reducePins dut
 where
  dut wbIn = bundle $ transmitRingBufferExample clk rst wbIn

receiveRingBufferExample ::
  (KnownDomain dom) =>
  "clk" ::: Clock dom ->
  "rst" ::: Reset dom ->
  "wbIn" ::: Signal dom (WishboneM2S AddressWidth 4) ->
  "rxData" ::: Signal dom (BitVector 64) ->
  "wbOut" ::: Signal dom (WishboneS2M 4)
receiveRingBufferExample clk rst wbIn rxData = wbOut
 where
  rxPrim ena = E.blockRamU clk rst ena NoClearOnReset (SNat @BufferDepth)
  (((SimOnly _mm, wbOut), ()), ()) =
    withLittleEndian
      $ toSignals
        ( withClockResetEnable clk rst enableGen
            $ receiveRingBuffer @_ @AddressWidth rxPrim (SNat @BufferDepth)
        )
        ((((), wbIn), rxData), ())

receiveRingBufferPnr ::
  "clk" ::: Clock Bittide ->
  "rst" ::: Reset Bittide ->
  "wbIn" ::: Signal Bittide (WishboneM2S AddressWidth 4) ->
  "rxData" ::: Signal Bittide (BitVector 64) ->
  "wbOut" ::: Signal Bittide (WishboneS2M 4)
receiveRingBufferPnr = receiveRingBufferExample

makeTopEntity 'receiveRingBufferPnr

receiveRingBufferFast ::
  Clock Basic350 -> Reset Basic350 -> Signal Basic350 Bit -> Signal Basic350 Bit
receiveRingBufferFast clk rst = withClock clk $ reducePins dut
 where
  dut (unbundle -> (wbIn, rxData)) = receiveRingBufferExample clk rst wbIn rxData
