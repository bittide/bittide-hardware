-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module Bittide.Instances.Pnr.Ringbuffer where

import Clash.Annotations.TH
import Clash.Prelude

import Protocols
import Protocols.Df.Extra (tdpbramRamOp)
import Protocols.Wishbone

import Bittide.Instances.Domains (Bittide)
import Bittide.Ringbuffer (
  receiveRingbuffer,
  transmitRingbuffer,
  transmitRingbufferBramOnly,
  transmitRingbufferFilterOnly,
  transmitRingbufferRamOnly,
 )
import Bittide.SharedTypes (withLittleEndian)
import Clash.Cores.Xilinx.BlockRam (tdpbram)

import qualified Clash.Explicit.Prelude as E

type BufferDepth = 4000
type AddressWidth = 30

transmitRingbufferPnr ::
  "clk" ::: Clock Bittide ->
  "rst" ::: Reset Bittide ->
  "wbIn" ::: Signal Bittide (WishboneM2S AddressWidth 4) ->
  ( "wbOut" ::: Signal Bittide (WishboneS2M 4)
  , "txData" ::: Signal Bittide (BitVector 64)
  )
transmitRingbufferPnr clk rst wbIn = (wbOut, txData)
 where
  txPrim = tdpbramRamOp tdpbram clk clk
  ((SimOnly _mm, wbOut), txData) =
    withLittleEndian
      $ toSignals
        (withClockResetEnable clk rst enableGen $ transmitRingbuffer txPrim (SNat @BufferDepth))
        (((), wbIn), ())

makeTopEntity 'transmitRingbufferPnr

receiveRingbufferPnr ::
  "clk" ::: Clock Bittide ->
  "rst" ::: Reset Bittide ->
  "wbIn" ::: Signal Bittide (WishboneM2S AddressWidth 4) ->
  "rxData" ::: Signal Bittide (BitVector 64) ->
  "wbOut" ::: Signal Bittide (WishboneS2M 4)
receiveRingbufferPnr clk rst wbIn rxData = wbOut
 where
  rxPrim ena = E.blockRamU clk rst ena NoClearOnReset (SNat @BufferDepth)
  (((SimOnly _mm, wbOut), ()), ()) =
    withLittleEndian
      $ toSignals
        ( withClockResetEnable clk rst enableGen
            $ receiveRingbuffer @_ @AddressWidth rxPrim (SNat @BufferDepth)
        )
        ((((), wbIn), rxData), ())

makeTopEntity 'receiveRingbufferPnr

transmitRingbufferRamOnlyPnr ::
  "clk" ::: Clock Bittide ->
  "rst" ::: Reset Bittide ->
  "txEnable" ::: Signal Bittide Bool ->
  "txData" ::: Signal Bittide (BitVector 64)
transmitRingbufferRamOnlyPnr clk rst txEnable = txData
 where
  txPrim = tdpbramRamOp tdpbram clk clk
  ((), txData) =
    withLittleEndian
      $ toSignals
        (withClockResetEnable clk rst enableGen $ transmitRingbufferRamOnly txPrim (SNat @BufferDepth))
        (txEnable, ())

makeTopEntity 'transmitRingbufferRamOnlyPnr

transmitRingbufferFilterOnlyPnr ::
  "clk" ::: Clock Bittide ->
  "rst" ::: Reset Bittide ->
  "txEnable" ::: Signal Bittide Bool ->
  "txData" ::: Signal Bittide (Maybe (Unsigned 64))
transmitRingbufferFilterOnlyPnr clk rst txEnable = txData
 where
  ((), txData) =
    withLittleEndian
      $ toSignals
        (withClockResetEnable clk rst enableGen transmitRingbufferFilterOnly)
        (txEnable, pure (Ack True))

makeTopEntity 'transmitRingbufferFilterOnlyPnr

type RamOpType = RamOp BufferDepth (BitVector 8, BitVector 64)

transmitRingbufferBramOnlyPnr ::
  "clk" ::: Clock Bittide ->
  "rst" ::: Reset Bittide ->
  "portA" ::: Signal Bittide (Maybe RamOpType) ->
  "portB" ::: Signal Bittide (Maybe RamOpType) ->
  ( "outA" ::: Signal Bittide (Maybe (BitVector 64))
  , "outB" ::: Signal Bittide (Maybe (BitVector 64))
  )
transmitRingbufferBramOnlyPnr clk rst portA portB = (outA, outB)
 where
  txPrim = tdpbramRamOp tdpbram clk clk
  ((_ackA, _ackB), (outA, outB)) =
    toSignals
      (withClockResetEnable clk rst enableGen $ transmitRingbufferBramOnly txPrim (SNat @BufferDepth))
      ((portA, portB), (pure (Ack True), pure (Ack True)))

makeTopEntity 'transmitRingbufferBramOnlyPnr
