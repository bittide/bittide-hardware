-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module Bittide.Instances.Pnr.ElasticBuffer where

import Clash.Annotations.TH
import Clash.Prelude

import Protocols
import Protocols.Experimental.Wishbone

import Bittide.ClockControl (RelDataCount)
import Bittide.ElasticBuffer
import Bittide.ElasticBuffer.AutoCenter (autoCenter)
import Bittide.Instances.Domains (Basic400)
import Bittide.Instances.Hacks (reducePins)
import Bittide.SharedTypes (BitboneMm, withLittleEndian)
import Clash.Cores.Xilinx (withXilinx)
import Clash.Cores.Xilinx.ElasticBuffer (xilinxElasticBuffer)

import qualified Clash.Explicit.Prelude as E

createDomain vXilinxSystem{vPeriod = hzToPeriod 201e6, vName = "Fast"}
createDomain vXilinxSystem{vPeriod = hzToPeriod 199e6, vName = "Slow"}

type FifoSize = 5

elasticBufferWb ::
  "clkRead" ::: Clock Fast ->
  "rstRead" ::: Reset Fast ->
  "clkWrite" ::: Clock Slow ->
  "rstWrite" ::: Reset Slow ->
  "wbIn" ::: Signal Fast (WishboneM2S 30 4) ->
  "writeData" ::: Signal Slow (Unsigned 64) ->
  ( "wbOut" ::: Signal Fast (WishboneS2M 4)
  , "dataCount" ::: Signal Fast (RelDataCount FifoSize)
  , "underflow" ::: Signal Fast Underflow
  , "overflow" ::: Signal Slow Overflow
  , "readData" ::: Signal Fast (ElasticBufferData (Unsigned 64))
  )
elasticBufferWb clkRead rstRead clkWrite rstWrite wbIn wdata =
  (wbOut, dataCount, fifoOut.underflow, fifoOut.overflow, fifoOut.fifoOut)
 where
  localCounter = E.register clkRead rstRead enableGen 0 (localCounter + 1)
  ebCircuit ::
    Circuit
      (BitboneMm Fast 30)
      (DcFifoOutput FifoSize Fast Slow (Unsigned 64), CSignal Fast (RelDataCount FifoSize))
  ebCircuit =
    withXilinx
      $ withLittleEndian
      $ joinEbAndControl
        (elasticBufferControl clkRead rstRead localCounter clkWrite rstWrite wdata)
        (xilinxElasticBuffer clkRead clkWrite)
  ((SimOnly _mm, wbOut), (fifoOut, dataCount)) = toSignals ebCircuit $ (((), wbIn), ((), ()))

makeTopEntity 'elasticBufferWb

autoCenterFast :: Clock Basic400 -> Reset Basic400 -> Signal Basic400 Bit -> Signal Basic400 Bit
autoCenterFast clk rst = withClock clk $ reducePins dut
 where
  dut (unbundle -> (margin, dataCount, ack)) =
    bundle
      $ snd
      $ toSignals
        (withLittleEndian $ withClock clk $ autoCenter @_ @5 rst enableGen margin dataCount)
        ((), (ack, (), ()))
