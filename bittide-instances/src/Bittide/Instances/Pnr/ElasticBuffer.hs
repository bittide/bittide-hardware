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
import Bittide.SharedTypes (withLittleEndian)

import qualified Clash.Explicit.Prelude as E

createDomain vXilinxSystem{vPeriod = hzToPeriod 201e6, vName = "Fast"}
createDomain vXilinxSystem{vPeriod = hzToPeriod 199e6, vName = "Slow"}

elasticBufferWb ::
  "clkRead" ::: Clock Fast ->
  "resetRead" ::: Reset Fast ->
  "clkWrite" ::: Clock Slow ->
  "wbIn" ::: Signal Fast (WishboneM2S 30 4) ->
  "writeData" ::: Signal Slow (Unsigned 64) ->
  ( "wbOut" ::: Signal Fast (WishboneS2M 4)
  , "dataCount" ::: Signal Fast (RelDataCount 5)
  , "underflow" ::: Signal Fast Underflow
  , "overflow" ::: Signal Fast Overflow
  , "readData" ::: Signal Fast (ElasticBufferData (Unsigned 64))
  )
elasticBufferWb clkRead rstRead clkWrite wbIn wdata = (wbOut, dataCount, underflow, overflow, readData)
 where
  localCounter = E.register clkRead rstRead enableGen 0 (localCounter + 1)
  ((SimOnly _mm, wbOut), (dataCount, underflow, overflow, readData)) =
    withLittleEndian
      $ toSignals
        (xilinxElasticBufferWb clkRead rstRead d5 localCounter clkWrite wdata)
        (((), wbIn), ((), (), (), ()))

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
