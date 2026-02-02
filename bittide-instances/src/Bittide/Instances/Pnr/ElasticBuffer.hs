-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module Bittide.Instances.Pnr.ElasticBuffer where

import Clash.Annotations.TH
import Clash.Prelude

import Protocols
import Protocols.Wishbone

import Bittide.ClockControl (RelDataCount)
import Bittide.ElasticBuffer (
  ElasticBufferData,
  Overflow,
  Underflow,
  xilinxElasticBufferWb,
 )
import Bittide.SharedTypes (Bytes, withBittideByteOrder)

import qualified Clash.Explicit.Prelude as E

createDomain vXilinxSystem{vPeriod = hzToPeriod 201e6, vName = "Fast"}
createDomain vXilinxSystem{vPeriod = hzToPeriod 199e6, vName = "Slow"}

elasticBufferWb ::
  "clkRead" ::: Clock Fast ->
  "resetRead" ::: Reset Fast ->
  "clkWrite" ::: Clock Slow ->
  "wbIn" ::: Signal Fast (WishboneM2S 30 4 (Bytes 4)) ->
  "writeData" ::: Signal Slow (Unsigned 64) ->
  ( "wbOut" ::: Signal Fast (WishboneS2M (Bytes 4))
  , "dataCount" ::: Signal Fast (RelDataCount 5)
  , "underflow" ::: Signal Fast Underflow
  , "overflow" ::: Signal Fast Overflow
  , "readData" ::: Signal Fast (ElasticBufferData (Unsigned 64))
  )
elasticBufferWb clkRead rstRead clkWrite wbIn wdata = (wbOut, dataCount, underflow, overflow, readData)
 where
  localCounter = E.register clkRead rstRead enableGen 0 (localCounter + 1)
  ((SimOnly _mm, wbOut), (dataCount, underflow, overflow, readData)) =
    withBittideByteOrder
      $ toSignals
        (xilinxElasticBufferWb clkRead rstRead d5 localCounter clkWrite wdata)
        (((), wbIn), ((), (), (), ()))

makeTopEntity 'elasticBufferWb
