-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module Bittide.Instances.Pnr.ElasticBuffer where

import Clash.Prelude
import Clash.Annotations.TH

import Bittide.ElasticBuffer
import Bittide.ClockControl (DataCount)

createDomain vXilinxSystem{vPeriod=hzToPeriod 201e6, vName="Fast"}
createDomain vXilinxSystem{vPeriod=hzToPeriod 199e6, vName="Slow"}

elasticBuffer5 ::
  "clkReadFast" ::: Clock Fast ->
  "clkWriteSlow" :::Clock Slow ->
  "resetRead" ::: Reset Fast ->
  "writeData" ::: Signal Slow (Unsigned 8) ->
  ( "dataCount" ::: Signal Fast (DataCount 5)
  , "underflow" ::: Signal Fast Underflow
  , "overrflow" ::: Signal Fast Overflow
  , "ebMode" ::: Signal Fast EbMode
  , "readData" ::: Signal Fast (Unsigned 8)
  )
elasticBuffer5 = resettableXilinxElasticBuffer

makeTopEntity 'elasticBuffer5
