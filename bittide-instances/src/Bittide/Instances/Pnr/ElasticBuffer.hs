-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module Bittide.Instances.Pnr.ElasticBuffer where

import Clash.Annotations.TH
import Clash.Prelude

import Bittide.ClockControl (RelDataCount)
import Bittide.ElasticBuffer

createDomain vXilinxSystem{vPeriod = hzToPeriod 201e6, vName = "Fast"}
createDomain vXilinxSystem{vPeriod = hzToPeriod 199e6, vName = "Slow"}

elasticBuffer5 ::
  "clkReadFast" ::: Clock Fast ->
  "clkWriteSlow" ::: Clock Slow ->
  "resetRead" ::: Reset Fast ->
  "writeData" ::: Signal Slow (Unsigned 8) ->
  ( "dataCount" ::: Signal Fast (RelDataCount 5)
  , "underflow" ::: Signal Fast Underflow
  , "overflow" ::: Signal Fast Overflow
  , "stable" ::: Signal Fast Stable
  , "readData" ::: Signal Fast (Unsigned 8)
  )
elasticBuffer5 = resettableXilinxElasticBuffer

makeTopEntity 'elasticBuffer5
