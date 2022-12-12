-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Si5391 where

import Clash.Prelude

import Bittide.ClockControl.ClockGenConfig
import Bittide.ClockControl.Si5391
import Bittide.Instances.Domains
import Bittide.SharedTypes
import Clash.Annotations.TH (makeTopEntity)

spiSi5391 ::
  "CLK_125MHZ_P" ::: Clock Basic125 ->
  "reset" ::: Reset Basic125 ->
  "extOp" ::: Signal Basic125 (Maybe RegisterOperation) ->
  "MISO" ::: Signal Basic125 Bit ->
  "" :::
    ( "readByte" ::: Signal Basic125 (Maybe Byte)
    , "BUSY" ::: Signal Basic125 Busy
    , "DONE" ::: Signal Basic125 Bool
    , "" :::
      ( "SCLK" ::: Signal Basic125 Bool
      , "MOSI" ::: Signal Basic125 Bit
      , "CSB" ::: Signal Basic125 Bool)
  )
spiSi5391 clk rst extOp miso = withClockResetEnable clk rst enableGen $
  si539xSpi testConfig (SNat @50000) extOp miso

makeTopEntity 'spiSi5391
