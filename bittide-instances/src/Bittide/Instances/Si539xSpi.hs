-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Si539xSpi where

import Clash.Prelude

import Bittide.ClockControl.Si539xSpi
import Bittide.ClockControl.Si5391A
import Bittide.Instances.Domains
import Bittide.SharedTypes
import Clash.Annotations.TH (makeTopEntity)

si5391Spi ::
  "CLK_125MHZ_P" ::: Clock Basic125 ->
  "reset" ::: Reset Basic125 ->
  "extOp" ::: Signal Basic125 (Maybe RegisterOperation) ->
  "MISO" ::: Signal Basic125 Bit ->
  "" :::
    ( "readByte" ::: Signal Basic125 (Maybe Byte)
    , "BUSY" ::: Signal Basic125 Busy
    , "STATE" ::: Signal Basic125 (ConfigState Basic125 432)
    , "" :::
      ( "SCLK" ::: Signal Basic125 Bool
      , "MOSI" ::: Signal Basic125 Bit
      , "CSB" ::: Signal Basic125 Bool)
  )
si5391Spi clk rst extOp miso = withClockResetEnable clk rst enableGen $
  si539xSpi testConfig (SNat @50000) extOp miso

makeTopEntity 'si5391Spi
