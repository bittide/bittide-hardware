-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Pnr.Si539xSpi where

import Clash.Prelude

import Clash.Annotations.TH (makeTopEntity)

import Bittide.ClockControl.Si5395J
import Bittide.ClockControl.Si539xSpi
import Bittide.Instances.Domains
import Bittide.SharedTypes

import qualified Protocols.Spi as Spi

si5391Spi ::
  "CLK_125MHZ_P" ::: Clock Basic125 ->
  "reset" ::: Reset Basic125 ->
  "extOp" ::: Signal Basic125 (Maybe RegisterOperation) ->
  Signal Basic125 Spi.S2M ->
  ""
    ::: ( "readByte" ::: Signal Basic125 (Maybe Byte)
        , "BUSY" ::: Signal Basic125 Busy
        , "STATE" ::: Signal Basic125 (ConfigState Basic125 TestConfig6_200_on_0a_TotalRegs)
        , "" ::: Signal Basic125 Spi.M2S
        )
si5391Spi clk rst extOp spiS2M = (readByte, busy, state, m2s)
 where
  (readByte, busy, state, m2s) =
    withClockResetEnable clk rst enableGen
      $ si539xSpi testConfig6_200_on_0a_1ppb (SNat @50000) extOp spiS2M

makeTopEntity 'si5391Spi
