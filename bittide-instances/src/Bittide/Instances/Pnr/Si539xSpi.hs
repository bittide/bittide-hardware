-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Pnr.Si539xSpi where

import Clash.Prelude

import Clash.Annotations.TH (makeTopEntity)

import Bittide.ClockControl.Si5395J
import Bittide.ClockControl.Si539xSpi
import Bittide.Instances.Domains (Basic125, Basic400)
import Bittide.Instances.Hacks (reducePins)
import Bittide.SharedTypes

import qualified Protocols.Spi as Spi

si5391SpiExample ::
  (KnownDomain dom) =>
  "CLK_125MHZ_P" ::: Clock dom ->
  "reset" ::: Reset dom ->
  "extOp" ::: Signal dom (Maybe RegisterOperation) ->
  Signal dom Spi.S2M ->
  ""
    ::: ( "readByte" ::: Signal dom (Maybe Byte)
        , "BUSY" ::: Signal dom Busy
        , "STATE" ::: Signal dom (ConfigState dom TestConfig6_200_on_0a_TotalRegs)
        , "" ::: Signal dom Spi.M2S
        )
si5391SpiExample clk rst extOp spiS2M = (readByte, busy, state, m2s)
 where
  (readByte, busy, state, m2s) =
    withClockResetEnable clk rst enableGen
      $ si539xSpi testConfig6_200_on_0a_1ppb (SNat @50000) extOp spiS2M

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
si5391Spi = si5391SpiExample

makeTopEntity 'si5391Spi

si5391SpiFast :: Clock Basic400 -> Reset Basic400 -> Signal Basic400 Bit -> Signal Basic400 Bit
si5391SpiFast clk rst = withClock clk $ reducePins dut
 where
  dut (unbundle -> (extOp, spiS2M)) = bundle $ si5391SpiExample clk rst extOp spiS2M
