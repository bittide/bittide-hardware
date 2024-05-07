-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Pnr.TemperatureMonitor where

import Clash.Prelude

import Clash.Annotations.TH (makeTopEntity)
import Clash.Xilinx.ClockGen (clockWizardDifferential)

import Clash.Cores.Xilinx.SystemMonitor (temperatureMonitor)
import Bittide.Instances.Domains

import qualified Clash.Explicit.Prelude as E


temperatureMonitorTop ::
  "CLK_125MHZ" ::: DiffClock Ext125 ->
  "LED" ::: Signal Basic100 (BitVector 8)
temperatureMonitorTop diffClk = leds
 where
  (clk, rst) = clockWizardDifferential diffClk (E.noReset)

  (_status, temperature) = unbundle $
    withClockResetEnable clk rst enableGen temperatureMonitor

  leds = (resize . flip shiftR 3 . pack) <$> temperature

makeTopEntity 'temperatureMonitorTop
