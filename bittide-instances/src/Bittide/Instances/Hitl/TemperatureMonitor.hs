-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Bittide.Instances.Hitl.TemperatureMonitor where

import Clash.Prelude

import Data.Maybe (isJust)

import Clash.Annotations.TH (makeTopEntity)
import Clash.Xilinx.ClockGen (clockWizardDifferential)

import Bittide.Arithmetic.Time (Seconds, Milliseconds, PeriodToCycles, trueFor)
import Bittide.Hitl (HitlTests, allFpgas, hitlVioBool, noConfigTest)

import Bittide.Instances.Domains

import Clash.Cores.Xilinx.Ila (ila, ilaConfig)

import qualified Clash.Explicit.Prelude as E
import qualified Clash.Cores.Xilinx.SystemMonitor as SysMon


temperatureMonitor ::
  "CLK_125MHZ" ::: DiffClock Ext125 ->
  "LED" ::: Signal Basic200 (BitVector 8)
temperatureMonitor diffClk =
  temperatureIla `hwSeqX`
  testSuccess `hwSeqX`
  (resize . pack <$> temperature)
 where
  (clk, rst) = clockWizardDifferential diffClk E.noReset

  (status, temperatureMaybe) = unbundle $
    withClockResetEnable clk rst enableGen SysMon.temperatureMonitorCelcius

  temperature =
    E.regMaybe clk rst enableGen
    0
    temperatureMaybe

  counter :: Signal Basic200 (Index (PeriodToCycles Basic200 (Milliseconds 1200)))
  counter =
    E.register clk testRst enableGen 0 $
      satSucc SatBound <$> counter

  temperatureInRange = 20 .<=. temperature .&&. temperature .<=. 45
  testSuccess = trueFor (SNat @(Seconds 1)) clk testRst temperatureInRange
  testDone = counter .==. pure maxBound

  testRst = unsafeFromActiveLow testStart

  testStart :: Signal Basic200 Bool
  testStart = hitlVioBool clk testDone testSuccess

  capture :: Signal Basic200 Bool
  capture =
    withClockResetEnable clk rst enableGen $
    onChange $ bundle
      ( temperature
      , status
      )
   where
    onChange :: (HiddenClockResetEnable dom, Eq a, NFDataX a) => Signal dom a -> Signal dom Bool
    onChange x = (Just <$> x) ./=. register Nothing (Just <$> x)

  temperatureIla :: Signal Basic200 ()
  temperatureIla = setName @"temperatureIla" $ ila
    (ilaConfig $
         "trigger"
      :> "capture"
      :> "probe_temperatureReady"
      :> "probe_temperature"
      :> "probe_busy"
      :> "probe_channel"
      :> "probe_endOfConversion"
      :> "probe_endOfSequence"
      :> Nil
    )
    clk
    -- Trigger as soon as we come out of reset
    (unsafeToActiveLow rst)
    capture
    -- Debug probes
    (isJust <$> temperatureMaybe)
    temperature
    ((.busy) <$> status)
    ((.channel) <$> status)
    ((.endOfConversion) <$> status)
    ((.endOfSequence) <$> status)

makeTopEntity 'temperatureMonitor

tests :: HitlTests ()
tests = noConfigTest "TemperatureMonitor" allFpgas
