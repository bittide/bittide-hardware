-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

{- | Check whether the temperature of the system monitor in the FPGA is within a
given range.
-}
module Bittide.Instances.Hitl.TemperatureMonitor where

import Clash.Prelude

import Data.Maybe (isJust)

import Clash.Annotations.TH (makeTopEntity)
import Clash.Xilinx.ClockGen (clockWizardDifferential)

import Bittide.Arithmetic.Time (trueFor)
import Bittide.Hitl (
  HitlTestCase (..),
  HitlTestGroup (..),
  hitlVioBool,
  paramForHwTargets,
 )
import Bittide.Instances.Hitl.Setup (allHwTargets)

import Bittide.Instances.Domains

import Clash.Cores.Xilinx.Ila (ila, ilaConfig)

import qualified Clash.Cores.Xilinx.SystemMonitor as SysMon
import qualified Clash.Explicit.Prelude as E

temperatureMonitor ::
  "CLK_125MHZ" ::: DiffClock Ext125 ->
  ""
    ::: Signal
          Basic200
          ( "done" ::: Bool
          , "success" ::: Bool
          )
temperatureMonitor diffClk = temperatureIla `hwSeqX` bundle (testDone, testSuccess)
 where
  (clk, rst) = clockWizardDifferential diffClk E.noReset

  (status, temperatureMaybe) =
    unbundle
      $ withClockResetEnable clk rst enableGen SysMon.temperatureMonitorCelcius
  temperature = E.regMaybe clk rst enableGen 0 temperatureMaybe

  testDone = trueFor (SNat @(Milliseconds 1200)) clk testRst (pure True)
  testSuccess =
    trueFor (SNat @(Seconds 1)) clk testRst
      $ 20
      .<=. temperature
      .&&. temperature .<=. 45

  testRst = unsafeFromActiveLow $ hitlVioBool clk testDone testSuccess

  capture :: Signal Basic200 Bool
  capture =
    withClockResetEnable clk rst enableGen
      $ onChange
      $ bundle
        ( temperature
        , status
        )
   where
    onChange ::
      (HiddenClockResetEnable dom, Eq a, NFDataX a) => Signal dom a -> Signal dom Bool
    onChange x = (Just <$> x) ./=. register Nothing (Just <$> x)

  temperatureIla :: Signal Basic200 ()
  temperatureIla =
    setName @"temperatureIla"
      $ ila
        ( ilaConfig
            $ "trigger"
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

tests :: HitlTestGroup
tests =
  HitlTestGroup
    { topEntity = 'temperatureMonitor
    , extraXdcFiles = []
    , externalHdl = []
    , testCases =
        [ HitlTestCase
            { name = "TemperatureMonitor"
            , parameters = paramForHwTargets allHwTargets ()
            , postProcData = ()
            }
        ]
    , mDriverProc = Nothing
    , mPostProc = Nothing
    }
