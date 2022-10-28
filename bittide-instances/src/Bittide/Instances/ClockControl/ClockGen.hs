-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.ClockControl.ClockGen where

import Clash.Explicit.Prelude

import Bittide.Instances.Domains
import Bittide.ClockControl.Config
import Bittide.ClockControl.Ppm
import GHC.Natural

import qualified Bittide.ClockControl.ClockGen as C

tunableClockGen :: Signal Basic200KHz C.SpeedChange -> Clock Basic200KHz
tunableClockGen =
  C.tunableClockGen
    @Basic200KHz
    $(lift (bitCoerce @_ @(Unsigned 64) $ cccSettlePeriod defClockConfig))
    $(lift (diffPeriod 50 (snatToNum (clockPeriod @Basic200KHz)))) -- 50 PPM offset
    $(lift (fromIntegral @_ @Natural (cccStepSize defClockConfig)))
