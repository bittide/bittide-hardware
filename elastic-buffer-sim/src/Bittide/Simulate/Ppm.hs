-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Simulate.Ppm where

import Clash.Explicit.Prelude
import Data.Ratio
import Numeric.Natural

newtype Ppm = Ppm Natural
type PeriodPs = Natural
type Hz = Ratio Natural

-- PPM arithmetic on Hz
diffHz :: Ppm -> Hz -> Hz
diffHz (Ppm ppm) hz = hz / (1e6 / (ppm % 1))

speedUpHz :: Ppm -> Hz -> Hz
speedUpHz ppm hz = hz + diffHz ppm hz

slowDownHz :: Ppm -> Hz -> Hz
slowDownHz ppm hz = hz - diffHz ppm hz

-- PPM arithmetic on periods
diffPeriod :: Ppm -> PeriodPs -> PeriodPs
diffPeriod ppm = hzToPeriod . diffHz ppm . periodToHz

speedUpPeriod :: Ppm -> PeriodPs -> PeriodPs
speedUpPeriod ppm = hzToPeriod . speedUpHz ppm . periodToHz

slownDownPeriod :: Ppm -> PeriodPs -> PeriodPs
slownDownPeriod ppm = hzToPeriod . slowDownHz ppm . periodToHz
