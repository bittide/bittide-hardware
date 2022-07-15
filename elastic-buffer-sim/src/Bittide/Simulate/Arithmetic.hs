module Bittide.Simulate.Arithmetic where

import Clash.Explicit.Prelude
import Data.Ratio
import Numeric.Natural

type Ppm = Natural
type PeriodPs = Natural
type Hz = Ratio Natural

-- PPM arithmetic on Hz
diffHz :: Ppm -> Hz -> Hz
diffHz ppm hz = hz / (1e6 / (ppm % 1))

fastHz :: Ppm -> Hz -> Hz
fastHz ppm hz = hz + diffHz ppm hz

slowHz :: Ppm -> Hz -> Hz
slowHz ppm hz = hz - diffHz ppm hz

slowFastHz :: Ppm -> Hz -> (Hz, Hz)
slowFastHz ppm hz = (slowHz ppm hz, fastHz ppm hz)

-- PPM arithmetic on periods
diffPeriod :: Ppm -> PeriodPs -> PeriodPs
diffPeriod ppm = hzToPeriod . diffHz ppm . periodToHz

fastPeriod :: Ppm -> PeriodPs -> PeriodPs
fastPeriod ppm = hzToPeriod . fastHz ppm . periodToHz

slowPeriod :: Ppm -> PeriodPs -> PeriodPs
slowPeriod ppm = hzToPeriod . slowHz ppm . periodToHz

slowFastPeriod :: Ppm -> PeriodPs -> (PeriodPs, PeriodPs)
slowFastPeriod ppm ps = (slowPeriod ppm ps, fastPeriod ppm ps)
