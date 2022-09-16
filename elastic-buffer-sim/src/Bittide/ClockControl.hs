-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}

-- | Clock controller types and some constants/defaults.
module Bittide.ClockControl
  ( ClockControlConfig (..)
  , DataCount
  , ElasticBufferSize
  , SettlePeriod
  , SpeedChange (..)
  , defClockConfig
  , specPeriod
  , targetDataCount
  )
where

import Clash.Explicit.Prelude
import Numeric.Natural (Natural)

import Bittide.Simulate.Ppm

import Data.Csv

type ElasticBufferSize = Natural
type DataCount = Natural
type SettlePeriod = Natural

-- | Configuration passed to 'clockControl'
data ClockControlConfig = ClockControlConfig
  { -- | The quickest a clock could possibly run at. Used to (pessimistically)
    -- estimate when a new command can be issued.
    cccPessimisticPeriod :: PeriodPs

  -- | Period it takes for a clock frequency request to settle. This is not
  -- modelled, but an error is thrown if a request is submitted more often than
  -- this. 'clockControl' should therefore not request changes more often.
  , cccSettlePeriod :: PeriodPs

  -- | Maximum divergence from initial clock frequency. Used to prevent frequency
  -- runoff.
  , cccDynamicRange :: Ppm

  -- | The size of the clock frequency should "jump" on a speed change request.
  , cccStepSize :: Integer

  -- | Size of elastic buffers. Used to observe bounds and 'targetDataCount'.
  , cccBufferSize :: ElasticBufferSize
  } deriving (Lift)

-- | Calculate target data count given a FIFO size. Currently returns a target
-- data count of half the FIFO size.
targetDataCount :: ElasticBufferSize -> DataCount
targetDataCount size = size `div` 2

-- | Safer version of FINC/FDEC signals present on the Si5395/Si5391 clock multipliers.
data SpeedChange
  = SpeedUp
  | SlowDown
  | NoChange
  deriving (Eq, Show, Generic, ShowX, NFDataX)

instance ToField SpeedChange where
  toField SpeedUp = "speedUp"
  toField SlowDown = "slowDown"
  toField NoChange = "noChange"

defClockConfig :: ClockControlConfig
defClockConfig = ClockControlConfig
  { cccPessimisticPeriod = pessimisticPeriod
  -- clock adjustment takes place at 1MHz, clock is 200MHz so we
  -- can have at most one correction per 200 cycles
  , cccSettlePeriod      = pessimisticPeriod * 200
  , cccDynamicRange      = 150
  , cccStepSize          = 1
  , cccBufferSize        = 65536 -- 128
  }
 where
  specPpm = 100
  pessimisticPeriod = speedUpPeriod specPpm specPeriod

-- we use 200kHz in simulation because otherwise the periods are so small that
-- deviations can't be expressed using 'Natural's
--
-- see: https://github.com/clash-lang/clash-compiler/issues/2328
specPeriod :: PeriodPs
specPeriod = hzToPeriod 200e3
