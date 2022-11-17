-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Clock controller types and some constants/defaults.
module Bittide.ClockControl
  ( ClockControlConfig (..)
  , DataCount
  , SettlePeriod
  , SpeedChange (..)
  , defClockConfig
  , pessimisticSettleCycles
  , targetDataCount
  , clockPeriodFs
  )
where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (Femtoseconds(..))
import Data.Aeson (ToJSON(toJSON))
import Data.Proxy (Proxy(..))
import GHC.Stack (HasCallStack)

import Bittide.Arithmetic.Ppm
import Bittide.Arithmetic.Time (microseconds)

import Data.Csv

type DataCount n = Unsigned n
type SettlePeriod = Femtoseconds

-- | Configuration passed to 'clockControl'
data ClockControlConfig dom n = ClockControlConfig
  { -- | The quickest a clock could possibly run at. Used to (pessimistically)
    -- estimate when a new command can be issued.
    --
    -- TODO: Should be removed, it follows from other fields + domain.
    --
    cccPessimisticPeriod :: Femtoseconds

    -- |  Like 'cccPessimisticPeriod', but expressed as number of cycles.
    --
    -- TODO: Should be removed, it follows from other fields + domain.
    --
  , cccPessimisticSettleCycles :: Unsigned 32

    -- | Period it takes for a clock frequency request to settle. This is not
    -- modelled, but an error is thrown if a request is submitted more often than
    -- this. 'clockControl' should therefore not request changes more often.
    --
    -- This is a PLL property.
    --
  , cccSettlePeriod :: Femtoseconds

    -- | Maximum deviation from "factory tuning". E.g., a clock tuned to 200 MHz
    -- and a maximum deviation of +- 100 ppm can produce a signal anywhere
    -- between 200 MHz +- 20 KHz.
    --
    -- This is an oscillator + PLL property.
    --
  , cccDeviation :: Ppm

    -- | Step size for frequency increments / decrements
    --
    -- This is a setting of the PLL. Note that though this is a setting, it is
    -- programmed over I2C. For the time being, we expect it to be programmed
    -- once after which only the FDEC/FINC pins will be used.
    --
    -- TODO: Should be expressed as PPM.
    --
  , cccStepSize :: Femtoseconds

    -- | Size of elastic buffers. Used to observe bounds and 'targetDataCount'.
    --
  , cccBufferSize :: SNat n
  } deriving (Lift)

-- | Calculate target data count given a FIFO size. Currently returns a target
-- data count of half the FIFO size.
targetDataCount :: KnownNat n => DataCount n
targetDataCount = shiftR maxBound 1

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

instance KnownNat n => ToField (Unsigned n) where
  toField = toField . toInteger

instance KnownNat n => ToJSON (Unsigned n) where
  toJSON = toJSON . toInteger

instance ToField Femtoseconds where
  toField (Femtoseconds fs) = toField fs

instance ToJSON Femtoseconds where
  toJSON (Femtoseconds fs) = toJSON fs

clockPeriodFs :: forall dom. KnownDomain dom => Proxy dom -> Femtoseconds
clockPeriodFs Proxy = Femtoseconds (1000 * snatToNum (clockPeriod @dom))

defClockConfig :: forall dom. KnownDomain dom => ClockControlConfig dom 12
defClockConfig = ClockControlConfig
  { cccPessimisticPeriod       = pessimisticPeriod
  , cccPessimisticSettleCycles = pessimisticSettleCycles self
  , cccSettlePeriod            = microseconds 1
  , cccStepSize                = stepSize
  , cccBufferSize              = d12 -- 2**12 ~ 4096
  , cccDeviation               = Ppm 100
  }
 where
  self = defClockConfig @dom
  stepSize = diffPeriod (Ppm 1) (clockPeriodFs @dom Proxy)
  pessimisticPeriod = adjustPeriod (cccDeviation self) (clockPeriodFs @dom Proxy)

-- | Number of cycles to wait on a given clock frequency and clock settings in
-- order for the settle period to pass. /Pessimistic/ means that it calculates
-- this for the fastest possible clock.
--
pessimisticSettleCycles ::
  forall dom n.
  ( HasCallStack
  , KnownDomain dom ) =>
  ClockControlConfig dom n ->
  -- | It would take a 10 GHz clock only a 10_000 cycles to wait 1 µs. This can be
  -- met by an @Unsigned 14@: @2^14 ~ 16384@. To massively overkill it we bump it
  -- up to 32 bits.
  Unsigned 32
pessimisticSettleCycles ClockControlConfig{cccSettlePeriod, cccDeviation} =
  checkedFromIntegral nCycles
 where
  nCycles = (settlePeriod `div` pessimisticPeriod) + 1
  Femtoseconds settlePeriod = cccSettlePeriod
  period = clockPeriodFs @dom Proxy
  Femtoseconds pessimisticPeriod = adjustPeriod cccDeviation period
