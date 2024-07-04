-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}

module Bittide.Instances.Domains where

import Clash.Explicit.Prelude hiding (PeriodToCycles)

import Bittide.ClockControl
import Bittide.Arithmetic.Time
import Bittide.Arithmetic.Ppm
import Data.Proxy

createDomain vXilinxSystem{vName="Basic100", vPeriod= hzToPeriod 100e6}
createDomain vXilinxSystem{vName="Basic125", vPeriod= hzToPeriod 125e6}
createDomain vXilinxSystem{vName="Basic125A", vPeriod= hzToPeriod 125e6}
createDomain vXilinxSystem{vName="Basic125B", vPeriod= hzToPeriod 125e6}
createDomain vXilinxSystem{vName="Basic199", vPeriod=hzToPeriod 199e6}
createDomain vXilinxSystem{vName="Basic200", vPeriod=hzToPeriod 200e6}
createDomain vXilinxSystem{vName="Basic25", vPeriod= hzToPeriod 25e6}
createDomain vXilinxSystem{vName="Basic300", vPeriod=hzToPeriod 300e6}
createDomain vXilinxSystem{vName="Basic50", vPeriod= hzToPeriod 50e6}
createDomain vXilinxSystem{vName="Basic625", vPeriod=hzToPeriod 625e6, vResetKind=Asynchronous}
createDomain vXilinxSystem{vName="Ext125", vPeriod= hzToPeriod 125e6, vResetKind=Asynchronous}
createDomain vXilinxSystem{vName="Ext200", vPeriod=hzToPeriod 200e6, vResetKind=Asynchronous}
createDomain vXilinxSystem{vName="Ext200A", vPeriod=hzToPeriod 200e6}
createDomain vXilinxSystem{vName="Ext200B", vPeriod=hzToPeriod 200e6}
createDomain vXilinxSystem{vName="Ext300", vPeriod=hzToPeriod 300e6, vResetKind=Asynchronous}
createDomain vXilinxSystem{vName="External", vPeriod=hzToPeriod 200e6}
createDomain vXilinxSystem{vName="GthRx", vPeriod=hzToPeriod 125e6}
createDomain vXilinxSystem{vName="GthTx", vPeriod= hzToPeriod 125e6}
createDomain vXilinxSystem{vName="GthRxS", vPeriod=hzToPeriod 10e9}
createDomain vXilinxSystem{vName="GthTxS", vPeriod= hzToPeriod 10e9}
createDomain vXilinxSystem{vName="Internal", vPeriod=hzToPeriod 200e6}

type CccBufferSize = 25 :: Nat
type CccStabilityCheckerMargin = 25 :: Nat
type CccStabilityCheckerFramesize dom = PeriodToCycles dom (Seconds 2)
type CccReframingWaitTime dom = PeriodToCycles dom (Seconds 5)

-- | Clock configuration used for instances.
--
-- Compared to 'defClockConfig' this configuration has an increased
-- buffer size ('CccBufferSize'), disables reframing and uses more "human"
-- values for framesize and wait time.
instancesClockConfig ::
  forall dom .
  KnownDomain dom =>
  Proxy dom ->
  ClockControlConfig
    dom
    CccBufferSize
    CccStabilityCheckerMargin
    (CccStabilityCheckerFramesize dom)
instancesClockConfig Proxy = ClockControlConfig
  { cccPessimisticPeriod         = pessimisticPeriod
  , cccPessimisticSettleCycles   = pessimisticSettleCycles self
  , cccSettlePeriod              = microseconds 1
  , cccStepSize                  = stepSize
  , cccBufferSize                = SNat
  , cccDeviation                 = Ppm 100
  , cccStabilityCheckerMargin    = SNat
  , cccStabilityCheckerFramesize = SNat
  , cccEnableReframing           = False
  -- changed from defClockConfig, which uses a fixed number of cycles independent
  -- the clock speed of the domain
  , cccReframingWaitTime         = natToNum @(PeriodToCycles dom (Seconds 1))
  , cccEnableRustySimulation     = False
  }
 where
  self = instancesClockConfig (Proxy @dom)
  stepSize = diffPeriod (Ppm 1) (clockPeriodFs @dom Proxy)
  pessimisticPeriod = adjustPeriod (cccDeviation self) (clockPeriodFs @dom Proxy)
