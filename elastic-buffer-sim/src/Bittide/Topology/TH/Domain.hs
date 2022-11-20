-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bittide.Topology.TH.Domain where

import Clash.Explicit.Prelude

import Bittide.ClockControl (ClockControlConfig, defClockConfig)

createDomain vSystem{
    vName="Bittide"
  , vPeriod=hzToPeriod 200e6
  , vResetKind=Synchronous
  }

defBittideClockConfig :: ClockControlConfig Bittide 12
defBittideClockConfig = defClockConfig
