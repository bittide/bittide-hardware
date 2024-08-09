{-# LANGUAGE NumericUnderscores #-}
-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE TypeApplications #-}

module Bittide.Instances.Pnr.StabilityChecker where

import Clash.Prelude

import Bittide.ClockControl (RelDataCount)
import Bittide.ClockControl.StabilityChecker
import Bittide.Instances.Domains (Basic200)

stabilityChecker_3_1M ::
  Clock Basic200 ->
  Reset Basic200 ->
  Signal Basic200 (RelDataCount 16) ->
  Signal Basic200 StabilityIndication
stabilityChecker_3_1M clk rst =
  withClockResetEnable clk rst enableGen
    $ stabilityChecker d3 (SNat @1_000_000)
