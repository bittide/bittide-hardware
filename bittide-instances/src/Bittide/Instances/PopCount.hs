-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}

module Bittide.Instances.PopCount where

import Clash.Prelude

import qualified Bittide.PopCountTest as T
import Bittide.Instances.Domains
import Bittide.ClockControl

popCountTest ::
  Clock Basic200 ->
  Reset Basic200 ->
  Enable Basic200 ->
  -- | Data counts from elastic buffers
  Vec 3 (Signal Basic200 (DataCount 12)) ->
  -- | Speed change requested from clock multiplier
  Signal Basic200 Bool
popCountTest clk rst ena dataCounts =
  withClockResetEnable clk rst ena $ T.popCountTest $ bundle dataCounts
