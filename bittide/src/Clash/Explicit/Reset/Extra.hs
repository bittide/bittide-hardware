-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Clash.Explicit.Reset.Extra where

import Clash.Explicit.Prelude
import Clash.Cores.Xilinx.Xpm.Cdc.Single

-- | Configuration value to indicate whether resets should be asserted or
-- deasserted. Used throughout this module.
data Asserted = Asserted | Deasserted

-- | A reset synchronizer based on 'xpmCdcSingle'. I.e., a reset synchronizer that
-- is recognized by Vivado as a safe CDC construct.
xpmResetSynchronizer ::
  (HasSynchronousReset src, KnownDomain dst) =>
  -- | Initial value of registers in 'xpmCdcSingle'
  Asserted ->
  Clock src ->
  Clock dst ->
  Reset src ->
  Reset dst
xpmResetSynchronizer asserted clkSrc clkDest =
  case asserted of
    Asserted ->   unsafeFromActiveLow . go . unsafeToActiveLow
    Deasserted -> unsafeFromActiveHigh . go . unsafeToActiveHigh
 where
  go = xpmCdcSingle clkSrc clkDest

-- | Like 'delay', but for 'Reset'. Can be used to filter glitches caused by
-- combinatorial logic.
delayReset ::
  HasSynchronousReset dom =>
  -- | Initial and reset value of register
  Asserted ->
  Clock dom ->
  Reset dom ->
  Reset dom
delayReset asserted clk =
    unsafeFromActiveHigh
  . delay clk enableGen assertedBool
  . unsafeToActiveHigh
 where
  assertedBool =
    case asserted of
      Asserted -> True
      Deasserted -> False
