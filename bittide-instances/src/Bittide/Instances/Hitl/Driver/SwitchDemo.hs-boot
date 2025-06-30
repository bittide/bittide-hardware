-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.Driver.SwitchDemo where

import Clash.Prelude (type (<=), BitVector, KnownNat, Unsigned)

import Bittide.Hitl (DeviceInfo)
import Data.String (String)
import GHC.Stack (HasCallStack)
import System.Exit (ExitCode)
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM (VivadoM)

driver :: (HasCallStack) => String -> [(HwTarget, DeviceInfo)] -> VivadoM ExitCode
whoAmIPrefix :: forall n. (KnownNat n, 3 <= n) => Unsigned n
ccWhoAmID :: BitVector 32
muWhoAmID :: BitVector 32
