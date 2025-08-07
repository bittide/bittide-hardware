-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.Driver.ClockControl.Config (
  toLinkMaskCcConf,
) where

import Prelude

import Bittide.ClockControl.Config (CcConf)
import Bittide.ClockControl.Topology (Topology (..))
import Bittide.ClockControl.Topology.LinkMasks (linkMasks)
import Bittide.Instances.Hitl.Setup (FpgaCount, LinkCount)

import qualified Bittide.Instances.Hitl.Setup as Setup
import qualified Clash.Prelude as C

type LinkMaskTopology = C.Vec FpgaCount (C.BitVector LinkCount)

{- | Convert a clock control configuration for the full topology to one that
contains only the link masks. The latter has a @BitPackC@ instance, making it
suitable for use in hardware/registers.
-}
toLinkMaskCcConf :: CcConf Topology -> CcConf LinkMaskTopology
toLinkMaskCcConf ccConf = linkMasks (fmap snd Setup.fpgaSetup) <$> ccConf
