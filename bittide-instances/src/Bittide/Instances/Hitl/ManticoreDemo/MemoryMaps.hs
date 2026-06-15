-- SPDX-FileCopyrightText: 2026 QBayLogic
--
-- SPDX-License-Identifier: Apache-2.0

{- | Memory maps for the Manticore demo, used by the driver to resolve the
Wishbone addresses of the chip's host registers (ManticoreControl /
ManticoreDmi) on the management unit's bus.
-}
module Bittide.Instances.Hitl.ManticoreDemo.MemoryMaps where

import Bittide.Instances.Hitl.GenericDemo.MemoryMaps (extractMemoryMaps)
import Bittide.Instances.Hitl.ManticoreDemo.UserCore (mkUserCore, ringBufferDepth)
import Protocols.MemoryMap (MemoryMap)

boot, managementUnit, clockControl :: MemoryMap
(boot, managementUnit, clockControl) = extractMemoryMaps ringBufferDepth mkUserCore
