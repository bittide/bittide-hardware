-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.WireDemo.MemoryMaps where

import Bittide.Instances.Hitl.GenericDemo.MemoryMaps (extractMemoryMaps)
import Bittide.Instances.Hitl.WireDemo.UserCore (mkUserCore, ringBufferDepth)
import Protocols.MemoryMap (MemoryMap)

boot, managementUnit, clockControl :: MemoryMap
(boot, managementUnit, clockControl) = extractMemoryMaps ringBufferDepth mkUserCore
