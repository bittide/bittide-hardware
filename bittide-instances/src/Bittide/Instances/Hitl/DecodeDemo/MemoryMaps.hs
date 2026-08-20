-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.DecodeDemo.MemoryMaps where

import Bittide.Instances.Hitl.DecodeDemo.UserCore (mkUserCore, ringBufferDepth)
import Bittide.Instances.Hitl.GenericDemo.MemoryMaps (extractMemoryMaps)
import Protocols.MemoryMap (MemoryMap)

boot, managementUnit, clockControl :: MemoryMap
(boot, managementUnit, clockControl) = extractMemoryMaps ringBufferDepth mkUserCore
