-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.AsyncCommsDemo.MemoryMaps where

import Bittide.Instances.Hitl.AsyncCommsDemo.UserCore (mkUserCore, ringBufferDepth)
import Bittide.Instances.Hitl.GenericDemo.MemoryMaps (extractMemoryMaps)
import Protocols.MemoryMap (MemoryMap)

boot, managementUnit, clockControl :: MemoryMap
(boot, managementUnit, clockControl) = extractMemoryMaps ringBufferDepth mkUserCore
