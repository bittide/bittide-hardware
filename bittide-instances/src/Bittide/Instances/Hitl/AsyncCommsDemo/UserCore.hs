-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | User core for the async-comms demo: essentially an empty circuit.
module Bittide.Instances.Hitl.AsyncCommsDemo.UserCore (
  UserCoreBusses,
  RingBufferDepth,
  ringBufferDepth,
  mkUserCore,
) where

import Clash.Explicit.Prelude
import Protocols

import Bittide.Instances.Hitl.GenericDemo.BringUp (NmuRemBusWidth, UserCoreCircuit)

type UserCoreBusses = 0

type RingBufferDepth = 128

ringBufferDepth :: SNat RingBufferDepth
ringBufferDepth = SNat

mkUserCore :: UserCoreCircuit UserCoreBusses (NmuRemBusWidth UserCoreBusses)
mkUserCore _bitClk _bitRst _bitEna _localCounter _maybeDna =
  circuit $ \(muBusses, _rxs2Raw, rxLinks) -> do
    [] <- idC -< muBusses
    idC -< rxLinks
