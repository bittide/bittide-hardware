-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | User core for the soft-UGN demo: forwards the handshake's TX output
verbatim to the GTH. No extra MU busses, no per-link tap, no business
logic.
-}
module Bittide.Instances.Hitl.SoftUgnDemo.UserCore (
  UserCoreBusses,
  RingBufferDepth,
  ringBufferDepth,
  mkUserCore,
) where

import Clash.Explicit.Prelude
import Protocols

import Bittide.Instances.Hitl.GenericDemo.BringUp (NmuRemBusWidth, UserCoreCircuit)

type UserCoreBusses = 0

type RingBufferDepth = 4000

ringBufferDepth :: SNat RingBufferDepth
ringBufferDepth = SNat

mkUserCore :: UserCoreCircuit UserCoreBusses (NmuRemBusWidth UserCoreBusses)
mkUserCore _bitClk _bitRst _bitEna _localCounter _maybeDna _appReset =
  circuit $ \(emptyUserCoreBusses, _rxs2Raw, handshakeOut) -> do
    [] <- idC -< emptyUserCoreBusses
    idC -< handshakeOut
