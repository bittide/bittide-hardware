-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | User core for the async-comms demo: forwards the handshake's TX output
verbatim to the GTH. No extra MU busses, no per-link tap, no business
logic. This is identical to the soft-UGN demo's user core; the demos differ
only in their ring-buffer depth (this demo uses shallow @d128@-deep buffers).
-}
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
  circuit $ \(emptyUserCoreBusses, _rxs2Raw, handshakeOut) -> do
    [] <- idC -< emptyUserCoreBusses
    idC -< handshakeOut
