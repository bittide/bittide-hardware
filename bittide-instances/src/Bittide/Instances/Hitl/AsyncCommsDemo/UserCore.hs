-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | User core for the async-comms demo: forwards the handshake's TX output
verbatim to the GTH. No extra MU busses, no per-link tap, no business
logic. This is identical to the soft-UGN demo's user core; the demos differ
only in their ring-buffer depth (this demo uses shallow @d128@-deep buffers)
and management unit memory size.
-}
module Bittide.Instances.Hitl.AsyncCommsDemo.UserCore (
  UserCoreBusses,
  RingBufferDepth,
  ringBufferDepth,
  mkUserCore,
  muConfig,
) where

import Clash.Explicit.Prelude
import Protocols

import Bittide.Instances.Hitl.GenericDemo.BringUp (NmuRemBusWidth, UserCoreCircuit)
import Bittide.Instances.Hitl.GenericDemo.Core (NmuInternalBusses)
import Bittide.ProcessingElement (PeConfig (..), PrefixWidth)

import qualified Bittide.Cpus.Riscv32imc as Riscv32imc

type UserCoreBusses = 0

type RingBufferDepth = 128

ringBufferDepth :: SNat RingBufferDepth
ringBufferDepth = SNat

{- | Management unit configuration for the async comms demo. Its firmware is
much larger than that of the other demos (it links in smoltcp), so it needs
more memory than the default 'Bittide.Instances.Hitl.GenericDemo.Core.muConfig'
provides.
-}
muConfig ::
  ( KnownNat n
  , PrefixWidth (n + NmuInternalBusses) <= 30
  ) =>
  PeConfig (n + NmuInternalBusses)
muConfig =
  PeConfig
    { cpu = Riscv32imc.vexRiscv1
    , depthI = SNat @(Div (10 * 16 * 1024) 4) -- One RAMB18E2 is 16KB, this uses 10 of them.
    , depthD = SNat @(Div (10 * 16 * 1024) 4) -- One RAMB18E2 is 16KB, this uses 10 of them.
    , initI = Nothing
    , initD = Nothing
    , iBusTimeout = d0
    , dBusTimeout = d0
    , includeIlaWb = False
    }

mkUserCore :: UserCoreCircuit UserCoreBusses (NmuRemBusWidth UserCoreBusses)
mkUserCore _bitClk _bitRst _bitEna _localCounter _maybeDna =
  circuit $ \(emptyUserCoreBusses, _rxs2Raw, handshakeOut) -> do
    [] <- idC -< emptyUserCoreBusses
    idC -< handshakeOut
