-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- |
A small Wishbone peripheral that releases a reset at a software-chosen cycle.

It holds its output reset asserted until the supplied local counter exceeds the
value in its single @release_cycle@ register, then deasserts it. The register is
written by the management unit (over the bus / via GDB). Because the local counter
is monotonic, once the reset is deasserted it stays deasserted.

This is used for UGN-grooming relabeling: by choosing, per FPGA, the local-counter
value at which the application (user core) comes out of reset, each node's
application counter starts at a chosen origin — effectively relabeling the counters
to remove boot-time-induced offsets. See "Bittide.ClockControl.Ugn.Grooming".

The @release_cycle@ register defaults to 'maxBound', so the reset stays asserted
until the management unit writes a (reachable) value — which it should only do once
clock control is stable.
-}
module Bittide.TimedReset (timedResetWb) where

import Clash.Prelude
import Protocols

import Clash.Class.BitPackC (ByteOrder)
import GHC.Stack (HasCallStack)
import Protocols.Experimental.Wishbone (Wishbone, WishboneMode (Standard))
import Protocols.MemoryMap (Access (ReadWrite), Mm)
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access, description),
  deviceConfig,
  deviceWbI,
  registerConfig,
  registerWbI,
 )

{- | Timed reset peripheral. Deasserts its output reset once @localCounter@ is
greater than the value written to its @release_cycle@ register.
-}
timedResetWb ::
  forall dom addrW nBytes.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , KnownNat addrW
  , KnownNat nBytes
  , 1 <= nBytes
  , ?byteOrder :: ByteOrder
  ) =>
  -- | Local counter
  Signal dom (Unsigned 64) ->
  Circuit
    (ToConstBwd Mm, Wishbone dom 'Standard addrW nBytes)
    ("RESET" ::: Reset dom)
timedResetWb localCounter = circuit $ \bus -> do
  [wbReleaseCycle] <- deviceWbI (deviceConfig "TimedReset") -< bus

  (Fwd releaseCycle, _releaseCycleActivity) <-
    registerWbI
      (registerConfig "release_cycle")
        { access = ReadWrite
        , description =
            "Local-counter value at which the output reset is released. While the\
            \ local counter is at or below this value the reset stays asserted; once\
            \ it exceeds it the reset deasserts and (being a monotonic counter) stays\
            \ deasserted. The management unit writes this to choose when the\
            \ application leaves reset (the UGN-grooming relabel). Defaults to\
            \ maxBound, i.e. held until written."
        }
      (maxBound :: Unsigned 64)
      -< (wbReleaseCycle, Fwd (pure Nothing))

  let reset = unsafeFromActiveLow (localCounter .>. releaseCycle)

  idC -< Fwd reset
