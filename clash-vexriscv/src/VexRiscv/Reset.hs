-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RoleAnnotations #-}

{- | This module contains definitions and utility for VexriscV's reset input.
The reset is required to be asserted for at least two cycles. Otherwise
the CPU will be in an undefined state.
-}
module VexRiscv.Reset (
  MinCyclesReset, -- We dont export the constructor to enforce usage of the safe functions
  fromMinCycles,
  toMinCycles,
  unsafeToMinCycles,
  convert,
  extend,
  fromExtended,
  or,
  unsafeOr,
) where

import Clash.Explicit.Prelude hiding (extend, or)
import qualified Clash.Explicit.Prelude as CE

-- | Data type that contains a reset that is asserted for at least @n@ cycles.
data MinCyclesReset (dom :: Domain) (n :: Nat) = MinCyclesReset (Reset dom)

type role MinCyclesReset nominal nominal

-- | Extract the underlying reset from a 'MinCyclesReset', this discards the minimum cycles information.
fromMinCycles :: MinCyclesReset dom n -> Reset dom
fromMinCycles (MinCyclesReset rst) = rst

{- | Convert a regular reset into a 'MinCyclesReset'. If the resulting minimum cycle count is higher than 1,
`holdReset` is used to extend the reset duration to match the new minimum cycle count.
-}
toMinCycles ::
  forall n dom.
  (KnownDomain dom, KnownNat n) =>
  Clock dom ->
  Reset dom ->
  MinCyclesReset dom n
toMinCycles clk rst = convert clk (MinCyclesReset @_ @1 rst)

{- | Convert a regular reset into a 'MinCyclesReset'.

This function is unsafe because it does not insert any logic to ensure the reset is actually
asserted for the required number of cycles. The caller must ensure that the provided reset meets
this requirement.
-}
unsafeToMinCycles ::
  forall n dom.
  (KnownDomain dom) =>
  Reset dom ->
  MinCyclesReset dom n
unsafeToMinCycles = MinCyclesReset

{- | Coerces a 'MinCyclesReset' into another 'MinCyclesReset' with a different minimum cycle count.
If the resulting minimum cycle count is higher than the original, `holdReset` is used to extend the reset duration.
to match the new minimum cycle count.

If the resulting minimum cycle count is lower than the original, the reset is returned as-is.
This is safe because the original reset already satisfies the higher minimum cycle requirement.
-}
convert ::
  forall n m dom.
  (KnownDomain dom, KnownNat n, KnownNat m) =>
  Clock dom ->
  MinCyclesReset dom n ->
  MinCyclesReset dom m
convert clk (MinCyclesReset rst) = MinCyclesReset $ case compareSNat (SNat @(n + 1)) (SNat @m) of
  SNatLE -> holdReset clk enableGen (SNat @(m - n + 1)) rst
  SNatGT -> rst

{- | Extends a 'MinCyclesReset' to a new 'MinCyclesReset' with a higher minimum cycle count.
This is done by using `holdReset` to ensure the reset is asserted for the required number of cycles.
-}
extend ::
  forall n m dom.
  (KnownDomain dom, KnownNat n, KnownNat m, n <= m) =>
  Clock dom ->
  MinCyclesReset dom n ->
  MinCyclesReset dom m
extend = convert

{- | Converts a 'MinCyclesReset' with a higher minimum cycle count to one with a lower minimum cycle count.
This does not insert any logic and the resulting reset will still be asserted for the higher minimum cycle count.
This is safe because the original reset already satisfies the higher minimum cycle requirement.
-}
fromExtended ::
  forall n m dom.
  (KnownNat n, KnownNat m, n <= m) =>
  MinCyclesReset dom m ->
  MinCyclesReset dom n
fromExtended (MinCyclesReset rst) = MinCyclesReset rst

{- | Combine two 'MinCyclesReset's into one that is asserted for at least
the maximum of the two input minimum cycles.
-}
or ::
  forall n m dom.
  (KnownDomain dom, HasSynchronousReset dom, KnownNat n, KnownNat m) =>
  Clock dom ->
  MinCyclesReset dom n ->
  MinCyclesReset dom m ->
  MinCyclesReset dom (Max n m)
or = unsafeOr

{- | Unsafely combine two 'MinCyclesReset's into one that is asserted for at least
the maximum of the two input minimum cycles.
-}
unsafeOr ::
  forall n m dom.
  (KnownDomain dom, KnownNat n, KnownNat m) =>
  Clock dom ->
  MinCyclesReset dom n ->
  MinCyclesReset dom m ->
  MinCyclesReset dom (Max n m)
unsafeOr clk rstL0 rstR0 =
  MinCyclesReset
    $ CE.unsafeOrReset rstL1 rstR1
 where
  rstL1 = fromMinCycles $ convert @n @(Max n m) clk rstL0
  rstR1 = fromMinCycles $ convert @m @(Max n m) clk rstR0
