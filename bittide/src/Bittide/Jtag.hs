-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedRecordDot #-}

module Bittide.Jtag where

import Clash.Explicit.Prelude
import Protocols

import Clash.Functor.Extra ((<<$>>))
import Data.Constraint.Nat.Extra (Dict (..))
import Data.Constraint.Nat.Lemmas (nLe0Eq0)
import VexRiscv (Jtag, JtagIn (..), JtagOut (..))

{- | Form a chain of JTAG devices. The first device in the chain is the one with
the lowest index in the vector. If the vector is empty, the circuit loops back
TDI to TDO.

Note: the 'debugReset' is ignored.
-}
jtagChain ::
  forall dom n.
  (KnownDomain dom, KnownNat n) =>
  Circuit (Jtag dom) (Vec n (Jtag dom))
jtagChain =
  case SNat @n `compareSNat` d0 of
    SNatLE -> Circuit (case nLe0Eq0 @n of Dict -> go0)
    SNatGT -> Circuit (leToPlus @1 @n go)
 where
  go0 ::
    (Signal dom JtagIn, Vec 0 (Signal dom JtagOut)) ->
    (Signal dom JtagOut, Vec 0 (Signal dom JtagIn))
  go0 (fwd, _) = (toBwd <$> fwd, Nil)
   where
    toBwd (JtagIn{testDataIn}) = JtagOut{testDataOut = testDataIn}

  go ::
    forall m.
    (KnownNat m) =>
    (Signal dom JtagIn, Vec (m + 1) (Signal dom JtagOut)) ->
    (Signal dom JtagOut, Vec (m + 1) (Signal dom JtagIn))
  go (fwd, bwds) = (bwd, fwds)
   where
    tcks = repeat $ (.testClock) <$> fwd
    tmss = repeat $ (.testModeSelect) <$> fwd
    tdis = ((.testDataIn) <$> fwd) :> ((.testDataOut) <<$>> init bwds)
    fwds = zipWith3 (liftA3 JtagIn) tcks tmss tdis
    bwd = (\jtagOut -> jtagOut) <$> last bwds

{- | 'unsafeSynchronizer', but for JTAG signals. Does not insert any synchronization
elements.

Note: we currently use this because we encode the JTAG signals on the same domain
as the CPU core. This is not ideal, but it is a limitation of the current
@clash-vexriscv@ implementation.
-}
unsafeJtagSynchronizer ::
  forall dom1 dom2.
  (KnownDomain dom1, KnownDomain dom2) =>
  Clock dom1 ->
  Clock dom2 ->
  Circuit (Jtag dom1) (Jtag dom2)
unsafeJtagSynchronizer clk1 clk2 = Circuit go
 where
  go ::
    (Signal dom1 JtagIn, Signal dom2 JtagOut) ->
    (Signal dom1 JtagOut, Signal dom2 JtagIn)
  go (jtagIn1, jtagOut2) = (jtagOut1, jtagIn2)
   where
    jtagOut1 = unsafeSynchronizer clk2 clk1 jtagOut2
    jtagIn2 = unsafeSynchronizer clk1 clk2 jtagIn1
