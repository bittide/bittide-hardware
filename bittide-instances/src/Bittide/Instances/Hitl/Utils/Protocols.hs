-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Hitl.Utils.Protocols where

import Clash.Explicit.Prelude
import Clash.Prelude.Extra (applyN)
import Protocols

import qualified Protocols.Vec as Vec

delayCSignal ::
  forall dom a n.
  (KnownDomain dom, NFDataX a, KnownNat n) =>
  SNat n ->
  Clock dom ->
  Circuit (CSignal dom a) (CSignal dom a)
delayCSignal n clk = applyC (applyN (snatToNum n) (dflipflop clk)) id

delayLinks ::
  forall dom a n m.
  (KnownDomain dom, NFDataX a, KnownNat n, KnownNat m) =>
  SNat n ->
  Vec m (Clock dom) ->
  Circuit (Vec m (CSignal dom a)) (Vec m (CSignal dom a))
delayLinks n clks = Vec.vecCircuits $ fmap (delayCSignal n) clks
