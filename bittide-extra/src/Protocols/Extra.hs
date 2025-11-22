-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Many of these functions should be added to `clash-protocols`, there is a PR
for this: https://github.com/clash-lang/clash-protocols/pull/116
And a bittide-hardware issue:
https://github.com/bittide/bittide-hardware/issues/645
-}
module Protocols.Extra where

import Clash.Prelude

import Protocols

{- | Replicates a 'CSignal dom a' into a 'Vec n (CSignal dom a)', where the length of the
vector is determined by the context.
-}
replicateCSignalI ::
  forall dom a n.
  (KnownNat n) =>
  Circuit (CSignal dom a) (Vec n (CSignal dom a))
replicateCSignalI = applyC repeat (const $ ())

-- | Map a function over a 'Circuit' of 'CSignal's
cSignalMap ::
  forall dom a b.
  (KnownDomain dom) =>
  (a -> b) ->
  Circuit (CSignal dom a) (CSignal dom b)
cSignalMap fn = applyC (fmap fn) (const $ ())
