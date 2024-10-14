-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Bittide.CircuitUtils where

import Clash.Explicit.Prelude

import Protocols

csDupe ::
  forall dom a n.
  (KnownDomain dom, KnownNat n) =>
  Circuit (CSignal dom a) (Vec n (CSignal dom a))
csDupe = Circuit $ \(m, _) -> (pure (), repeat m)

cSigMap ::
  forall dom a b.
  (KnownDomain dom) =>
  (a -> b) ->
  Circuit (CSignal dom a) (CSignal dom b)
cSigMap fn = Circuit $ \(m, _) -> (pure (), fn <$> m)
