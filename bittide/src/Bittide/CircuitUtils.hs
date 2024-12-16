-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Bittide.CircuitUtils where

import Clash.Explicit.Prelude

import Protocols

cSignalDupe ::
  forall dom a n.
  (KnownDomain dom, KnownNat n) =>
  Circuit (CSignal dom a) (Vec n (CSignal dom a))
cSignalDupe = Circuit $ \(m, _) -> (pure (), repeat m)

cSignalMap ::
  forall dom a b.
  (KnownDomain dom) =>
  (a -> b) ->
  Circuit (CSignal dom a) (CSignal dom b)
cSignalMap fn = Circuit $ \(m, _) -> (pure (), fn <$> m)
