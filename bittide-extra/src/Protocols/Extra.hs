-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Protocols.Extra where

import Clash.Prelude

import Protocols

fanoutC ::
  forall dom a n.
  (KnownNat n) =>
  Circuit (CSignal dom a) (Vec n (CSignal dom a))
fanoutC = Circuit go
 where
  go (sigFwd, _) = (pure (), repeat sigFwd)
