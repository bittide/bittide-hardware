-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Clash.Explicit.Signal.Extra where

import Clash.Explicit.Prelude

-- | Give a pulse whenever the signal changes.
changepoints ::
  forall dom a.
  (KnownDomain dom, NFDataX a, Eq a) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  Signal dom a ->
  Signal dom Bool
changepoints clk rst ena =
  mealy clk rst ena (\s i -> (Just i, maybe False (/= i) s)) Nothing
