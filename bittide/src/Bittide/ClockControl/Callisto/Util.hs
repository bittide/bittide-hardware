-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Collection of helpers used in 'Bittide.ClockControl.Callisto.callisto'.
module Bittide.ClockControl.Callisto.Util where

import Clash.Prelude

-- | A counter that starts at a given value, counts down, and if it reaches
-- zero wraps around to the initial value.
wrappingCounter ::
  (HiddenClockResetEnable dom, KnownNat n) =>
  Unsigned n ->
  Signal dom (Unsigned n)
wrappingCounter upper = counter
 where
  counter = register upper (go <$> counter)

  go 0 = upper
  go n = pred n

-- | A version of 'sum' that is guaranteed not to overflow.
safeSum ::
  ( KnownNat n
  , KnownNat m
  , 1 <= n
  ) =>
  Vec n (Unsigned m) ->
  Unsigned (m + n - 1)
safeSum = sum . map extend

-- | Sum a bunch of 'Unsigned's to a @Signed 32@, without overflowing.
sumTo32 ::
  forall n m .
  ( KnownNat m
  , KnownNat n
  , (m + n) <= 32
  , 1 <= n
  ) =>
  Vec n (Unsigned m) ->
  Signed 32
sumTo32 =
    extend @_ @_ @(32 - (m+n))
  . unsignedToSigned
  . safeSum

-- | Safe 'Unsigned' to 'Signed' conversion
unsignedToSigned :: forall n. KnownNat n => Unsigned n -> Signed (n + 1)
unsignedToSigned n = bitCoerce (zeroExtend n)
