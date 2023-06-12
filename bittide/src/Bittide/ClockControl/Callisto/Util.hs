-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Collection of helpers used in 'Bittide.ClockControl.Callisto.callisto'.
module Bittide.ClockControl.Callisto.Util where

import Clash.Prelude

import Bittide.ClockControl (DataCount)

-- | Safe 'DataCount' to 'Signed' conversion.
-- (works for both: 'DataCount' being 'Signed' or 'Unsigned')
dataCountToSigned ::
  forall n .
  KnownNat n =>
  DataCount n ->
  Signed (n + 1)
dataCountToSigned = bitCoerce . extend

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
-- (works for both: 'DataCount' being 'Signed' or 'Unsigned')
safeSum ::
  ( KnownNat n
  , KnownNat m
  , 1 <= n
  ) =>
  Vec n (DataCount m) ->
  DataCount (m + n - 1)
safeSum = sum . map extend

-- | Sum a bunch of 'DataCount's to a @Signed 32@, without overflowing.
-- (works for both: 'DataCount' being 'Signed' or 'Unsigned')
sumTo32 ::
  forall n m .
  ( KnownNat m
  , KnownNat n
  , (m + n) <= 32
  , 1 <= n
  ) =>
  Vec n (DataCount m) ->
  Signed 32
sumTo32 =
    extend @_ @_ @(32 - (m+n))
  . dataCountToSigned
  . safeSum

-- | Counts the number of 'high' bits in a bitvector.
safePopCountTo32 ::
  forall n .
  ( KnownNat n
  , (1 + n) <= 32
  , 1 <= n
  ) =>
  BitVector n ->
  Signed 32
safePopCountTo32 =
  sumTo32 . unpack @(Vec n (DataCount 1))
