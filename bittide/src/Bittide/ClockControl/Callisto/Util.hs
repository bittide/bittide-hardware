-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Collection of helpers used in 'Bittide.ClockControl.Callisto.callisto'.
module Bittide.ClockControl.Callisto.Util where

import Clash.Prelude

import Bittide.ClockControl (RelDataCount, SpeedChange (..))

{- | Safe 'RelDataCount' to 'Signed' conversion.
(works for both: 'RelDataCount' being 'Signed' or 'Unsigned')
-}
dataCountToSigned ::
  forall n.
  (KnownNat n) =>
  RelDataCount n ->
  Signed (n + 1)
dataCountToSigned = bitCoerce . extend

{- | A counter that starts at a given value, counts down, and if it reaches
zero wraps around to the initial value.
-}
wrappingCounter ::
  (HiddenClockResetEnable dom, KnownNat n) =>
  Unsigned n ->
  Signal dom (Unsigned n)
wrappingCounter upper = counter
 where
  counter = register upper (go <$> counter)

  go 0 = upper
  go n = pred n

{- | A version of 'sum' that is guaranteed not to overflow.
(works for both: 'RelDataCount' being 'Signed' or 'Unsigned')
-}
safeSum ::
  ( KnownNat n
  , KnownNat m
  , 1 <= n
  ) =>
  Vec n (RelDataCount m) ->
  RelDataCount (m + n - 1)
safeSum = sum . map extend

{- | Sum a bunch of 'RelDataCount's to a @Signed 32@, without overflowing.
(works for both: 'RelDataCount' being 'Signed' or 'Unsigned')
-}
sumTo32 ::
  forall n m.
  ( KnownNat m
  , KnownNat n
  , (m + n) <= 32
  , 1 <= n
  ) =>
  Vec n (RelDataCount m) ->
  Signed 32
sumTo32 =
  extend @_ @_ @(32 - (m + n))
    . dataCountToSigned
    . safeSum

-- | Counts the number of 'high' bits in a bitvector.
safePopCountTo32 ::
  forall n.
  ( KnownNat n
  , (1 + n) <= 32
  , 1 <= n
  ) =>
  BitVector n ->
  Signed 32
safePopCountTo32 =
  sumTo32 . unpack @(Vec n (RelDataCount 1))

type FINC = Bool
type FDEC = Bool

speedChangeToPins :: SpeedChange -> (FINC, FDEC)
speedChangeToPins = \case
  SpeedUp -> (True, False)
  SlowDown -> (False, True)
  NoChange -> (False, False)

{- | Holds any @a@ which has any bits set for @stickyCycles@ clock cycles.
On receiving a new @a@ with non-zero bits, it sets the new incoming value as it output
and holds it for @stickyCycles@ clock cycles.
-}
stickyBits ::
  forall dom stickyCycles a.
  ( HiddenClockResetEnable dom
  , NFDataX a
  , BitPack a
  , 1 <= stickyCycles
  ) =>
  SNat stickyCycles ->
  Signal dom a ->
  Signal dom a
stickyBits SNat = mealy go (0, unpack 0)
 where
  go :: (Index stickyCycles, a) -> a -> ((Index stickyCycles, a), a)
  go (count, storedBits) incomingBits = ((nextCount, nextStored), storedBits)
   where
    newIncoming = pack incomingBits /= 0
    predCount = satPred SatZero count
    holdingBits = count /= 0
    (nextStored, nextCount)
      | newIncoming = (incomingBits, maxBound)
      | holdingBits = (storedBits, predCount)
      | otherwise = (unpack 0, predCount)
