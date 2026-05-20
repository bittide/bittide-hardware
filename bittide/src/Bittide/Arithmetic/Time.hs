-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module Bittide.Arithmetic.Time where

import Clash.Explicit.Prelude hiding (PeriodToCycles)
import GHC.Stack (HasCallStack)

import Clash.Class.Counter (Counter, countSucc)
import Data.Data (Proxy (..))
import Data.Kind (Type)

{- | XXX: We currently retain this in favor of @clash-prelude@s 'PeriodToCycles'
until @1 <= DomainPeriod dom@ is trivially true. Related issue:
https://github.com/clash-lang/ghc-typelits-extra/issues/56

Number of clock cycles required at the clock frequency of @dom@ before a minimum @period@ has passed.
Is always at least one.
-}
type PeriodToCycles dom period = Max 1 (DivRU period (Max 1 (DomainPeriod dom)))

{- | 'Index' with its 'maxBound' corresponding to the number of cycles needed to
wait for /n/ milliseconds.
-}
type IndexMs dom n = Index (PeriodToCycles dom (Milliseconds n))

{- | Rises after the incoming signal has been 'True' for the specified amount of
time. Use this function if you know the time to wait for at compile time. If
not, use 'trueForSteps'.
-}
trueFor ::
  forall dom t.
  (HasCallStack) =>
  (KnownDomain dom, KnownNat t) =>
  {- | Use the type aliases of 'Bittide.Arithmetic.Time' for time span
  specification.
  -}
  SNat t ->
  Clock dom ->
  Reset dom ->
  Signal dom Bool ->
  Signal dom Bool
trueFor _ clk rst =
  moore
    clk
    rst
    enableGen
    transF
    (== maxBound)
    (0 :: Index (PeriodToCycles dom t))
 where
  transF counter = \case
    True -> satSucc SatBound counter
    _ -> 0

{- | Rises after the incoming signal has been 'True' for the specified amount of
time given as a configurable number of steps of a given step size (i.e., wait
for @stepSize * numberOfSteps@)). Example invocation:

> trueForSteps @(Milliseconds 1) Proxy someLimit clk rst signal

which will wait for @someLimit@ milliseconds. Use 'trueFor' if you know the
time to wait for at compile time. If not, use 'trueForSteps'.
-}
trueForSteps ::
  forall (stepSize :: Nat) (dom :: Domain) (counter :: Type).
  ( HasCallStack
  , KnownDomain dom
  , KnownNat stepSize
  , NFDataX counter
  , Bounded counter
  , Counter counter
  , Eq counter
  , Num counter
  ) =>
  {- | Step size. Use the type aliases of 'Bittide.Arithmetic.Time' for time span
  specification.
  -}
  Proxy stepSize ->
  -- | Number of steps to wait for
  counter ->
  Clock dom ->
  Reset dom ->
  Signal dom Bool ->
  Signal dom Bool
trueForSteps _ limit clk rst =
  moore
    clk
    rst
    enableGen
    transF
    ((== limit) . fst)
    (0, 0 :: Index (PeriodToCycles dom stepSize))
 where
  transF cntr@(ms, _) = \case
    True
      | ms == limit -> cntr
      | otherwise -> countSucc cntr
    _ -> minBound
