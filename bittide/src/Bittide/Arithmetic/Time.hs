-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Bittide.Arithmetic.Time where

import Clash.Explicit.Prelude hiding (PeriodToCycles, natVal)
import GHC.Stack (HasCallStack)

import Clash.Class.Counter (Counter, countSucc)
import Clash.Signal.Internal (Femtoseconds (Femtoseconds), mapFemtoseconds)
import Data.Data (Proxy (..))
import Data.Int (Int64)
import Data.Kind (Type)

import GHC.TypeLits.KnownNat (KnownNat1 (..), SNatKn (..), nameToSymbol)
import GHC.TypeNats (natVal)

{- | XXX: We currently retain this in favor of @clash-prelude@s 'PeriodToCycles'
until @1 <= DomainPeriod dom@ is trivially true. Related issue:
https://github.com/clash-lang/ghc-typelits-extra/issues/56

Number of clock cycles required at the clock frequency of @dom@ before a minimum @period@ has passed.
Is always at least one.
-}
type PeriodToCycles dom period = Max 1 (DivRU period (Max 1 (DomainPeriod dom)))

-- Make ghc-typelits-knownnat look through time related type aliases.
-- https://github.com/clash-lang/ghc-typelits-knownnat/issues/53
instance (KnownNat ps) => KnownNat1 $(nameToSymbol ''Picoseconds) ps where
  natSing1 = SNatKn (natVal (Proxy @ps))
  {-# NOINLINE natSing1 #-}

instance (KnownNat ps) => KnownNat1 $(nameToSymbol ''Nanoseconds) ps where
  natSing1 = SNatKn (natVal (Proxy @(1_000 * ps)))
  {-# NOINLINE natSing1 #-}

instance (KnownNat ps) => KnownNat1 $(nameToSymbol ''Microseconds) ps where
  natSing1 = SNatKn (natVal (Proxy @(1_000_000 * ps)))
  {-# NOINLINE natSing1 #-}

instance (KnownNat ps) => KnownNat1 $(nameToSymbol ''Milliseconds) ps where
  natSing1 = SNatKn (natVal (Proxy @(1_000_000_000 * ps)))
  {-# NOINLINE natSing1 #-}

instance (KnownNat ps) => KnownNat1 $(nameToSymbol ''Seconds) ps where
  natSing1 = SNatKn (natVal (Proxy @(1_000_000_000_000 * ps)))
  {-# NOINLINE natSing1 #-}

{- | 'Index' with its 'maxBound' corresponding to the number of cycles needed to
wait for /n/ milliseconds.
-}
type IndexMs dom n = Index (PeriodToCycles dom (Milliseconds n))

seconds :: Int64 -> Femtoseconds
seconds s = mapFemtoseconds (* 1000) (milliseconds s)

milliseconds :: Int64 -> Femtoseconds
milliseconds s = mapFemtoseconds (* 1000) (microseconds s)
{-# INLINE milliseconds #-}

microseconds :: Int64 -> Femtoseconds
microseconds s = mapFemtoseconds (* 1000) (nanoseconds s)
{-# INLINE microseconds #-}

nanoseconds :: Int64 -> Femtoseconds
nanoseconds s = mapFemtoseconds (* 1000) (picoseconds s)
{-# INLINE nanoseconds #-}

picoseconds :: Int64 -> Femtoseconds
picoseconds s = mapFemtoseconds (* 1000) (femtoseconds s)
{-# INLINE picoseconds #-}

femtoseconds :: Int64 -> Femtoseconds
femtoseconds = Femtoseconds
{-# INLINE femtoseconds #-}

{- | Rises after the incoming signal has been 'True' for the specified amount of
time. Use this function if you know the time to wait for at compile time. If
not, use 'trueForSteps'.
-}
trueFor ::
  forall dom t.
  (HasCallStack) =>
  (KnownDomain dom, KnownNat t) =>
  -- | Use the type aliases of 'Bittide.Arithmetic.Time' for time span
  -- specification.
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
  -- | Step size. Use the type aliases of 'Bittide.Arithmetic.Time' for time span
  -- specification.
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
