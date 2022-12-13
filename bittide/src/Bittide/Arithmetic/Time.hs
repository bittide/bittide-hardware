-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Arithmetic.Time where

import Clash.Explicit.Prelude

import Clash.Signal.Internal (Femtoseconds (Femtoseconds), mapFemtoseconds)
import Data.Int (Int64)

-- | Gets time in 'Picoseconds' from time in 'Seconds'.
type Seconds      (s  :: Nat) = Milliseconds (1000 * s)
-- | Gets time in 'Picoseconds' from time in 'Milliseconds'.
type Milliseconds (ms :: Nat) = Microseconds (1000 * ms)
-- | Gets time in 'Picoseconds' from time in 'Microseconds'.
type Microseconds (us :: Nat) = Nanoseconds  (1000 * us)
-- | Gets time in 'Picoseconds' from time in 'Nanoseconds'.
type Nanoseconds  (ns :: Nat) = Picoseconds  (1000 * ns)
-- | Gets time in 'Picoseconds' from time in 'Picoseconds', essentially 'id'.
type Picoseconds  (ps :: Nat) = ps

-- | Number of clock cycles required at the clock frequency of @dom@ before a minimum @period@ has passed.
-- Is always at least one.
type PeriodCycles dom period = Max 1 (DivRU period (Max 1 (DomainPeriod dom)))

-- | Number of clock cycles at the clock frequency of @dom@ before a minimum of half @period@
-- has passed. Will always be at least one, so the resulting period is always at least
-- twice the period of @dom@.
type HalfPeriodCycles dom period = Max 1 (DivRU period (Max 1 (2 * DomainPeriod dom)))

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
