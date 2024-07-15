-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses,TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Bittide.Arithmetic.Time where

import GHC.Stack (HasCallStack)
import Clash.Explicit.Prelude hiding (natVal)

import Clash.Class.Counter (countSucc, Counter)
import Clash.Signal.Internal (Femtoseconds (Femtoseconds), mapFemtoseconds)
import Data.Data (Proxy(Proxy))
import Data.Int (Int64)
import Data.Kind (Type)

import GHC.TypeNats (natVal)
import GHC.TypeLits.KnownNat (KnownNat1 (..),SNatKn(..), nameToSymbol)

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

-- Make ghc-typelits-knownnat look through the Picoseconds type alias
instance (KnownNat ps) => KnownNat1 $(nameToSymbol ''Picoseconds) ps where
  natSing1 = SNatKn (natVal (Proxy @ps))
  {-# NOINLINE natSing1 #-}

-- | Number of clock cycles required at the clock frequency of @dom@ before a minimum @period@ has passed.
-- Is always at least one.
type PeriodToCycles dom period = Max 1 (DivRU period (Max 1 (DomainPeriod dom)))

-- | The domain's clock frequency in Hertz, calculated based on the period stored in ps.
-- This might lead to rounding errors.
type DomainFrequency dom = Div (Seconds 1) (DomainPeriod dom)

-- | 'Index' with its 'maxBound' corresponding to the number of cycles needed to
-- wait for /n/ milliseconds.
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

-- | Rises after the incoming signal has been 'True' for the specified amount of
-- time. Use this function if you know the time to wait for at compile time. If
-- not, use 'trueForSteps'.
trueFor ::
  forall dom t. HasCallStack =>
  (KnownDomain dom, KnownNat t) =>
  SNat t ->
  -- ^ Use the type aliases of 'Bittide.Arithmetic.Time' for time span
  -- specification.
  Clock dom ->
  Reset dom ->
  Signal dom Bool ->
  Signal dom Bool
trueFor _ clk rst =
  moore clk rst enableGen transF (== maxBound)
    (0 :: Index (PeriodToCycles dom t))
 where
  transF counter = \case
    True -> satSucc SatBound counter
    _    -> 0

-- | Rises after the incoming signal has been 'True' for the specified amount of
-- time given as a configurable number of steps of a given step size (i.e., wait
-- for @stepSize * numberOfSteps@)). Example invocation:
--
-- > trueForSteps @(Milliseconds 1) Proxy someLimit clk rst signal
--
-- which will wait for @someLimit@ milliseconds. Use 'trueFor' if you know the
-- time to wait for at compile time. If not, use 'trueForSteps'.
trueForSteps ::
  forall (stepSize :: Nat) (dom :: Domain) (counter :: Type) .
  ( HasCallStack
  , KnownDomain dom
  , KnownNat stepSize
  , NFDataX counter, Bounded counter, Counter counter, Eq counter, Num counter
  ) =>
  Proxy stepSize ->
  -- ^ Step size. Use the type aliases of 'Bittide.Arithmetic.Time' for time span
  -- specification.
  counter ->
  -- ^ Number of steps to wait for
  Clock dom ->
  Reset dom ->
  Signal dom Bool ->
  Signal dom Bool
trueForSteps _ limit clk rst =
  moore clk rst enableGen transF ((== limit) . fst) (0, 0 :: Index (PeriodToCycles dom stepSize))
 where
  transF cntr@(ms, _) = \case
    True
      | ms == limit -> cntr
      | otherwise   -> countSucc cntr
    _    -> minBound
