-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NoImplicitPrelude #-}

module Tests.ElasticBuffer.ClockSync ( entangledEna ) where

import Clash.Explicit.Prelude

-- | This processes our variable "period" and turns it into something we can
-- pretend is an enable
blink ::
  (KnownDomain dom, Num a, Eq a, NFDataX a) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  Signal dom a ->
  -- | Cycle length
  Signal dom Bool
blink clk rst ena = mealy clk rst ena go (0, False)
 where
  go (0, b) m = ((m, not b), b)
  go (n, b) _ = ((n-1, b), b)

data Adjust = GoFaster | SlowDown | Stable

data VarClkState a = Ready | Wait a deriving (Generic, NFDataX)

-- | Output a period based on internal state, adjusting based on feedback
varClk
  :: (KnownDomain dom, Num a, Eq a, NFDataX a) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  a ->
  -- | Initial period
  Signal dom Adjust ->
  Signal dom a
  -- | Cycle length
varClk clk rst ena t = mealy clk rst ena s (t, Ready)
 where
  s (n, Ready) GoFaster = ((n+1, Wait$ n+1), n+1)
  s (n, Ready) SlowDown = ((n-1, Wait$ n-1), n-1)
  s (n, Ready) Stable = ((n, Ready), n)
  s (n, Wait 0) _ = ((n, Ready), n)
  s (n, Wait m) _ = ((n, Wait$m-1), n)

clockSynchronizer ::
  (KnownDomain dom, Num a, Eq a, Ord a, NFDataX a) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  a ->
  -- | Initial period
  a ->
  (Signal dom a, Signal dom a)
  -- | Signals end up being the same after some time
  --
  -- The output is the (time-varying) period.
clockSynchronizer clk rst ena t0 t1 =
  (clkA, clkB)
 where
  (adjI, adjO) = unbundle $ moore clk rst ena pF output (t0, t1) (bundle (clkA', clkB'))
   where
    pF _ i = i
    output (t0', t1') =
      case compare t0' t1' of
        EQ -> (Stable, Stable)
        GT -> (SlowDown, GoFaster)
        LT -> (GoFaster, SlowDown)

  clkA = varClk clk rst ena t0 adjI
  clkB = varClk clk rst ena t1 adjO

  clkA' = register clk rst ena t0 clkA
  clkB' = register clk rst ena t1 clkB

entangledEna ::
  (KnownDomain dom, Num a, Eq a, Ord a, NFDataX a) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  a ->
  -- | Initial period
  a ->
  -- | Initial period
  (Signal dom Bool, Signal dom Bool) -- TODO: coerce domains?
entangledEna clk rst ena t0 t1 = onBoth blinkDom $ clockSynchronizer clk rst ena t0 t1
 where
  blinkDom = blink clk rst ena

onBoth :: (a -> b) -> (a, a) -> (b, b)
onBoth f (x, y) = (f x, f y)
