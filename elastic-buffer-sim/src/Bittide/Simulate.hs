{-|

Provides a rudimentary simulation of elastic buffers.

-}

-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}

module Bittide.Simulate where

import Clash.Prelude
import Clash.Signal.Internal
import Data.Proxy
import GHC.Stack

import Bittide.ClockControl

-- Number of type aliases for documentation purposes in various functions defined
-- down below.
type Offset = Femtoseconds
type StepSize = Femtoseconds
type Period = Femtoseconds

addFs :: Femtoseconds -> Femtoseconds -> Femtoseconds
addFs (Femtoseconds a) (Femtoseconds b) = Femtoseconds (a + b)

subFsZero :: Femtoseconds -> Femtoseconds -> Femtoseconds
subFsZero (Femtoseconds a) (Femtoseconds b)
  | aMinB < 0 = Femtoseconds 0
  | otherwise = Femtoseconds aMinB
 where
  aMinB = a - b

--
-- TODO:
--
--   * Reset adjustment to zero after reset assertion
--
tunableClockGen ::
  forall dom.
  (HasCallStack, KnownDomain dom) =>
  -- | Period it takes for a clock frequency request to settle. This is not
  -- modelled, but an error is thrown if a request is submitted more often than
  -- this.
  SettlePeriod ->
  -- | Offset from the ideal period (encoded in the domain) of this clock. For
  -- the Si5395/Si5391 oscillators, this value lies between ±100 ppm.
  Offset ->
  -- | The size of the clock frequency should "jump" on a speed change request.
  StepSize ->
  -- | When asserted, clock multiplier resets the outgoing clock to its original
  -- frequency. TODO: Implement.
  Reset dom ->
  -- | Speed change request. After submitting 'SpeedUp'/'SpeedDown', caller
  -- shouldn't submit another request for 1 microsecond (i.e., the clock tuner
  -- effectively operates at 1 MHz).
  --
  -- TODO: For the actual boards this needs to be modelled as a pulse. This pulse
  -- should be asserted for at least 100 ns and at a maximum rate of 1 MHz.
  --
  Signal dom SpeedChange ->
  -- | Clock with a dynamic frequency. At the time of writing, Clash primitives
  -- don't account for this yet, so be careful when using them. Note that dynamic
  -- frequencies are only relevant for components handling multiple domains.
  Clock dom
tunableClockGen settlePeriod periodOffset stepSize _reset speedChange =
  let
    period = clockPeriodFs @dom Proxy
    initPeriod = period `addFs` periodOffset
    clockSignal = initPeriod :- go settlePeriod initPeriod speedChange
  in
    Clock SSymbol (Just clockSignal)
 where
  go ::
    Femtoseconds ->
    Period ->
    Signal dom SpeedChange ->
    Signal dom StepSize
  go !settleCounter !period (sc :- scs) =
    let
      vars =
           "settlePeriod: " <> show settlePeriod <> ", "
        <> "settleCounter: " <> show settleCounter <> ", "
        <> "period: " <> show period <> ", "

      (newSettleCounter, newPeriod) = case sc of
        SpeedUp
          | settleCounter >= settlePeriod -> (Femtoseconds 0, period `subFsZero` stepSize)
          | otherwise -> error $ "tunableClockGen: frequency change requested too often. " <> vars
        SlowDown
          | settleCounter >= settlePeriod -> (Femtoseconds 0, period `addFs` stepSize)
          | otherwise -> error $ "tunableClockGen: frequency change requested too often. " <> vars
        NoChange ->
          (settleCounter `addFs` period, period)
    in
      newPeriod :- go newSettleCounter newPeriod scs
