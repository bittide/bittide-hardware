-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}

module Bittide.ClockControl.Strategies.Callisto
  ( callisto
  , initControlSt
  )
where

import Clash.Explicit.Prelude
import Clash.Signal.Internal

import Data.Bifunctor (second)

import Bittide.ClockControl

data ControlSt = ControlSt
  { x_k :: Double -- ^ 'x_k' is the integral of the measurement
  , z_k :: Integer
  , b_k :: SpeedChange
  }

initControlSt :: ControlSt
initControlSt = ControlSt 0 0 NoChange

callisto ::
  forall n dom.
  (KnownNat n, 1 <= n) =>
  ClockControlConfig ->
  SettlePeriod ->
  ControlSt ->
  Signal dom (Vec n DataCount) ->
  (ControlSt, Signal dom SpeedChange)
callisto
  cfg@ClockControlConfig{..} settleCounter ControlSt{..} (dataCounts :- nextDataCounts)
  | settleCounter > cccSettlePeriod
  = second (b_kNext :-) nextChanges
 where

  -- see clock control algorithm simulation here:
  -- https://github.com/bittide/Callisto.jl/blob/e47139fca128995e2e64b2be935ad588f6d4f9fb/demo/pulsecontrol.jl#L24
  --
  -- the constants here are chosen to match the above code.
  k_p = 2e-4 :: Double
  k_i = 1e-11 :: Double
  r_k =
    let
      measuredSum = realToFrac (sum dataCounts)
      targetCount = realToFrac (targetDataCount cccBufferSize)
      nBuffers = realToFrac (length dataCounts)
    in 
      measuredSum - targetCount * nBuffers
  x_kNext =
    x_k + p * r_k

  c_des = k_p * r_k + k_i * realToFrac x_kNext
  z_kNext = z_k + sgn b_k
  fStep = 5e-4
  c_est = fStep * realToFrac z_kNext
  -- we are using 200kHz instead of 200MHz
  -- (see https://github.com/clash-lang/clash-compiler/issues/2328)
  -- so typical freq. is 0.0002 GHz
  --
  -- (this is adjusted by a factor of 100 because our clock corrections are
  -- faster than those simulated in Callisto; we correct as often as hardware
  -- allows)
  p = 1e3 * typicalFreq where typicalFreq = 0.0002

  b_kNext =
    case compare c_des c_est of
      LT -> SlowDown
      GT -> SpeedUp
      EQ -> NoChange

  sgn NoChange = 0
  sgn SpeedUp = 1
  sgn SlowDown = -1

  nextChanges =
    callisto cfg 0 (ControlSt x_kNext z_kNext b_kNext) nextDataCounts

callisto cfg@ClockControlConfig{..} settleCounter st (_ :- nextDataCounts) =
  second (NoChange :-) nextChanges
 where
  nextChanges = callisto cfg newSettleCounter st nextDataCounts
  newSettleCounter = settleCounter + cccPessimisticPeriod
