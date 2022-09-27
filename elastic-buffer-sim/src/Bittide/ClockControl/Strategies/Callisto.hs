-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}

module Bittide.ClockControl.Strategies.Callisto
  ( callisto
  )
where

import Clash.Explicit.Prelude

import Bittide.ClockControl

data ControlSt = ControlSt
  { x_k :: Float -- ^ 'x_k' is the integral of the measurement
  , z_k :: Integer
  , b_k :: SpeedChange
  } deriving (Generic, NFDataX)

initControlSt :: ControlSt
initControlSt = ControlSt 0 0 NoChange

callisto ::
  forall n dom.
  (KnownDomain dom, KnownNat n, 1 <= n) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  ClockControlConfig ->
  Signal dom (Vec n DataCount) ->
  Signal dom SpeedChange
callisto clk rst ena ClockControlConfig{..} =
  mealy clk rst ena go (initControlSt, 0)
 where
  go (ControlSt{..}, settleCounter) dataCounts
    | settleCounter > cccSettlePeriod
    = ((ControlSt x_kNext z_kNext b_kNext, 0), b_kNext)
   where

    -- see clock control algorithm simulation here:
    -- https://github.com/bittide/Callisto.jl/blob/e47139fca128995e2e64b2be935ad588f6d4f9fb/demo/pulsecontrol.jl#L24
    --
    -- the constants here are chosen to match the above code.
    k_p = 2e-4 :: Float
    k_i = 1e-11 :: Float
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

  go (st, settleCounter) _ =
    ((st, settleCounter + cccPessimisticPeriod), NoChange)
