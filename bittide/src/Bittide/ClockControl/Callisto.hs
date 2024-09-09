-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}

module Bittide.ClockControl.Callisto (
  CallistoResult (..),
  ReframingState (..),
  callistoClockControl,
) where

import Clash.Prelude

import Data.Constraint
import Data.Constraint.Nat (leTrans)
import Data.Constraint.Nat.Extra (euclid3, useLowerLimit)

import Bittide.ClockControl
import Bittide.ClockControl.Callisto.Types
import Bittide.ClockControl.Callisto.Util
import Bittide.ClockControl.StabilityChecker
import Bittide.Extra.Maybe

import qualified Clash.Cores.Xilinx.Floating as F
import qualified Clash.Signal.Delayed as D

{-# NOINLINE callistoClockControl #-}

{- | Determines how to influence clock frequency given statistics provided by
all elastic buffers. See 'callisto' for more information.
-}
callistoClockControl ::
  forall n m dom margin framesize.
  ( KnownDomain dom
  , KnownNat n
  , KnownNat m
  , KnownNat margin
  , KnownNat framesize
  , 1 <= n
  , 1 <= m
  , n + m <= 32
  , 1 <= framesize
  ) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  -- | Configuration for this component, see individual fields for more info.
  ClockControlConfig dom m margin framesize ->
  -- | Link availability mask
  Signal dom (BitVector n) ->
  -- | Statistics provided by elastic buffers.
  Vec n (Signal dom (RelDataCount m)) ->
  Signal dom (CallistoResult n)
callistoClockControl clk rst ena ClockControlConfig{..} mask allDataCounts =
  withClockResetEnable clk rst ena
    $ let
        dataCounts = filterCounts <$> fmap bv2v mask <*> bundle allDataCounts
        updateCounter = wrappingCounter cccSettleCycles
        shouldUpdate = updateCounter .==. 0
        scs = bundle $ map stabilityCheck $ unbundle dataCounts
        allStable = allAvailable stable <$> mask <*> scs
        allSettled = allAvailable settled <$> mask <*> scs
        state = register initState state'

        state' =
          mux
            shouldUpdate
            (callisto controlConfig mask scs dataCounts state)
            state

        stabilityCheck =
          stabilityChecker
            cccStabilityCheckerMargin
            cccStabilityCheckerFramesize
       in
        CallistoResult
          <$> (orNothing <$> shouldUpdate <*> fmap _b_k state')
          <*> scs
          <*> allStable
          <*> allSettled
          <*> (rfState <$> state')
 where
  controlConfig =
    ControlConfig
      { reframingEnabled = cccEnableReframing
      , waitTime = cccReframingWaitTime
      , targetCount = targetDataCount
      }

  initState =
    ControlSt
      { _z_k = 0
      , _b_k = NoChange
      , _steadyStateTarget = 0.0
      , rfState = Detect
      }

  filterCounts vMask vCounts = flip map (zip vMask vCounts)
    $ \(isActive, count) -> if isActive == high then count else 0

  allAvailable f x y =
    and $ zipWith ((||) . not) (bitToBool <$> bv2v x) (f <$> y)

{- | Clock correction strategy based on:

  https://github.com/bittide/Callisto.jl

Note that this is an incredibly wasteful implementation: it instantiates
numerous floating point multipliers and adders, even though they're not doing
any useful work 99% of the time. Furthermore, 'RelDataCount' isn't properly
scaled to match elastic buffer sizes, resulting in unnecessarily big integer
adders. Optimization work has been postponed because:

  * It isn't clear yet whether this will be the final clock control algorithm.
  * These algorithms will probably run on a Risc core in the future.
-}
callisto ::
  forall m n dom.
  ( HiddenClockResetEnable dom
  , KnownNat n
  , KnownNat m
  , 1 <= n
  , 1 <= m
  , -- 'callisto' sums incoming 'RelDataCount's and feeds them to a Xilinx signed to
    -- float IP. We can currently only interpret 32 bit signeds to unsigned, so to
    -- make sure we don't overflow any addition we force @n + m <= 32@.
    n + m <= 32
  ) =>
  -- | Configuration parameters.
  ControlConfig m ->
  -- | Link availability mask.
  Signal dom (BitVector n) ->
  -- | Stability indicators for each of the elastic buffers.
  Signal dom (Vec n StabilityIndication) ->
  -- | Data counts from elastic buffers.
  Signal dom (Vec n (RelDataCount m)) ->
  -- | Current state.
  Signal dom ControlSt ->
  -- | Updated state.
  Signal dom ControlSt
callisto ControlConfig{..} mask scs dataCounts state =
  rfStateUpdate
    <$> (all stable <$> scs)
    <*> D.toSignal c_des
    <*> updatedState
 where
  updatedState =
    D.toSignal
      $ ControlSt
      <$> delayIU "[1]" z_kNext
      <*> b_kNext
      <*> delayIU "[2]" steadyStateTarget
      <*> delayIU "[3]" (D.fromSignal (rfState <$> state))

  -- See fields in 'ControlSt' for documentation of 'z_k', 'b_k', and css.
  z_k :: DSignal dom 0 (Signed 32)
  z_k = D.fromSignal (_z_k <$> state)

  b_k :: DSignal dom 0 SpeedChange
  b_k = D.fromSignal (_b_k <$> state)

  steadyStateTarget :: DSignal dom 0 Float
  steadyStateTarget = D.fromSignal (_steadyStateTarget <$> state)

  -- see clock control algorithm simulation here:
  --
  -- https://github.com/bittide/Callisto.jl/blob/e47139fca128995e2e64b2be935ad588f6d4f9fb/demo/pulsecontrol.jl#L24
  --
  -- `k_p` (proportional gain) is copied from the Julia implementation. `fStep` should
  -- match the step size of the clock boards. For all our HITL tests this is set by
  -- `HwCcTopologies.commonStepSizeSelect`.
  --
  k_p, fStep :: forall d. DSignal dom d Float
  k_p = pure 2e-9
  fStep = pure 10e-9

  r_k :: DSignal dom F.FromS32DefDelay Float
  r_k =
    F.fromS32
      $ D.fromSignal
      $ let
          nBuffers = case useLowerLimit @n @m @32 of
            Dict -> safePopCountTo32 <$> mask
          measuredSum = sumTo32 <$> dataCounts
          targetCountSigned = case euclid3 @n @m @32 of
            Dict -> case leTrans @1 @n @(32 - m) of
              Sub Dict -> extend @_ @_ @(32 - m - 1) $ dataCountToSigned targetCount
         in
          measuredSum - (pure targetCountSigned * nBuffers)

  c_des :: DSignal dom (F.FromS32DefDelay + F.MulDefDelay + F.AddDefDelay) Float
  c_des = delayIU "[4]" $ (k_p `F.mul` r_k) `F.add` delayIU "[5]" steadyStateTarget

  z_kNext :: DSignal dom 0 (Signed 32)
  z_kNext = z_k + fmap sign b_k

  c_est :: DSignal dom (F.FromS32DefDelay + F.MulDefDelay + F.AddDefDelay) Float
  c_est = delayIU "[6]" $ fStep `F.mul` F.fromS32 z_kNext

  b_kNext =
    flip fmap (F.compare c_des c_est) $ \case
      F.LT -> SlowDown
      F.GT -> SpeedUp
      F.EQ -> NoChange
      -- TODO: Propagate errors upwards?
      F.NaN -> NoChange

  rfStateUpdate stable target st@ControlSt{..}
    | not reframingEnabled = st
    | otherwise = case rfState of
        Detect
          | not stable ->
              st
          | otherwise ->
              st
                { rfState =
                    Wait
                      { curWaitTime = waitTime
                      , targetCorrection = target
                      }
                }
        Wait{..}
          | curWaitTime > 0 ->
              st
                { rfState =
                    Wait
                      { curWaitTime = curWaitTime - 1
                      , ..
                      }
                }
          | otherwise ->
              st
                { rfState = Done
                , _steadyStateTarget = targetCorrection
                }
        Done -> st

  -- Uninitialized version of 'Clash.Signal.Delayed.delayI'
  delayIU ::
    forall d k a.
    (HiddenClock dom, HiddenEnable dom, NFDataX a, KnownNat d) =>
    String ->
    DSignal dom k a ->
    DSignal dom (k + d) a
  delayIU =
    D.delayI . errorX . ("callisto: No start value " <>)
