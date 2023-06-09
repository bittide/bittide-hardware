-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}

module Bittide.ClockControl.Callisto
  ( CallistoResult(..)
  , ReframingState(..)
  , callistoClockControl
  ) where

import Clash.Prelude

import Data.Constraint
import Data.Constraint.Nat.Extra (euclid3, useLowerLimit)

import Bittide.ClockControl
import Bittide.ClockControl.Callisto.Util
import Bittide.ClockControl.StabilityChecker

import qualified Clash.Cores.Xilinx.Floating as F
import qualified Clash.Signal.Delayed as D

-- | Result of the clock control algorithm.
data CallistoResult (n :: Nat) =
  CallistoResult
    { speedChange :: SpeedChange
    -- ^ Speed change requested from clock multiplier.
    , stability :: Vec n StabilityIndication
    -- ^ All stability indicators for all of the elastic buffers.
    , allStable :: Bool
    -- ^ Joint stability indicator signaling that all elastic buffers
    -- are stable.
    , allSettled :: Bool
    -- ^ Joint "being-settled" indicator signaling that all elastic
    -- buffers have been settled.
    , reframingState :: ReframingState
    -- ^ State of the Reframing detector
    }
  deriving (Generic, NFDataX)

-- | State of the state machine for realizing the "detect, store, and
-- wait" approach of [arXiv:2303.11467](https://arxiv.org/abs/2303.11467)
data ReframingState =
    Detect
    -- ^ The controller remains in this state until stability has been
    -- detected.
  | Wait
    -- ^ The controller remains in this state for the predefined
    -- number of cycles with the assumption that the elastic buffers
    -- of all other nodes are sufficiently stable after that time.
      { targetCorrection :: !Float
      -- ^ Stored correction value to be applied at reframing time.
      , curWaitTime :: !(Unsigned 32)
      -- ^ Number of cycles to wait until reframing takes place.
      }
  | Done
    -- ^ Reframing has taken place. There is nothing more to do.
  deriving (Generic, NFDataX)

{-# NOINLINE callistoClockControl #-}
-- | Determines how to influence clock frequency given statistics provided by
-- all elastic buffers. See 'callisto' for more information.
--
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
  Vec n (Signal dom (DataCount m)) ->
  Signal dom (CallistoResult n)
callistoClockControl clk rst ena ClockControlConfig{..} mask =
  withClockResetEnable clk rst ena $
    callisto
      cccEnableReframing
      cccReframingWaitTime
      cccStabilityCheckerMargin
      cccStabilityCheckerFramesize
      targetDataCount
      cccPessimisticSettleCycles
      mask
      . bundle

-- | State used in 'callisto'
data ControlSt =
  ControlSt
    { _z_k :: !(Signed 32)
    -- ^ Accumulated speed change requests, where speedup ~ 1, slowdown ~ -1.
    , _b_k :: !SpeedChange
    -- ^ Previously submitted speed change request. Used to determine the estimated
    -- clock frequency.
    , _steadyStateTarget :: !Float
    -- ^ Steady-state value (determined when stability is detected for
    -- the first time).
    , rfState :: !ReframingState
    -- ^ finite state machine for reframing detection
    }
  deriving (Generic, NFDataX)

-- | Initial state of control
initState :: ControlSt
initState =
  ControlSt
    { _z_k = 0
    , _b_k = NoChange
    , _steadyStateTarget = 0.0
    , rfState = Detect
    }

-- | Clock correction strategy based on:
--
--   https://github.com/bittide/Callisto.jl
--
-- Note that this is an incredibly wasteful implementation: it instantiates
-- numerous floating point multipliers and adders, even though they're not doing
-- any useful work 99% of the time. Furthermore, 'DataCount' isn't properly
-- scaled to match elastic buffer sizes, resulting in unnecessarily big integer
-- adders. Optimization work has been postponed because:
--
--   * It isn't clear yet whether this will be the final clock control algorithm.
--   * These algorithms will probably run on a Risc core in the future.
--
callisto ::
  forall m n dom margin framesize.
  ( HiddenClockResetEnable dom
  , KnownNat n
  , KnownNat m
  , KnownNat margin
  , KnownNat framesize
  , 1 <= n
  , 1 <= m
  -- 'callisto' sums incoming 'DataCount's and feeds them to a Xilinx signed to
  -- float IP. We can currently only interpret 32 bit signeds to unsigned, so to
  -- make sure we don't overflow any addition we force @n + m <= 32@.
  , n + m <= 32
  , 1 <= framesize
  ) =>
  -- | Enable reframing. Reframing allows a system to resettle buffers around
  -- their midpoints, without dropping any frames. For more information, see
  -- [arXiv:2303.11467](https://arxiv.org/abs/2303.11467).
  Bool ->
  -- | Number of cycles to wait until reframing takes place after
  -- stability has been detected.
  Unsigned 32 ->
  -- | Maximum number of elements the incoming buffer occupancy is
  -- allowed to deviate from the current @target@ for it to be
  -- considered "stable".
  SNat margin ->
  -- | Minimum number of clock cycles the incoming buffer occupancy
  -- must remain within the @margin@ for it to be considered "stable".
  SNat framesize ->
  -- | Target data count. See 'targetDataCount'.
  DataCount m ->
  -- | Provide an update every /n/ cycles
  Unsigned 32 ->
  -- | Link availability mask.
  Signal dom (BitVector n) ->
  -- | Data counts from elastic buffers.
  Signal dom (Vec n (DataCount m)) ->
  Signal dom (CallistoResult n)
callisto
  -- configuration options
  reframingEnabled waitTime margin framesize targetCount updateEveryNCycles
  -- input signals
  mask allDataCounts =
  -- output signal
  CallistoResult
    <$> mux shouldUpdate (D.toSignal b_kNext) (pure NoChange)
    <*> scs
    <*> allStable
    <*> allSettled
    <*> (rfState <$> state)
 where
  dataCounts = filterCounts <$> fmap bv2v mask <*> allDataCounts
  filterCounts vMask vCounts = flip map (zip vMask vCounts) $
    \(isActive, count) -> if isActive == high then count else 0
  scs = bundle $ map (stabilityChecker margin framesize) $ unbundle dataCounts
  updateCounter = wrappingCounter updateEveryNCycles
  shouldUpdate = updateCounter .==. 0
  allStable = and . map stable <$> scs
  allSettled = and . map settled <$> scs
  state = register initState $
    rfStateUpdate
      <$> allStable
      <*> D.toSignal c_des
      <*> mux shouldUpdate updatedState state
  updatedState = D.toSignal $
    ControlSt
      <$> D.delayI
            (errorX "callisto: No start value [1]")
            z_kNext
      <*> b_kNext
      <*> D.delayI
            (errorX "callisto: No start value [2]")
            steadyStateTarget
      <*> D.delayI
            (errorX "callisto: No start value [3]")
            (D.fromSignal (rfState <$> state))

  -- See fields in 'ControlSt' for documentation of 'z_k', 'b_k', and css.
  z_k :: DSignal dom 0 (Signed 32)
  z_k = D.fromSignal (_z_k <$> state)

  b_k :: DSignal dom 0 SpeedChange
  b_k = D.fromSignal (_b_k <$> state)

  steadyStateTarget :: DSignal dom 0 Float
  steadyStateTarget = D.fromSignal (_steadyStateTarget <$> state)

  -- see clock control algorithm simulation here:
  -- https://github.com/bittide/Callisto.jl/blob/e47139fca128995e2e64b2be935ad588f6d4f9fb/demo/pulsecontrol.jl#L24
  --
  -- the constants here are chosen to match the above code.
  k_p, fStep :: forall d. DSignal dom d Float
  k_p = pure 2e-4
  fStep = pure 5e-4

  r_k :: DSignal dom F.FromS32DefDelay Float
  r_k = F.fromS32 $ D.fromSignal $
    let
      nBuffers = case useLowerLimit @n @m @32 of
        Dict -> safePopCountTo32 <$> mask
      measuredSum = sumTo32 <$> dataCounts
      targetCountSigned =
        case euclid3 @n @m @32 of
          Dict ->
            extend @_ @_ @(32 - m - 1) (dataCountToSigned targetCount)
    in
      measuredSum - (pure targetCountSigned * nBuffers)

  c_des :: DSignal dom (F.FromS32DefDelay + F.MulDefDelay + F.AddDefDelay) Float
  c_des = D.delayI (errorX "callisto: No start value [4]") (k_p `F.mul` r_k)
            `F.add` (delayI 0 steadyStateTarget)

  z_kNext :: DSignal dom 0 (Signed 32)
  z_kNext = z_k + fmap sign b_k

  c_est :: DSignal dom (F.FromS32DefDelay + F.MulDefDelay + F.AddDefDelay) Float
  c_est = D.delayI (errorX "callisto: No start value [5]") (fStep `F.mul` F.fromS32 z_kNext)

  b_kNext =
    flip fmap (F.compare c_des c_est) $ \case
      F.LT -> SlowDown
      F.GT -> SpeedUp
      F.EQ -> NoChange

      -- TODO: Propagate errors upwards?
      F.NaN -> NoChange

  sign NoChange = 0
  sign SpeedUp = 1
  sign SlowDown = -1

  rfStateUpdate stable target st@ControlSt{..}
    | not reframingEnabled = st
    | otherwise = case rfState of
        Detect
          | not stable ->
              st
          | otherwise ->
              st { rfState = Wait { curWaitTime = waitTime
                                  , targetCorrection = target
                                  }
                 }
        Wait{..}
          | curWaitTime > 0 ->
              st { rfState = Wait { curWaitTime = curWaitTime - 1
                                  , ..
                                  }
                 }
          | otherwise ->
              st { rfState = Done
                 , _steadyStateTarget = targetCorrection
                 }
        Done -> st
