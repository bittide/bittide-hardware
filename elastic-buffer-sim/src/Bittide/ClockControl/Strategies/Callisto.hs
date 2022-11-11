-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ScopedTypeVariables #-}

module Bittide.ClockControl.Strategies.Callisto
  ( callisto
  )
where

import Clash.Prelude

import Bittide.ClockControl
import Data.Constraint
import Data.Constraint.Nat.Extra (euclid3)

import qualified Clash.Signal.Delayed as D
import qualified Clash.Cores.Xilinx.Floating as F

data ControlSt = ControlSt
  { _x_k :: !Float
  -- ^ Integral of the measurement
  , _z_k :: !(Signed 32)
  -- ^ Accumulated speed change requests, where speedup ~ 1, slowdown ~ -1.
  , _b_k :: !SpeedChange
  -- ^ Previously submitted speed change request. Used to determine the estimated
  -- clock frequency.
  } deriving (Generic, NFDataX)

-- | Initial state of control
initState :: ControlSt
initState = ControlSt 0 0 NoChange

-- | A counter that starts at a given value, counts down, and if it reaches
-- zero wraps around to the initial value.
wrappingCounter ::
  (HiddenClockResetEnable dom, KnownNat n) =>
  Unsigned n ->
  Signal dom (Unsigned n)
wrappingCounter upper = counter
 where
  counter = register upper (go <$> counter)

  go 0 = upper
  go n = pred n

-- | A version of 'sum' that is guaranteed not to overflow.
safeSum ::
  ( KnownNat n
  , KnownNat m
  , 1 <= n
  ) =>
  Vec n (Unsigned m) ->
  Unsigned (m + n - 1)
safeSum = sum . map extend

-- | Sum a bunch of 'Unsigned's to a @Signed 32@, without overflowing.
sumTo32 ::
  forall n m .
  ( KnownNat m
  , KnownNat n
  , (m + n) <= 32
  , 1 <= n
  ) =>
  Vec n (Unsigned m) ->
  Signed 32
sumTo32 =
    extend @_ @_ @(32 - (m+n))
  . unsignedToSigned
  . safeSum

-- | Safe 'Unsigned' to 'Signed' conversion
unsignedToSigned :: forall n. KnownNat n => Unsigned n -> Signed (n + 1)
unsignedToSigned n = bitCoerce (zeroExtend n)

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
  forall m n dom.
  ( HiddenClockResetEnable dom
  , KnownNat n
  , KnownNat m
  , 1 <= n
  , 1 <= m

  -- 'callisto' sums incoming 'DataCount's and feeds them to a Xilinx signed to
  -- float IP. We can currently only interpret 32 bit signeds to unsigned, so to
  -- make sure we don't overflow any addition we force @n + m <= 32@.
  , n + m <= 32
  ) =>
  -- | Target data count. See 'targetDataCount'.
  DataCount m ->
  -- | Provide an update every /n/ cycles
  Unsigned 32 ->
  -- | Data counts from elastic buffers
  Signal dom (Vec n (DataCount m)) ->
  -- | Speed change requested from clock multiplier
  Signal dom SpeedChange
callisto targetCount updateEveryNCycles dataCounts =
  mux shouldUpdate (D.toSignal b_kNext) (pure NoChange)
 where
  updateCounter = wrappingCounter updateEveryNCycles
  shouldUpdate = updateCounter .==. 0
  state = register initState (mux shouldUpdate updatedState state)
  updatedState = D.toSignal $
    ControlSt
      <$> D.delayI (errorX "callisto: No start value [1]") x_kNext
      <*> D.delayI (errorX "callisto: No start value [2]") z_kNext
      <*> b_kNext

  -- See fields in 'ControlSt' for documentation of 'x_k', 'z_k', and 'b_k'.
  x_k :: DSignal dom 0 Float
  x_k = D.fromSignal (_x_k <$> state)

  z_k :: DSignal dom 0 (Signed 32)
  z_k = D.fromSignal (_z_k <$> state)

  b_k :: DSignal dom 0 SpeedChange
  b_k = D.fromSignal (_b_k <$> state)

  -- see clock control algorithm simulation here:
  -- https://github.com/bittide/Callisto.jl/blob/e47139fca128995e2e64b2be935ad588f6d4f9fb/demo/pulsecontrol.jl#L24
  --
  -- the constants here are chosen to match the above code.
  k_p, k_i, fStep :: forall d. DSignal dom d Float
  k_p = pure 2e-4
  k_i = pure 1e-11
  fStep = pure 5e-4

  r_k :: DSignal dom F.FromS32DefDelay Float
  r_k = F.fromS32 $ D.fromSignal $
    let
      nBuffers = natToNum @n
      measuredSum = sumTo32 <$> dataCounts
      targetCountSigned =
        case euclid3 @n @m @32 of
          Dict ->
            extend @_ @_ @(32 - m - 1) (unsignedToSigned targetCount)
    in
      measuredSum - pure (targetCountSigned * nBuffers)

  x_kNext :: DSignal dom (F.FromS32DefDelay + F.MulDefDelay + F.AddDefDelay) Float
  x_kNext = D.delayI (errorX "callisto: No start value [3]") x_k `F.add` (p `F.mul` r_k)

  c_des :: DSignal dom (F.FromS32DefDelay + 2*F.MulDefDelay + 2*F.AddDefDelay) Float
  c_des = delayLhs (k_p `F.mul` r_k) `F.add` (k_i `F.mul` x_kNext)
   where
    delayLhs = D.delayI (errorX "callisto: No start value [5]")

  z_kNext :: DSignal dom 0 (Signed 32)
  z_kNext = z_k + fmap sign b_k

  c_est :: DSignal dom (F.FromS32DefDelay + 2*F.MulDefDelay + 2*F.AddDefDelay) Float
  c_est = D.delayI (errorX "callisto: No start value [4]") (fStep `F.mul` F.fromS32 z_kNext)

  -- we are using 200kHz instead of 200MHz
  -- (see https://github.com/clash-lang/clash-compiler/issues/2328)
  -- so typical freq. is 0.0002 GHz
  --
  -- (this is adjusted by a factor of 100 because our clock corrections are
  -- faster than those simulated in Callisto; we correct as often as hardware
  -- allows)
  p = 1e3 * typicalFreq where typicalFreq = 0.0002

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
