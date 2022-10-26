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

data ControlSt = ControlSt
  { _x_k :: Float -- ^ 'x_k' is the integral of the measurement
  , _z_k :: Signed 32
  , _b_k :: SpeedChange
  } deriving (Generic, NFDataX)

initState :: ControlSt
initState = ControlSt 0 0 NoChange

unsignedToSigned :: KnownNat n => Unsigned n -> Signed (n + 1)
unsignedToSigned = bitCoerce . zeroExtend

wrappingCounter ::
  (HiddenClockResetEnable dom, KnownNat n) =>
  Unsigned n ->
  Signal dom (Unsigned n)
wrappingCounter upper = counter
 where
  counter = register upper (go <$> counter)

  go 0 = upper
  go n = pred n

callisto ::
  forall n dom.
  (HiddenClockResetEnable dom, KnownNat n, 1 <= n) =>
  -- | Target data count. See 'targetDataCount'.
  DataCount ->
  -- | Provide an update every /n/ cycles
  Unsigned 32 ->
  -- | Data counts from elastic buffers
  Signal dom (Vec n DataCount) ->
  -- | Speed change requested from clock multiplier
  Signal dom SpeedChange
callisto targetCount updateEveryNCycles dataCounts =
  mux shouldUpdate b_kNext (pure NoChange)
 where
  updateCounter = wrappingCounter updateEveryNCycles -- TODO: use Index
  shouldUpdate = updateCounter .==. 0
  state = register initState (mux shouldUpdate updatedState state)
  updatedState = ControlSt <$> x_kNext <*> z_kNext <*> b_kNext

  x_k :: Signal dom Float
  x_k = _x_k <$> state

  z_k :: Signal dom (Signed 32)
  z_k = _z_k <$> state

  b_k :: Signal dom SpeedChange
  b_k = _b_k <$> state

  -- see clock control algorithm simulation here:
  -- https://github.com/bittide/Callisto.jl/blob/e47139fca128995e2e64b2be935ad588f6d4f9fb/demo/pulsecontrol.jl#L24
  --
  -- the constants here are chosen to match the above code.
  k_p = pure 2e-4 :: Signal dom Float
  k_i = pure 1e-11 :: Signal dom Float
  fStep = pure 5e-4 :: Signal dom Float

  r_k = realToFrac <$>
    let
      measuredSum = unsignedToSigned . sum <$> dataCounts
      targetCountS = unsignedToSigned targetCount
      nBuffers = natToNum @n
    in
      measuredSum - pure (targetCountS * nBuffers)
  x_kNext =
    x_k + p * r_k

  c_des = k_p * r_k + k_i * x_kNext
  z_kNext = z_k + fmap sign b_k
  c_est = fStep * fmap realToFrac z_kNext
  -- we are using 200kHz instead of 200MHz
  -- (see https://github.com/clash-lang/clash-compiler/issues/2328)
  -- so typical freq. is 0.0002 GHz
  --
  -- (this is adjusted by a factor of 100 because our clock corrections are
  -- faster than those simulated in Callisto; we correct as often as hardware
  -- allows)
  p = 1e3 * typicalFreq where typicalFreq = 0.0002

  b_kNext =
    flip fmap (compare <$> c_des <*> c_est) $ \case
      LT -> SlowDown
      GT -> SpeedUp
      EQ -> NoChange

  sign NoChange = 0
  sign SpeedUp = 1
  sign SlowDown = -1
