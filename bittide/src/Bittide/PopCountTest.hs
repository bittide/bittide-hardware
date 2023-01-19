-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.PopCountTest
  ( popCountTest
  ) where

import Clash.Prelude

import Bittide.ClockControl
import qualified Bittide.ClockControl.Callisto.Util as U

import qualified Clash.Cores.Xilinx.Floating as F
import qualified Clash.Signal.Delayed as D

data ControlSt = ControlSt
  { _x_k :: !Float
  , _z_k :: !(Signed 32)
  , _b_k :: !Bool
  } deriving (Generic, NFDataX)

{-# NOINLINE popCountTest #-}
popCountTest ::
  forall dom.
  HiddenClockResetEnable dom =>
  Signal dom (Vec 3 (DataCount 12)) ->
  Signal dom Bool
popCountTest dataCounts = D.toSignal b_kNext
 where
  mask :: Signal dom (BitVector 3)
  mask = pure $ pack $ repeat high

  state =
    D.toSignal $ ControlSt
      <$> D.delayI 0 x_k
      <*> D.delayI 0 z_k
      <*> b_kNext

  -- `x_k` is technically not used, but removing it produces an error
  -- in clash. With x_k available clash passes with a strange warning,
  -- but vivado synthesis fails afterwards.
  x_k :: DSignal dom 0 Float
  x_k = D.fromSignal (_x_k <$> state)

  z_k :: DSignal dom 0 (Signed 32)
  z_k = D.fromSignal (_z_k <$> state)

  -- `sumTo32` seems to be essential for producing the
  -- error. Inlineing it produces the same error in clash as described
  -- in the comment above. Moving the call outside the library removes
  -- the error completely.
  r_k :: DSignal dom F.FromS32DefDelay Float
  r_k = F.fromS32 $ D.fromSignal $ (U.sumTo32 <$> dataCounts) - (popCountTo32 <$> mask)

  c_des :: DSignal dom (F.FromS32DefDelay + 2*F.MulDefDelay + 2*F.AddDefDelay) Float
  c_des = D.delayI 0 r_k

  c_est :: DSignal dom (F.FromS32DefDelay + 2*F.MulDefDelay + 2*F.AddDefDelay) Float
  c_est = D.delayI 0 $ F.fromS32 z_k

  b_kNext = (== F.LT) <$> F.compare c_des c_est

-- The generalization to n-sized bit vectors is needed for reproducing
-- the error, i.e., there is no error if `popCoutnTo32` is specialized
-- to bit vectors of size 3. Note that moving the definition to the
-- where clause of `popCountTest` has the same effect.
popCountTo32 :: KnownNat n => BitVector n -> Signed 32
popCountTo32 = resize . bitCoerce . popCount
