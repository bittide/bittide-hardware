-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Bittide.ClockControl.ClockGen where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (dynamicClockGen)

import Data.Csv
import GHC.Stack (HasCallStack)


-- | Safer version of FINC/FDEC signals present on the Si5395/Si5391 clock multipliers.
data SpeedChange
  = SpeedUp
  | SlowDown
  | NoChange
  deriving (Eq, Show, Generic, ShowX, NFDataX)

instance ToField SpeedChange where
  toField SpeedUp = "speedUp"
  toField SlowDown = "slowDown"
  toField NoChange = "noChange"

type DataCount = Unsigned 64
type SettlePeriod = Signed 64

-- Translatable version of the Si5395/Si5391 boards.
--
-- Note that various arguments are 'Signed' while they - fundamentally - should
-- be 'Unsigned'. This is done to ease implementation. Given that @Signed 64@ is
-- enough to store 5 hours if it represents the period in femtoseconds, it
-- shouldn't pose any problems - we can't simulate 5 hours of runtime.
tunableClockGen ::
  forall dom.
  (HasCallStack, KnownDomain dom) =>
  -- | Period it takes for a clock frequency request to settle. This is not
  -- modelled, but an error is thrown if a request is submitted more often than
  -- this.
  Unsigned 64 ->
  -- | Offset from the ideal period (encoded in the domain) of this clock. For
  -- the Si5395/Si5391 oscillators, this value lies between ±100 ppm.
  Signed 64 ->
  -- | The size of the clock frequency should "jump" on a speed change request.
  Signed 64 ->
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
tunableClockGen settlePeriod periodOffset stepSize speedChanges =
  let
    period = snatToNum (clockPeriod @dom)
    initPeriod = period + periodOffset
    (settleCounter, periods) =
      unbundle $ delay clock enableGen (settlePeriod, initPeriod) $
        go <$> settleCounter <*> periods <*> speedChanges
    clock = dynamicClockGen (fromIntegral <$> periods)
  in
    clock
 where
  go :: Unsigned 64 -> Signed 64 -> SpeedChange -> (Unsigned 64, Signed 64)
  go !counter !period = \case
    NoChange         -> (satSub SatBound counter (bitCoerce period), period)
    _ | counter /= 0 -> errorX "tunableClockGen: frequency change requested too often"
    SpeedUp          -> (settlePeriod, period - stepSize)
    SlowDown         -> (settlePeriod, period + stepSize)
