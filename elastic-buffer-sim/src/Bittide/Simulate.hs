{-|

Provides a rudimentary simulation of elastic buffers.

-}

-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bittide.Simulate where

import Clash.Prelude
import Clash.Signal.Internal
import Data.Bifunctor (second)
import GHC.Stack
import Numeric.Natural

import Bittide.Simulate.Ppm

import Data.Csv

-- Number of type aliases for documentation purposes in various functions defined
-- down below.
type StepSize = Natural
type InitialPeriod = Natural
type DataCount = Natural
type ElasticBufferSize = Natural
type Offset = Integer
type DynamicRange = Natural
type SettlePeriod = Natural

-- | Calculate target data count given a FIFO size. Currently returns a target
-- data count of half the FIFO size.
targetDataCount :: ElasticBufferSize -> DataCount
targetDataCount size = size `div` 2

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

-- | Simple model of the Si5395/Si5391 clock multipliers. In real hardware, these
-- are connected to some oscillator (i.e., incoming Clock) but for simulation
-- purposes we pretend it generates the clock too.
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
  -- | Offset from the ideal period (encoded in the domain) of thiss clock. For
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
  let period = snatToNum (clockPeriod @dom)
      initPeriod = fromIntegral (period + periodOffset)
      clockSignal = initPeriod :- go settlePeriod initPeriod speedChange in
  Clock SSymbol (Just clockSignal)
 where
  go :: SettlePeriod -> PeriodPs -> Signal dom SpeedChange -> Signal dom StepSize
  go !settleCounter !period (sc :- scs) =
    let
      (newSettleCounter, newPeriod) = case sc of
        SpeedUp
          | settleCounter >= settlePeriod -> (0, period - stepSize)
          | otherwise -> error "tunableClockGen: frequency change requested too often"
        SlowDown
          | settleCounter >= settlePeriod -> (0, period + stepSize)
          | otherwise -> error "tunableClockGen: frequency change requested too often"
        NoChange ->
          (settleCounter + period, period)
    in
      newPeriod :- go newSettleCounter newPeriod scs

-- | Determines how 'elasticBuffer' should respond to underflow/overflow.
data OverflowMode
  -- | Saturate at empty/full boundaries. Useful for testing purposes.
  = Saturate
  -- | Error on write-when-full, or read-when-empty. This mode shuold be used in
  -- practise. Overflowing elastic buffer cannot happen in a real Bittide system.
  | Error

-- | Simple model of a FIFO that only models the interesting part for conversion:
-- data counts.
elasticBuffer ::
  forall readDom writeDom.
  (HasCallStack, KnownDomain readDom, KnownDomain writeDom) =>
  -- | What behavior to pick on underflow/overflow
  OverflowMode ->
  -- | Size of FIFO. To reflect our target platforms, this should be a power of two
  -- where typical sizes would probably be: 16, 32, 64, 128.
  ElasticBufferSize ->
  Clock readDom ->
  Clock writeDom ->
  Signal readDom DataCount
elasticBuffer mode size clkRead clkWrite
  | Clock _ (Just readPeriods) <- clkRead
  , Clock _ (Just writePeriods) <- clkWrite
  = go 0 (targetDataCount size) readPeriods writePeriods
 where
  go !relativeTime !fillLevel rps wps@(writePeriod :- _) =
    if relativeTime < toInteger writePeriod
    then goRead relativeTime fillLevel rps wps
    else goWrite relativeTime fillLevel rps wps

  goWrite relativeTime fillLevel rps (writePeriod :- wps) =
    go (relativeTime - toInteger writePeriod) newFillLevel rps wps
   where
    newFillLevel
      | fillLevel >= size = case mode of
          Saturate -> fillLevel
          Error -> error "elasticBuffer: overflow"
      | otherwise = fillLevel + 1

  goRead relativeTime fillLevel (readPeriod :- rps) wps =
    newFillLevel :- go (relativeTime + toInteger readPeriod) newFillLevel rps wps
   where
    newFillLevel
      | fillLevel <= 0 = case mode of
          Saturate -> 0
          Error -> error "elasticBuffer: underflow"
      | otherwise = fillLevel - 1

elasticBuffer mode size (Clock ss Nothing) clock1 =
  -- Convert read clock to a "dynamic" clock if it isn't one
  let
    period = snatToNum (clockPeriod @readDom)
  in
    elasticBuffer mode size (Clock ss (Just (pure period))) clock1

elasticBuffer mode size clock0 (Clock ss Nothing) =
  -- Convert write clock to a "dynamic" clock if it isn't one
  let
    period = snatToNum (clockPeriod @writeDom)
  in
    elasticBuffer mode size clock0 (Clock ss (Just (pure period)))

-- | Configuration passed to 'clockControl'
data ClockControlConfig = ClockControlConfig
  { -- | The quickest a clock could possibly run at. Used to (pessimistically)
    -- estimate when a new command can be issued.
    cccPessimisticPeriod :: PeriodPs

  -- | Period it takes for a clock frequency request to settle. This is not
  -- modelled, but an error is thrown if a request is submitted more often than
  -- this. 'clockControl' should therefore not request changes more often.
  , cccSettlePeriod :: PeriodPs

  -- | Maximum divergence from initial clock frequency. Used to prevent frequency
  -- runoff.
  , cccDynamicRange :: Ppm

  -- | The size of the clock frequency should "jump" on a speed change request.
  , cccStepSize :: Integer

  -- | Size of elastic buffers. Used to observe bounds and 'targetDataCount'.
  , cccBufferSize :: ElasticBufferSize
  } deriving (Lift)

-- we use 200kHz in simulation because otherwise the periods are so small that
-- deviations can't be expressed using 'Natural's
specPeriod :: PeriodPs
specPeriod = hzToPeriod 200e3

defClockConfig :: ClockControlConfig
defClockConfig = ClockControlConfig
  { cccPessimisticPeriod = pessimisticPeriod
  -- clock adjustment takes place at 1MHz, clock is 200MHz so we can have at most
  -- one correction per 200 cycles
  , cccSettlePeriod      = pessimisticPeriod * 200
  , cccDynamicRange      = 150
  , cccStepSize          = 1
  , cccBufferSize        = 2048 -- 128
  }
 where
  specPpm = 100
  pessimisticPeriod = speedUpPeriod specPpm specPeriod

data ControlSt = ControlSt
  { x_k :: Double -- ^ 'x_k' is the integral of the measurement
  , z_k :: Integer
  , b_k :: SpeedChange
  }

-- | Determines how to influence clock frequency given statistics provided by
-- all elastic buffers.
clockControl ::
  forall n dom.
  (KnownNat n, 1 <= n) =>
  -- | Configuration for this component, see individual fields for more info.
  ClockControlConfig ->
  -- | Statistics provided by elastic buffers.
  Vec n (Signal dom DataCount) ->
  Signal dom SpeedChange
clockControl cfg = runClockControl cfg callisto (ControlSt 0 0 NoChange)

runClockControl ::
  forall n dom a.
  (KnownNat n, 1 <= n) =>
  -- | Configuration for this component, see individual fields for more info.
  ClockControlConfig ->
  -- | Clock control strategy
  (ClockControlConfig -> SettlePeriod -> a -> Signal dom (Vec n DataCount) -> (a, Signal dom SpeedChange)) ->
  -- | Initial clock control state
  a ->
  -- | Statistics provided by elastic buffers.
  Vec n (Signal dom DataCount) ->
  -- | Whether to adjust node clock frequency
  Signal dom SpeedChange
runClockControl cfg@ClockControlConfig{..} f initSt =
  snd . f cfg (cccSettlePeriod + 1) initSt . bundle

callisto :: 
  forall n dom.
  (KnownNat n, 1 <= n) =>
  ClockControlConfig ->
  SettlePeriod ->
  ControlSt ->
  Signal dom (Vec n DataCount) ->
  (ControlSt, Signal dom SpeedChange)
callisto cfg@ClockControlConfig{..} settleCounter ControlSt{..} (dataCounts :- nextDataCounts) 
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
    toInteger (sum dataCounts) - (toInteger (targetDataCount cccBufferSize) * toInteger (length dataCounts))
  x_kNext =
    x_k + p * realToFrac r_k

  c_des = k_p * realToFrac r_k + k_i * realToFrac x_kNext
  z_kNext = z_k + sgn b_k
  fStep = 5e-4
  c_est = fStep * realToFrac z_kNext
  -- we are using 200kHz instead of 200MHz
  -- typical freq. is in GHz
  --
  -- (this is adjusted by a factor of 100 because our clock corrections are
  -- faster than those simulated in Callisto)
  p = 1e3 * typicalFreq where typicalFreq = 0.0002

  b_kNext =
    case compare c_des c_est of
      LT -> SlowDown
      GT -> SpeedUp
      EQ -> NoChange

  sgn NoChange = 0
  sgn SpeedUp = 1
  sgn SlowDown = -1

  nextChanges = callisto cfg 0 (ControlSt x_kNext z_kNext b_kNext) nextDataCounts

callisto cfg@ClockControlConfig{..} settleCounter st (_ :- nextDataCounts) =
  second (NoChange :-) nextChanges
 where
  nextChanges = callisto cfg newSettleCounter st nextDataCounts
  newSettleCounter = settleCounter + cccPessimisticPeriod

minTOffset, maxTOffset :: Ppm -> PeriodPs -> Offset
minTOffset ppm period = toInteger (speedUpPeriod ppm period) - toInteger period
maxTOffset ppm period = toInteger (slowDownPeriod ppm period - period)
