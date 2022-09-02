{-|

Provides a rudimentary simulation of elastic buffers.

-}

-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bittide.Simulate where

import Debug.Trace

import Clash.Prelude
import Clash.Signal.Internal
import GHC.Stack
import Numeric.Natural
import Control.DeepSeq (deepseq)

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
  -- clock adjustment takes place at 1MHz, clock is 200MHz so we get one correction per 200 cycles
  , cccSettlePeriod      = pessimisticPeriod * 200 * 100
  , cccDynamicRange      = 150
  , cccStepSize          = 1
  , cccBufferSize        = 65536 -- 128
  }
 where
  specPpm = 100
  pessimisticPeriod = speedUpPeriod specPpm specPeriod

-- | Determines how to influence clock frequency given statistics provided by
-- all elastic buffers.
--
-- TODO:
--
--   * Generalize to make it easy to "swap" strategies?
--
clockControl ::
  forall n dom.
  (KnownNat n, KnownDomain dom, 1 <= n) =>
  -- | Configuration for this component, see individual fields for more info.
  ClockControlConfig ->
  -- | Statistics provided by elastic buffers.
  Vec n (Signal dom DataCount) ->
  -- | Whether to adjust node clock frequency
  Signal dom SpeedChange
clockControl ClockControlConfig{..} =
  fth5 . go (cccSettlePeriod + 1) 0 0 0 NoChange . bundle
 where
  fth5 (_, _, _, _, e) = e
  -- x_k is the integral of the measurement
  go :: SettlePeriod -> Offset -> Double -> Integer -> SpeedChange -> Signal dom (Vec n DataCount) -> (Offset, Double, Integer, SpeedChange, Signal dom SpeedChange)
  go settleCounter offs x_k z_k b_k (dataCounts :- nextDataCounts) | settleCounter > cccSettlePeriod
    = fifth5 (speedChange :-) nextChanges
   where

    -- k_p = 5e-7, k_i = 1 gives typical overzealous control
    k_p = 2e-8 :: Double
    k_i = 1e-15 :: Double
    r_k =
      toInteger (sum dataCounts) - (toInteger (targetDataCount cccBufferSize) * toInteger (length dataCounts))
    x_k' =
      x_k + p * realToFrac r_k

    c_des = k_p * realToFrac r_k + k_i * realToFrac x_k'
    z_k' = z_k + b_kI
    c_est = realToFrac (cccStepSize * z_k')
    p = 1e5

    b_k' =
      case compare c_des c_est of
        LT -> SlowDown
        GT -> SpeedUp
        EQ -> NoChange

    b_kI = case b_k of
      NoChange -> 0
      SpeedUp -> 1
      SlowDown -> -1

    nextChanges = go newSettleCounter nextOffs x_k' z_k' b_k' nextDataCounts

    fifth5 f ~(a, b, c, d, e) = (a, b, c, d, f e)

    (speedChange, nextOffs) =
        case compare c_des c_est of
          LT | offs + cccStepSize <= ma -> (SlowDown, offs + cccStepSize)
          GT | offs - cccStepSize >= mi -> (SpeedUp, offs - cccStepSize)
          _ -> (NoChange, offs)

    newSettleCounter = 0
      -- case b_k' of
        -- NoChange -> settleCounter + cccPessimisticPeriod
        -- SpeedUp -> 0
        -- SlowDown -> 0

    mi = minTOffset cccDynamicRange domT
    ma = maxTOffset cccDynamicRange domT

    domT = snatToNum @PeriodPs (clockPeriod @dom)

  go settleCounter offs x_k z_k b_k (_ :- nextDataCounts) =
    fifth5 (NoChange :-) nextChanges
   where
    nextChanges = go newSettleCounter offs x_k z_k b_k nextDataCounts
    newSettleCounter = settleCounter + cccPessimisticPeriod
    fifth5 f ~(a, b, c, d, e) = (a, b, c, d, f e)


minTOffset, maxTOffset :: Ppm -> PeriodPs -> Offset
minTOffset ppm period = toInteger (speedUpPeriod ppm period) - toInteger period
maxTOffset ppm period = toInteger (slowDownPeriod ppm period - period)
