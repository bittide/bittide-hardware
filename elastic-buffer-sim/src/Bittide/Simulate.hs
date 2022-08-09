{-|

Provides a rudimentary simulation of elastic buffers.

TODO:

  * Define static topologies
  * Ability to extract statistics
  * Ability to generate topologies

-}

-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}

module Bittide.Simulate where

import Clash.Prelude
import Clash.Signal.Internal
import Data.Bifunctor (second)
import GHC.Stack
import Numeric.Natural

import Bittide.Simulate.Ppm

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
-- Note that the actual clock mulipliers detect edges. If we ever want to reuse code
-- from this module, we should make sure to generate these edges.
data SpeedChange
  = SpeedUp
  | SlowDown
  | NoChange
  deriving (Eq, Show, Generic, ShowX, NFDataX)

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
tunableClockGen settlePeriod periodOffset stepSize _reset =
  case knownDomain @dom of
    SDomainConfiguration _ (snatToNum -> period) _ _ _ _ ->
      DClock SSymbol . Just . go settlePeriod (fromIntegral (period + periodOffset))
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
  | DClock _ (Just readPeriods) <- clkRead
  , DClock _ (Just writePeriods) <- clkWrite
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

elasticBuffer mode size (DClock ss Nothing) clock1 =
  -- Convert read clock to a "dynamic" clock if it isn't one
  case knownDomain @readDom of
    SDomainConfiguration _ (snatToNum -> period) _ _ _ _ ->
      elasticBuffer mode size (DClock ss (Just (pure period))) clock1

elasticBuffer mode size clock0 (DClock ss Nothing) =
  -- Convert write clock to a "dynamic" clock if it isn't one
  case knownDomain @writeDom of
    SDomainConfiguration _ (snatToNum -> period) _ _ _ _ ->
      elasticBuffer mode size clock0 (DClock ss (Just (pure period)))

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
  }

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
  snd . go (cccSettlePeriod + 1) 0 . bundle
 where
  go :: SettlePeriod -> Offset -> Signal dom (Vec n DataCount) -> (Offset, Signal dom SpeedChange)
  go settleCounter offs (dataCounts :- nextDataCounts) = second (speedChange :-) nextChanges
   where
    nextChanges = go newSettleCounter nextOffs nextDataCounts
    average = sum dataCounts `div` fromIntegral (length dataCounts)

    (speedChange, nextOffs)
      | settleCounter > cccSettlePeriod =
          case compare average (targetDataCount cccBufferSize) of
            LT | offs + cccStepSize <= ma -> (SlowDown, offs + cccStepSize)
            GT | offs - cccStepSize >= mi -> (SpeedUp, offs - cccStepSize)
            _ -> (NoChange, offs)
      | otherwise = (NoChange, offs)

    newSettleCounter =
      case speedChange of
        NoChange -> settleCounter + cccPessimisticPeriod
        SpeedUp -> 0
        SlowDown -> 0

    mi = minTOffset cccDynamicRange domT
    ma = maxTOffset cccDynamicRange domT

    domT = snatToNum @PeriodPs (clockPeriod @dom)

minTOffset, maxTOffset :: Ppm -> PeriodPs -> Offset
minTOffset ppm period = toInteger (speedUpPeriod ppm period) - toInteger period
maxTOffset ppm period = toInteger (slowDownPeriod ppm period - period)
