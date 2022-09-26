{-|

Provides a rudimentary simulation of elastic buffers.

-}

-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}

module Bittide.Simulate where

import Clash.Explicit.Prelude
import Clash.Signal.Internal
import Data.Bifunctor (first, second)
import GHC.Stack
import Numeric.Natural

import Bittide.Simulate.Ppm
import Bittide.ClockControl

-- Number of type aliases for documentation purposes in various functions defined
-- down below.
type StepSize = Natural
type InitialPeriod = Natural
type Offset = Integer
type DynamicRange = Natural

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
  go ::
    SettlePeriod ->
    PeriodPs ->
    Signal dom SpeedChange ->
    Signal dom StepSize
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

type ResetClockControl = Bool
type HasUnderflowed = Bool
type HasOverflowed = Bool
type DisableTilHalf = Bool

type DisableWrites = Bool
type DisableReads = Bool

data WrResetDomain = Wait -- ^ after reset/when nothing has overflowed
                   | DisableWritesEnableReads
                   | EnableWritesDisableReads
                   deriving (Generic, NFDataX)

-- | This wrapper disables reads or writes when the elastic buffer
-- over-/underflows, as appropriate.
--
-- It also censors 'DataCount' after an over-/underflow, so that we do not take
-- measurements from an elastic buffer until it has settled back to its
-- midpoint.
ebController ::
  forall readDom writeDom.
  (KnownDomain readDom, KnownDomain writeDom) =>
  ElasticBufferSize ->
  Clock readDom ->
  Reset readDom ->
  Enable readDom ->
  Clock writeDom ->
  Reset writeDom ->
  Enable writeDom ->
  (Signal readDom DataCount, Signal readDom ResetClockControl)
ebController size clkRead rstRead enaRead clkWrite rstWrite enaWrite =
  unbundle
    (go <$> rdToggle <*> outRd <*> overflowRd)
 where
  (outRd, outWr) = elasticBuffer size clkRead clkWrite rdToggle wrToggle

  go True (dc, False) False = (dc, False)
  go _ (dc, _) _ = (dc, True)

  overflowRd =
    dualFlipFlopSynchronizer clkWrite clkRead rstRead enaRead False (snd <$> outWr)

  wrToggle = not <$> wrDisable; rdToggle = not <$> rdDisable

  wrDisable =
    dualFlipFlopSynchronizer clkRead clkWrite rstWrite enaWrite False wrDisableRd

  (wrDisableRd, rdDisable) = unbundle direct

  -- write reset process (so that is accurate in the read domain):
  --
  -- 1. disable writes (keep reads enabled), drain COMPLETELY (control from read
  -- domain?)
  -- 2. re-enable writes, disable reads until EXACTLY half (in the read domain)
  -- 3. (reads can proceed)

  direct :: Signal readDom (DisableWrites, DisableReads)
  direct =
    register clkRead rstRead enaRead (False, False)
      $ mealy clkRead rstRead enaRead f Wait (bundle (outRd, overflowRd))
   where
    f :: WrResetDomain -> ((DataCount, Underflow), Overflow) -> (WrResetDomain, (DisableWrites, DisableReads))
    f EnableWritesDisableReads ((d, _), _) | d == targetDataCount size = (Wait, (False, False))
    f EnableWritesDisableReads _ = (EnableWritesDisableReads, (False, True))
    f DisableWritesEnableReads ((0, _), _) = (EnableWritesDisableReads, (False, True))
    f DisableWritesEnableReads _ = (DisableWritesEnableReads, (True, False))
    f Wait ((_, True), _) = (EnableWritesDisableReads, (False, True))
    f Wait (_, True) = (DisableWritesEnableReads, (True, False))
    f Wait _ = (Wait, (False, False))

type Underflow = Bool
type Overflow = Bool

-- | Model FIFO. This is exposed as 'ebController'
--
-- Output signal exposes 'DataCount' and over-/underflow.
elasticBuffer ::
  forall readDom writeDom.
  (KnownDomain readDom, KnownDomain writeDom) =>
  -- | Size of FIFO. To reflect our target platforms, this should be a power of two
  -- where typical sizes would probably be: 16, 32, 64, 128.
  ElasticBufferSize ->
  Clock readDom ->
  Clock writeDom ->
  -- | Read enable
  Signal readDom Bool ->
  -- | Write enable
  Signal writeDom Bool ->
  (Signal readDom (DataCount, Underflow), Signal writeDom (DataCount, Overflow))
elasticBuffer size clkRead clkWrite readEna writeEna
  | Clock _ (Just readPeriods) <- clkRead
  , Clock _ (Just writePeriods) <- clkWrite
  = go 0 (targetDataCount size) readPeriods writePeriods readEna writeEna
 where
  go !relativeTime !fillLevel rps wps@(writePeriod :- _) =
    if relativeTime < toInteger writePeriod
      then goRead relativeTime fillLevel rps wps
      else goWrite relativeTime fillLevel rps wps

  goWrite relativeTime fillLevel rps (writePeriod :- wps) rdEna (wrEna :- wrEnas)
    | wrEna =
    second (next :-) $
      go (relativeTime - toInteger writePeriod) newFillLevel rps wps rdEna wrEnas
   where
    next@(newFillLevel, _)
      | fillLevel >= size = (targetDataCount size, True)
      | otherwise = (fillLevel + 1, False)

  goWrite relativeTime fillLevel rps (writePeriod :- wps) rdEna (_ :- wrEnas) =
    second ((fillLevel, False) :-) $
      go (relativeTime - toInteger writePeriod) fillLevel rps wps rdEna wrEnas

  goRead relativeTime fillLevel (readPeriod :- rps) wps (rdEna :- rdEnas) wrEnas
    | rdEna =
    first (next :-) $
      go (relativeTime + toInteger readPeriod) newFillLevel rps wps rdEnas wrEnas
   where
    next@(newFillLevel, _)
      | fillLevel <= 0 = (targetDataCount size, True)
      | otherwise = (fillLevel - 1, False)

  goRead relativeTime fillLevel (readPeriod :- rps) wps (_ :- rdEnas) wrEnas =
    first ((fillLevel, False) :-) $
      go (relativeTime + toInteger readPeriod) fillLevel rps wps rdEnas wrEnas

elasticBuffer size (Clock ss Nothing) clock1 readEna writeEna =
  -- Convert read clock to a "dynamic" clock if it isn't one
  let
    period = snatToNum (clockPeriod @readDom)
  in
    elasticBuffer size (Clock ss (Just (pure period))) clock1 readEna writeEna

elasticBuffer size clock0 (Clock ss Nothing) readEna writeEna =
  -- Convert write clock to a "dynamic" clock if it isn't one
  let
    period = snatToNum (clockPeriod @writeDom)
  in
    elasticBuffer size clock0 (Clock ss (Just (pure period))) readEna writeEna
