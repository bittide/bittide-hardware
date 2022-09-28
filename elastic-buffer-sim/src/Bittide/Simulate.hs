{-|

Provides a rudimentary simulation of elastic buffers.

-}

-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

type ResetClockControl = Bool -- request a reset in other elastic buffers
type HasUnderflowed = Bool
type HasOverflowed = Bool
type DisableTilHalf = Bool
type ForceReset = Bool -- force a "reset" to EB midpoint
type PropagateReset = Bool
type ResetEbs = Bool

type DisableWrites = Bool
type DisableReads = Bool

data EbControlSt = Wait | Drain | Fill deriving (Generic, NFDataX)

data EbMarshalSt = ResetInProgress | Stable deriving (Generic, NFDataX)

ebReadDom ::
  forall readDom writeDom.
  (KnownDomain readDom, KnownDomain writeDom) =>
  Clock writeDom ->
  Clock readDom ->
  Reset readDom ->
  Reset writeDom ->
  Enable readDom ->
  Enable writeDom ->
  Signal readDom EbControlSt ->
  Signal readDom EbRdDom
ebReadDom wrClk rdClk rdRst wrRst rdEna wrEna rdCtrl =
  bundle (readCount, isUnderflow, ebRdOver)
 where
  ebRdOver = dualFlipFlopSynchronizer wrClk rdClk rdRst rdEna False isOverflow
  -- combinatorial loop here...
  FifoOut{..} = ebWrap rdClk rdRst rdToggle wrClk wrRst wrToggle
  wrToggle = dualFlipFlopSynchronizer rdClk wrClk wrRst wrEna True wrToggleRd

  (rdToggle, wrToggleRd) = unbundle (controlStToBools <$> rdCtrl)

  controlStToBools :: EbControlSt -> (Bool, Bool)
  controlStToBools Wait = (True, True)
  controlStToBools Drain = (True, False)
  controlStToBools Fill = (False, True)

data FifoOut readDom writeDom =
  FifoOut
    { isOverflow :: Signal writeDom Overflow
    , writeCount :: Signal writeDom DataCount
    , isUnderflow :: Signal readDom Underflow
    , readCount :: Signal readDom DataCount
    }

type EbRdDom = (DataCount, Underflow, Overflow)

data NodeInternalSt n = Continue | ResetState (Vec n EbControlSt) deriving (Generic, NFDataX)

-- | Marshal all the elastic buffers of a node together.
directEbs ::
  forall readDom n.
  (KnownDomain readDom, KnownNat n, 1 <= n) =>
  Clock readDom ->
  Reset readDom ->
  Enable readDom ->
  Vec n (Signal readDom EbControlSt -> Signal readDom EbRdDom) ->
  -- | Request a reset from clock control for the node
  (Signal readDom ForceReset, Vec n (Signal readDom DataCount))
directEbs clk rst ena ebs = (nodeRequestReset, dcs)
 where
  dcs = fmap (fmap fst3) ebsOut

  fst3 (x,_,_) = x

  -- ebsOut = imap (\i x -> x (dats !! i)) ebs where dats = unbundle marshalNodes
  -- ebsOut = imap (\i x -> (ebs !! i) x) dats where dats = unbundle marshalNodes
  ebsOut = zipWith ($) ebs (unbundle marshalNodes)

  -- output 'EbControlSt' to each node (given its status etc.)
  marshalNodes :: Signal readDom (Vec n EbControlSt)
  marshalNodes =
    delay clk ena (repeat Wait) $
      mealy clk rst ena go Continue (bundle ebsOut)
   where
    go :: NodeInternalSt n -> Vec n (DataCount, Underflow, Overflow) -> (NodeInternalSt n, Vec n EbControlSt)
    go Continue ebOuts
      | not (any underflowOrOverflow ebOuts) = (Continue, repeat Wait)
      | otherwise =
        let
          ctrls = fmap handleUO ebOuts
        in (ResetState ctrls, ctrls)
    go (ResetState ctrls) ebOuts
      | all atMidpoint ebOuts = (Continue, repeat Wait)
      | otherwise =
        let
          nextCtrls = zipWith stepNext ebOuts ctrls
        in (ResetState nextCtrls, nextCtrls)

  -- drain stops at 64 (read domain, fine)
  -- fill stops at 128 and goes to drain (so that it will be accurate count in
  -- the read domain)
  stepNext :: (DataCount, Underflow, Overflow) -> EbControlSt -> EbControlSt
  stepNext (64, _, _) Drain = Wait
  stepNext _ Drain = Drain
  stepNext (128, _, _) Fill = Drain
  stepNext _ Fill = Fill
  stepNext out _ = handleUO out

  -- request reset to node's clock controller
  nodeRequestReset :: Signal readDom ForceReset
  nodeRequestReset = mealy clk rst ena go Stable (bundle ebsOut)
   where
    go :: EbMarshalSt -> Vec n (DataCount, Underflow, Overflow) -> (EbMarshalSt, ForceReset)
    go Stable ebOuts | not (any underflowOrOverflow ebOuts) = (Stable, False)
                     | otherwise = (ResetInProgress, True)
    go ResetInProgress ebOuts | all atMidpoint ebOuts = (Stable, False)
                              | otherwise = (ResetInProgress, True)

  atMidpoint :: (DataCount, a, b) -> Bool
  atMidpoint (dc, _, _) = dc == 64

  underflowOrOverflow :: (a, Underflow, Overflow) -> Bool
  underflowOrOverflow (_, u, o) = u || o

  handleUO :: (a, Underflow, Overflow) -> EbControlSt
  handleUO (_, False, False) = Wait
  handleUO (_, True, _) = Fill
  handleUO (_, _, True) = Drain

ebWrap ::
  (KnownDomain readDom, KnownDomain writeDom) =>
  Clock readDom ->
  Reset readDom ->
  Signal readDom Bool ->
  Clock writeDom ->
  Reset writeDom ->
  Signal writeDom Bool ->
  FifoOut readDom writeDom
ebWrap rdClk _rdRst rdEna wrClk _wrRst wrEna =
  FifoOut{..}
 where
  (readCount, isUnderflow) = unbundle rdOuts
  (writeCount, isOverflow) = unbundle wrOuts

  (rdOuts, wrOuts) = elasticBuffer 128 rdClk wrClk rdEna wrEna

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

  goWrite relativeTime fillLevel rps (writePeriod :- wps) rdEna (True :- wrEnas) =
    second (next :-) $
      go (relativeTime - toInteger writePeriod) newFillLevel rps wps rdEna wrEnas
   where
    next@(newFillLevel, _)
      | fillLevel >= size = (size, True)
      | otherwise = (fillLevel + 1, False)

  goWrite relativeTime fillLevel rps (writePeriod :- wps) rdEna (_ :- wrEnas) =
    second ((fillLevel, False) :-) $
      go (relativeTime - toInteger writePeriod) fillLevel rps wps rdEna wrEnas

  goRead relativeTime fillLevel (readPeriod :- rps) wps (True :- rdEnas) wrEnas =
    first (next :-) $
      go (relativeTime + toInteger readPeriod) newFillLevel rps wps rdEnas wrEnas
   where
    next@(newFillLevel, _)
      | fillLevel <= 0 = (0, True)
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
