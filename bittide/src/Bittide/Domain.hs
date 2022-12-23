-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Bittide.Domain where


import Clash.Prelude hiding (DomainConfiguration)
import Bittide.ClockControl (SpeedChange, ClockControlConfig, targetDataCount)
import Clash.Cores.Xilinx.DcFifo
import Bittide.ClockControl.Callisto
import Bittide.ClockControl.StabilityChecker
import Bittide.ElasticBuffer
import Bittide.SharedTypes

import qualified Clash.Explicit.Prelude as E

data DomainConfiguration dom incomingDomains stabilityMargin stabilityCycles =
   1 <= stabilityCycles =>
  DomainConfiguration
    { clockControlConfig :: ClockControlConfig dom incomingDomains
    , stabilityConfig :: (SNat stabilityMargin, SNat stabilityCycles)
    }

data BootProcedure
  = AllUnstable
  | ResetBuffers
  | WaitForPasses
  | Stable
  deriving (Generic, NFDataX)

domain ::
  ( KnownDomain dom, KnownNat n, 1 <= n, KnownNat m, 1 <= m, n + m <= 32) =>
  DomainConfiguration dom m stabilityMargin stabilityCycles ->
  Clock dom ->
  Reset dom ->
  Enable dom ->
  Vec n (Signal dom (DataCount m)) ->
  Vec n (Signal dom EbMode) ->
  ( Signal dom SpeedChange
  , Signal dom BootProcedure)
domain DomainConfiguration{..} clkCallisto externalReset enableCallisto dataCounts ebModes =
  ( speedChange, bootState)
 where
  -- Active clock control
  speedChange = callistoClockControl clkCallisto callistoReset enableCallisto
    clockControlConfig callistoCounts

-- Reset callisto whenever one of the elastic buffers goes from Pass to any other mode.
  callistoReset = withClockResetEnable clkCallisto externalReset enableCallisto
    (forceReset $ holdTrue d3 (or <$> bundle (lostPass <$> ebModes)))
  lostPass ebMode = register Drain ebMode  .==. pure Pass .&&. ebMode ./=. pure Pass

  -- Pass targetDataCount to callisto when the elastic buffer is not in Pass mode.
  callistoCounts = (\ bool a -> mux bool a $ pure targetDataCount) <$> allPasses <*> dataCounts

  allPasses = fmap (fmap (== Pass)) ebModes
  -- Boot procedure

  -- The stabilityCheckers are in reset when the corresponding elastic buffer is not in
  -- Pass mode.
  (margin, cyclesStable) = stabilityConfig
  stabilityIndicators =
    (\ isPass ->
      withClockResetEnable clkCallisto (unsafeFromLowPolarity isPass) enableCallisto
      stabilityChecker margin cyclesStable
    ) <$> allPasses <*> dataCounts

  -- The state machine changes state depending on the stability indicators.
  bootState = E.mealy clkCallisto externalReset enableCallisto go AllUnstable $ bundle stabilityIndicators
  go currentState stableIndicatorsGo = (nextState, nextState)
   where
    nextState = case (currentState, and stableIndicatorsGo) of
      (AllUnstable   , True)  -> ResetBuffers
      (ResetBuffers  , False) -> WaitForPasses
      (WaitForPasses , True)  -> Stable
      (Stable        , _)     -> Stable
      _                       -> currentState
