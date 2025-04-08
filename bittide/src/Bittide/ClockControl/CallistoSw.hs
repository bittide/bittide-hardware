-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Bittide.ClockControl.CallistoSw (
  callistoSwClockControl,
  SwControlConfig (..),
) where

import Clash.Prelude hiding (PeriodToCycles)

import Clash.Cores.Xilinx.Ila (Depth (..), IlaConfig (..), ila, ilaConfig)
import Data.Maybe (fromMaybe, isJust)
import Protocols
import Protocols.Idle
import VexRiscv (DumpVcd (NoDumpVcd), JtagIn)

import Bittide.CircuitUtils
import Bittide.ClockControl (RelDataCount)
import Bittide.ClockControl.Callisto.Types (CallistoResult (..), ReframingState (Done))
import Bittide.ClockControl.DebugRegister (
  DebugRegisterCfg (DebugRegisterCfg),
  debugRegisterWb,
 )
import Bittide.ClockControl.Registers (ClockControlData (..), clockControlWb)
import Bittide.DoubleBufferedRam (InitialContent (Undefined))
import Bittide.ProcessingElement (PeConfig (..), processingElement)
import Protocols.MemoryMap

-- | Configuration type for software clock control.
data SwControlConfig dom mgn fsz where
  SwControlConfig ::
    ( KnownNat mgn
    , KnownNat fsz
    , 1 <= fsz
    , KnownDomain dom
    ) =>
    -- | JTAG input to the CPU
    Signal dom JtagIn ->
    -- | Enable reframing?
    --
    -- N.B.: FOR TESTING USE ONLY. Reframing should eventually be handled solely within
    -- the clock control software. See issue #693.
    Signal dom Bool ->
    -- | Stability checker margin
    SNat mgn ->
    -- | Stability checker frame size
    SNat fsz ->
    SwControlConfig dom mgn fsz

{- | Instantiates a Vex RISC-V core ready to run the Callisto clock control algorithm.

The CPU is instantiated with 64KB of IMEM containing the 'clock-control' binary from
'firmware-binaries' and another 64KB of DMEM. The CPU is connected to the 'clockControlWb'
(@0xC000_0000@) and 'debugRegisterWb' (@0xE000_0000@) MMIO registers.
-}
callistoSwClockControl ::
  forall nLinks eBufBits dom margin framesize.
  ( HiddenClockResetEnable dom
  , KnownNat nLinks
  , KnownNat eBufBits
  , 1 <= nLinks
  , 1 <= eBufBits
  , nLinks + eBufBits <= 32
  , 1 <= framesize
  , 1 <= DomainPeriod dom
  ) =>
  -- | Clock control config
  SwControlConfig dom margin framesize ->
  -- | Availability mask
  Signal dom (BitVector nLinks) ->
  -- | Diff counters
  Vec nLinks (Signal dom (RelDataCount eBufBits)) ->
  Signal dom (CallistoResult nLinks)
callistoSwClockControl (SwControlConfig jtagIn reframe mgn fsz) mask ebs =
  hwSeqX callistoSwIla callistoResult
 where
  callistoResult =
    CallistoResult
      <$> ccData.clockMod
      <*> ccData.stabilityIndications
      <*> ccData.allStable
      <*> ccData.allSettled
      <*> resultRfs
      <*> jtag

  resultRfs = fromMaybe Done <$> debugData.reframingState

  callistoSwIla :: Signal dom ()
  callistoSwIla =
    setName @"callistoSwIla"
      $ ila
        ( ilaConfig
            $ "trigger_0"
            :> "capture_0"
            :> "probe_updatePeriod"
            :> "probe_updatePeriodMin"
            :> "probe_updatePeriodMax"
            :> Nil
        )
          { depth = D16384
          }
        hasClock
        (unsafeToActiveLow hasReset)
        capture
        debugData.updatePeriod
        debugData.updatePeriodMin
        debugData.updatePeriodMax

  debugRegisterCfg = DebugRegisterCfg <$> reframe

  capture = isRising False (isJust <$> ccData.clockMod)

  ((_mm, jtag), (ccData, debugData)) =
    circuitFn (((), jtagIn), (pure (), pure ()))

  Circuit circuitFn = circuit $ \(mm, jtag) -> do
    [ (prefixCc, (mmCc, wbClockControl))
      , (prefixDebug, (mmDebug, wbDebug))
      , (prefixDummy, (mmDummy, wbDummy))
      ] <-
      processingElement NoDumpVcd peConfig -< (mm, jtag)
    idleSink -< wbDummy
    constBwd 0b001 -< prefixDummy
    constBwd todoMM -< mmDummy
    [ccd0, ccd1] <- cSignalDupe <| clockControlWb mgn fsz mask ebs -< (mmCc, wbClockControl)
    constBwd 0b110 -< prefixCc
    cm <- cSignalMap clockMod -< ccd0
    dbg <- debugRegisterWb debugRegisterCfg -< (mmDebug, (wbDebug, cm))
    constBwd 0b111 -< prefixDebug
    idC -< (ccd1, dbg)

  peConfig =
    PeConfig
      { -- memMapConfig = 0b100 :> 0b010 :> 0b110 :> 0b111 :> 0b001 :> Nil
        initI = Undefined @(Div (64 * 1024) 4)
      , prefixI = 0b100
      , initD = Undefined @(Div (64 * 1024) 4)
      , prefixD = 0b010
      , iBusTimeout = d0 -- No timeouts on the instruction bus
      , dBusTimeout = d0 -- No timeouts on the data bus
      , includeIlaWb = True
      }
