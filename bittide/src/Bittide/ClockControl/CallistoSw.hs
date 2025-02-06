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
  callistoSwClockControlC,
  SwControlConfig (..),
  SwControlCConfig (..),
  SwcccRemBusWidth,
) where

import Clash.Prelude hiding (PeriodToCycles)

import Clash.Cores.Xilinx.Ila (Depth (..), IlaConfig (..), ila, ilaConfig)
import Data.Maybe (fromMaybe, isJust)
import Protocols
import Protocols.Idle
import Protocols.Wishbone
import VexRiscv

import Bittide.CircuitUtils
import Bittide.ClockControl (RelDataCount)
import Bittide.ClockControl.Callisto.Types (
  CallistoCResult (CallistoCResult),
  CallistoResult (CallistoResult),
  ReframingState (Done),
 )
import Bittide.ClockControl.DebugRegister (
  DebugRegisterCfg (DebugRegisterCfg),
  DebugRegisterData,
  debugRegisterWb,
 )
import Bittide.ClockControl.Registers (ClockControlData (..), clockControlWb)
import Bittide.DoubleBufferedRam (InitialContent (Undefined))
import Bittide.ProcessingElement (PeConfig (..), processingElement, splitAtC)
import Bittide.SharedTypes

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

  (jtag, (ccData, debugData)) =
    circuitFn (jtagIn, (pure (), pure ()))

  Circuit circuitFn = circuit $ \jtag -> do
    [wbClockControl, wbDebug, wbDummy] <- processingElement NoDumpVcd peConfig -< jtag
    idleSink -< wbDummy
    [ccd0, ccd1] <- cSignalDupe <| clockControlWb mgn fsz mask ebs -< wbClockControl
    cm <- cSignalMap clockMod -< ccd0
    dbg <- debugRegisterWb debugRegisterCfg -< (wbDebug, cm)
    idC -< (ccd1, dbg)

  peConfig =
    PeConfig
      { memMapConfig = 0b100 :> 0b010 :> 0b110 :> 0b111 :> 0b001 :> Nil
      , initI = Undefined @(Div (64 * 1024) 4)
      , initD = Undefined @(Div (64 * 1024) 4)
      , iBusTimeout = d0 -- No timeouts on the instruction bus
      , dBusTimeout = d0 -- No timeouts on the data bus
      , includeIlaWb = True
      }

type SwcccInternalBusses = 4
type SwcccRemBusWidth n = 30 - CLog 2 (n + SwcccInternalBusses)

data SwControlCConfig mgn fsz otherWb where
  SwControlCConfig ::
    ( KnownNat mgn
    , KnownNat fsz
    , KnownNat otherWb
    , 1 <= fsz
    ) =>
    SNat mgn ->
    SNat fsz ->
    PeConfig (otherWb + 4) ->
    SwControlCConfig mgn fsz otherWb

callistoSwClockControlC ::
  forall nLinks eBufBits dom margin framesize otherWb.
  ( HiddenClockResetEnable dom
  , KnownNat nLinks
  , KnownNat eBufBits
  , KnownNat otherWb
  , 1 <= nLinks
  , 1 <= eBufBits
  , nLinks + eBufBits <= 32
  , 1 <= framesize
  , CLog 2 (otherWb + SwcccInternalBusses) <= 30
  , 1 <= DomainPeriod dom
  ) =>
  DumpVcd ->
  SwControlCConfig margin framesize otherWb ->
  Circuit
    ( Jtag dom
    , CSignal dom Bool -- reframing enable
    , CSignal dom (BitVector nLinks) -- link mask
    , Vec nLinks (CSignal dom (RelDataCount eBufBits)) -- diff counters
    )
    ( CSignal dom (CallistoCResult nLinks)
    , Vec otherWb (Wishbone dom 'Standard (SwcccRemBusWidth otherWb) (Bytes 4))
    )
callistoSwClockControlC dumpVcd (SwControlCConfig mgn fsz peConfig) = Circuit go
 where
  go ::
    ( Fwd
        ( Jtag dom
        , CSignal dom Bool
        , CSignal dom (BitVector nLinks)
        , Vec nLinks (CSignal dom (RelDataCount eBufBits))
        )
    , Bwd
        ( CSignal dom (CallistoCResult nLinks)
        , Vec otherWb (Wishbone dom 'Standard (SwcccRemBusWidth otherWb) (Bytes 4))
        )
    ) ->
    ( Bwd
        ( Jtag dom
        , CSignal dom Bool
        , CSignal dom (BitVector nLinks)
        , Vec nLinks (CSignal dom (RelDataCount eBufBits))
        )
    , Fwd
        ( CSignal dom (CallistoCResult nLinks)
        , Vec otherWb (Wishbone dom 'Standard (SwcccRemBusWidth otherWb) (Bytes 4))
        )
    )
  go ((jtagIn, reframe, linkMask, diffCounters), (_, otherS2M)) =
    ( (jtagOut, pure (), pure (), repeat $ pure ())
    , (callistoCResult, otherM2S)
    )
   where
    debugRegisterCfg :: Signal dom DebugRegisterCfg
    debugRegisterCfg = DebugRegisterCfg <$> reframe

    peFn ::
      ( Fwd (Jtag dom)
      , Bwd
          ( CSignal dom (ClockControlData nLinks)
          , CSignal dom DebugRegisterData
          , Vec otherWb (Wishbone dom 'Standard (SwcccRemBusWidth otherWb) (Bytes 4))
          )
      ) ->
      ( Bwd (Jtag dom)
      , Fwd
          ( CSignal dom (ClockControlData nLinks)
          , CSignal dom DebugRegisterData
          , Vec otherWb (Wishbone dom 'Standard (SwcccRemBusWidth otherWb) (Bytes 4))
          )
      )
    Circuit peFn = circuit $ \jtag -> do
      allWishbone <- processingElement dumpVcd peConfig -< jtag
      ([wbClockControl, wbDebug], wbRest) <- splitAtC d2 -< allWishbone
      [ccd0, ccd1] <-
        cSignalDupe
          <| clockControlWb mgn fsz linkMask diffCounters
          -< wbClockControl
      cm <- cSignalMap clockMod -< ccd0
      dbg <- debugRegisterWb debugRegisterCfg -< (wbDebug, cm)
      idC -< (ccd1, dbg, wbRest)

    (jtagOut, (clockControlData, debugData, otherM2S)) = peFn (jtagIn, (pure (), pure (), otherS2M))

    resultRfs = fromMaybe Done <$> debugData.reframingState

    callistoCResult :: Signal dom (CallistoCResult nLinks)
    callistoCResult =
      CallistoCResult
        <$> clockControlData.clockMod
        <*> clockControlData.stabilityIndications
        <*> clockControlData.allStable
        <*> clockControlData.allSettled
        <*> resultRfs
