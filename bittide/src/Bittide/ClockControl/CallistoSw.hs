-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Bittide.ClockControl.CallistoSw (
  callistoSwClockControl,
  callistoSwClockControlC,
  SwControlConfig (..),
  SwControlCConfig (..),
  SwcccRemBusWidth,
) where

import Clash.Prelude hiding (PeriodToCycles)

import BitPackC (ByteOrder)
import Clash.Cores.Xilinx.Ila (Depth (..), IlaConfig (..), ila, ilaConfig)
import Data.Maybe (isJust)
import Protocols
import Protocols.Extra (cSignalMap, replicateCSignalI, splitAtCI)
import Protocols.Wishbone
import VexRiscv

import Bittide.ClockControl (RelDataCount)
import Bittide.ClockControl.Callisto.Types (
  CallistoCResult (CallistoCResult),
  CallistoResult (CallistoResult),
 )
import Bittide.ClockControl.DebugRegister (
  DebugRegisterCfg (DebugRegisterCfg),
  debugRegisterWb,
 )
import Bittide.ClockControl.Registers (ClockControlData (..), clockControlWb)
import Bittide.DoubleBufferedRam (InitialContent (Undefined))
import Bittide.ProcessingElement (PeConfig (..), PeInternalBusses, processingElement)
import Bittide.SharedTypes
import Bittide.Wishbone (timeWb)
import Protocols.MemoryMap

-- | Configuration type for software clock control.
data SwControlConfig dom mgn fsz where
  SwControlConfig ::
    ( KnownNat mgn
    , KnownNat fsz
    , 1 <= fsz
    , KnownDomain dom
    ) =>
    { jtagIn :: Signal dom JtagIn
    -- ^ JTAG input to the CPU
    , enableReframing :: Signal dom Bool
    -- ^ Enable reframing?
    --
    -- N.B.: FOR TESTING USE ONLY. Reframing should eventually be handled solely within
    -- the clock control software. See issue #693.
    , margin :: SNat mgn
    -- ^ Stability checker margin
    , framesize :: SNat fsz
    -- ^ Stability checker frame size
    } ->
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
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Clock control config
  SwControlConfig dom margin framesize ->
  -- | Availability mask
  Signal dom (BitVector nLinks) ->
  -- | Diff counters
  Vec nLinks (Signal dom (RelDataCount eBufBits)) ->
  (MM, Signal dom (CallistoResult nLinks))
callistoSwClockControl (ccConfig@SwControlConfig{framesize}) mask ebs =
  hwSeqX callistoSwIla (mm, callistoResult)
 where
  callistoResult =
    CallistoResult
      <$> ccData.clockMod
      <*> ccData.stabilityIndications
      <*> ccData.allStable
      <*> ccData.allSettled
      <*> resultRfs
      <*> jtag

  resultRfs = debugData.reframingState

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

  debugRegisterCfg = DebugRegisterCfg <$> ccConfig.enableReframing

  capture = isRising False (isJust <$> ccData.clockMod)

  ((mm, jtag), (ccData, debugData)) =
    circuitFn (((), ccConfig.jtagIn), (pure (), pure ()))

  Circuit circuitFn = circuit $ \(mm, jtag) -> do
    [ (prefixCc, (mmCc, wbClockControl))
      , (prefixDebug, debugWbBus)
      , (timePfx, timeWbBus)
      ] <-
      processingElement NoDumpVcd peConfig -< (mm, jtag)
    _localCounter <- timeWb -< timeWbBus
    constBwd 0b011 -< timePfx
    [ccd0, ccd1] <-
      replicateCSignalI
        <| clockControlWb ccConfig.margin framesize mask ebs
        -< (mmCc, wbClockControl)
    constBwd 0b110 -< prefixCc
    cm <- cSignalMap clockMod -< ccd0
    dbg <- debugRegisterWb debugRegisterCfg -< (debugWbBus, cm)
    constBwd 0b101 -< prefixDebug
    idC -< (ccd1, dbg)

  peConfig =
    PeConfig
      { initI = Undefined @(Div (64 * 1024) 4)
      , prefixI = 0b100
      , initD = Undefined @(Div (64 * 1024) 4)
      , prefixD = 0b010
      , iBusTimeout = d0 -- No timeouts on the instruction bus
      , dBusTimeout = d0 -- No timeouts on the data bus
      , includeIlaWb = True
      , whoAmID = 0x6363_7773
      , whoAmIPrefix = 0b111
      }

type SwcccInternalBusses = PeInternalBusses + 3
type SwcccRemBusWidth n = 30 - CLog 2 (n + SwcccInternalBusses)

-- The additional 'otherWb' type parameter is necessary since this type helps expose
-- the Wishbone interconnect of the internal 'processingElement' so that other Wishbone
-- components may be connected to it. As such, the interconnect needs to know how many
-- other Wishbone ('otherWb') components are connected.
data SwControlCConfig mgn fsz otherWb where
  SwControlCConfig ::
    ( KnownNat mgn
    , KnownNat fsz
    , KnownNat otherWb
    , 1 <= fsz
    ) =>
    { margin :: SNat mgn
    -- ^ Stability checker margin
    , framesize :: SNat fsz
    -- ^ Stability checker frame size
    , peConfig :: PeConfig (otherWb + SwcccInternalBusses)
    -- ^ Configuration for the internal 'processingElement'
    , ccRegPrefix :: Unsigned (CLog 2 (otherWb + SwcccInternalBusses))
    -- ^ Clock control register prefix
    , dbgRegPrefix :: Unsigned (CLog 2 (otherWb + SwcccInternalBusses))
    -- ^ Debug register prefix
    , timePrefix :: Unsigned (CLog 2 (otherWb + SwcccInternalBusses))
    -- ^ Time prefix
    } ->
    SwControlCConfig mgn fsz otherWb

-- TODO: Make this the primary Callisto function once the reset logic is fixed
-- and Callisto is detached from the ILA plotting mechanisms.
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
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  DumpVcd ->
  SwControlCConfig margin framesize otherWb ->
  Circuit
    ( ConstBwd MM
    , ( Jtag dom
      , CSignal dom Bool -- reframing enable
      , CSignal dom (BitVector nLinks) -- link mask
      , Vec nLinks (CSignal dom (RelDataCount eBufBits)) -- diff counters
      )
    )
    ( CSignal dom (CallistoCResult nLinks)
    , Vec
        otherWb
        ( ConstBwd (Unsigned (CLog 2 (otherWb + SwcccInternalBusses)))
        , ( ConstBwd MM
          , Wishbone dom 'Standard (SwcccRemBusWidth otherWb) (Bytes 4)
          )
        )
    )
callistoSwClockControlC dumpVcd ccConfig@SwControlCConfig{framesize} =
  circuit $ \(mm, (jtag, Fwd reframingEnabled, Fwd linkMask, Fwd diffCounters)) -> do
    let
      debugRegisterCfg :: Signal dom DebugRegisterCfg
      debugRegisterCfg = DebugRegisterCfg <$> reframingEnabled

    allWishbone <- processingElement dumpVcd ccConfig.peConfig -< (mm, jtag)
    ( [ (clockControlPfx, clockControlBus)
        , (debugPfx, debugWbBus)
        , (timePfx, timeWbBus)
        ]
      , wbRest
      ) <-
      splitAtCI -< allWishbone

    Fwd clockControlData <-
      clockControlWb ccConfig.margin framesize linkMask diffCounters -< clockControlBus

    Fwd debugData <-
      debugRegisterWb debugRegisterCfg -< (debugWbBus, Fwd ((.clockMod) <$> clockControlData))

    _localCounter <- timeWb -< timeWbBus

    constBwd ccConfig.ccRegPrefix -< clockControlPfx
    constBwd ccConfig.dbgRegPrefix -< debugPfx
    constBwd ccConfig.timePrefix -< timePfx

    let
      callistoCResult :: Signal dom (CallistoCResult nLinks)
      callistoCResult =
        CallistoCResult
          <$> clockControlData.clockMod
          <*> clockControlData.stabilityIndications
          <*> clockControlData.allStable
          <*> clockControlData.allSettled
          <*> debugData.reframingState

    idC -< (Fwd callistoCResult, wbRest)
