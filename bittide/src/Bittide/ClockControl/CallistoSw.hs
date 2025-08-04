-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.ClockControl.CallistoSw (
  callistoSwClockControlC,
  SwControlConfig (..),
  SwControlCConfig (..),
  SwcccRemBusWidth,
  SwcccInternalBusses,
) where

import Clash.Prelude hiding (PeriodToCycles)

import Clash.Class.BitPackC (ByteOrder)
import Protocols
import Protocols.Extra (splitAtCI)
import Protocols.Wishbone
import VexRiscv

import Bittide.ClockControl (RelDataCount)
import Bittide.ClockControl.Callisto.Types (
  CallistoCResult (CallistoCResult),
 )
import Bittide.ClockControl.DebugRegister (
  DebugRegisterCfg (DebugRegisterCfg),
  DebugRegisterData (reframingState),
  debugRegisterWb,
 )
import Bittide.ClockControl.Freeze (freeze)
import Bittide.ClockControl.Registers (ClockControlData (..), clockControlWb)
import Bittide.ProcessingElement (PeConfig (..), processingElement)
import Bittide.SharedTypes
import Bittide.Sync (syncInCounterC, syncOutGenerateWbC)
import Bittide.Wishbone (timeWb)
import Protocols.MemoryMap

-- | Configuration type for software clock control.
data SwControlConfig dom where
  SwControlConfig ::
    (KnownDomain dom) =>
    { jtagIn :: Signal dom JtagIn
    -- ^ JTAG input to the CPU
    , enableReframing :: Signal dom Bool
    -- ^ Enable reframing?
    --
    -- N.B.: FOR TESTING USE ONLY. Reframing should eventually be handled solely within
    -- the clock control software. See issue #693.
    } ->
    SwControlConfig dom

type SwcccInternalBusses = 7
type SwcccRemBusWidth n = 30 - CLog 2 (n + SwcccInternalBusses)

-- The additional 'otherWb' type parameter is necessary since this type helps expose
-- the Wishbone interconnect of the internal 'processingElement' so that other Wishbone
-- components may be connected to it. As such, the interconnect needs to know how many
-- other Wishbone ('otherWb') components are connected.
data SwControlCConfig otherWb where
  SwControlCConfig ::
    ( KnownNat otherWb
    ) =>
    { peConfig :: PeConfig (otherWb + SwcccInternalBusses)
    -- ^ Configuration for the internal 'processingElement'
    , ccRegPrefix :: Unsigned (CLog 2 (otherWb + SwcccInternalBusses))
    -- ^ Clock control register prefix
    , dbgRegPrefix :: Unsigned (CLog 2 (otherWb + SwcccInternalBusses))
    -- ^ Debug register prefix
    , timePrefix :: Unsigned (CLog 2 (otherWb + SwcccInternalBusses))
    -- ^ Time prefix
    , freezePrefix :: Unsigned (CLog 2 (otherWb + SwcccInternalBusses))
    -- ^ Freeze prefix
    , syncOutGeneratorPrefix :: Unsigned (CLog 2 (otherWb + SwcccInternalBusses))
    } ->
    SwControlCConfig otherWb

-- TODO: Make this the primary Callisto function once the reset logic is fixed
-- and Callisto is detached from the ILA plotting mechanisms.
callistoSwClockControlC ::
  forall nLinks eBufBits dom otherWb.
  ( HiddenClockResetEnable dom
  , KnownNat nLinks
  , KnownNat eBufBits
  , KnownNat otherWb
  , HasSynchronousReset dom
  , HasDefinedInitialValues dom
  , 1 <= nLinks
  , 1 <= eBufBits
  , nLinks + eBufBits <= 32
  , CLog 2 (otherWb + SwcccInternalBusses) <= 30
  , 1 <= DomainPeriod dom
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  , 4 <= SwcccRemBusWidth otherWb
  ) =>
  DumpVcd ->
  SwControlCConfig otherWb ->
  Circuit
    ( ConstBwd MM
    , ( "SYNC_IN" ::: CSignal dom Bit
      , Jtag dom
      , CSignal dom Bool -- reframing enable
      , CSignal dom (BitVector nLinks) -- link mask
      , CSignal dom (BitVector nLinks) -- what links are suitable for clock control
      , Vec nLinks (CSignal dom (RelDataCount eBufBits)) -- diff counters
      )
    )
    ( "SYNC_OUT" ::: CSignal dom Bit
    , CSignal dom (CallistoCResult nLinks)
    , Vec
        otherWb
        ( ConstBwd (Unsigned (CLog 2 (otherWb + SwcccInternalBusses)))
        , ( ConstBwd MM
          , Wishbone dom 'Standard (SwcccRemBusWidth otherWb) (Bytes 4)
          )
        )
    )
callistoSwClockControlC dumpVcd ccConfig =
  circuit $ \(mm, (syncIn, jtag, Fwd reframingEnabled, Fwd linkMask, Fwd linksOk, Fwd diffCounters)) -> do
    let
      debugRegisterCfg :: Signal dom DebugRegisterCfg
      debugRegisterCfg = DebugRegisterCfg <$> reframingEnabled

    allWishbone <- processingElement dumpVcd ccConfig.peConfig -< (mm, jtag)
    ( [ (clockControlPfx, clockControlBus)
        , (debugPfx, debugWbBus)
        , (timePfx, timeWbBus)
        , (freezePfx, freezeBus)
        , (syncOutGeneratorPfx, syncOutGeneratorBus)
        ]
      , wbRest
      ) <-
      splitAtCI -< allWishbone

    Fwd clockControlData <- clockControlWb linkMask linksOk diffCounters -< clockControlBus

    Fwd debugData <-
      debugRegisterWb debugRegisterCfg -< (debugWbBus, Fwd ((.clockMod) <$> clockControlData))

    freeze hasClock hasReset
      -< ( freezeBus
         , Fwd (bundle diffCounters)
         , localCounter
         , pulseCounter
         , cyclesSinceLastPulse
         )

    (pulseCounter, cyclesSinceLastPulse) <- syncInCounterC hasClock hasReset -< syncIn

    localCounter <- timeWb -< timeWbBus
    syncOut <- syncOutGenerateWbC hasClock hasReset -< syncOutGeneratorBus

    constBwd ccConfig.ccRegPrefix -< clockControlPfx
    constBwd ccConfig.dbgRegPrefix -< debugPfx
    constBwd ccConfig.timePrefix -< timePfx
    constBwd ccConfig.freezePrefix -< freezePfx
    constBwd ccConfig.syncOutGeneratorPrefix -< syncOutGeneratorPfx

    let
      callistoCResult :: Signal dom (CallistoCResult nLinks)
      callistoCResult =
        CallistoCResult
          <$> clockControlData.clockMod
          <*> clockControlData.stabilities
          <*> clockControlData.allStable
          <*> clockControlData.allSettled
          <*> debugData.reframingState

    idC -< (syncOut, Fwd callistoCResult, wbRest)
