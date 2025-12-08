-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.ClockControl.CallistoSw (
  callistoSwClockControlC,
  SwControlConfig (..),
  SwcccInternalBusses,
) where

import Clash.Prelude hiding (PeriodToCycles)

import Clash.Class.BitPackC (ByteOrder)
import Clash.Functor.Extra ((<<$>>))
import Protocols
import Protocols.Wishbone
import VexRiscv

import Bittide.ClockControl.Callisto.Types (
  CallistoResult (CallistoResult),
 )
import Bittide.ClockControl.Freeze (freeze)
import Bittide.ClockControl.Registers (ClockControlData (..), clockControlWb)
import Bittide.Counter (domainDiffCountersWbC)
import Bittide.ProcessingElement
import Bittide.SharedTypes
import Bittide.Sync (Sync, syncInCounterC, syncOutGenerateWbC, toSync)
import Bittide.Wishbone (arbiterMm, extendAddressWidthWbMm, timeWb)
import Protocols.MemoryMap
import Protocols.Wishbone.Extra (delayWishboneMm)

import qualified Protocols.Vec as Vec

-- | Configuration type for software clock control.
data SwControlConfig dom where
  SwControlConfig ::
    (KnownDomain dom) =>
    { jtagIn :: Signal dom JtagIn
    -- ^ JTAG input to the CPU
    } ->
    SwControlConfig dom

type SwcccInternalBusses = 7
type SwcccRemBusWidth n = 30 - PrefixWidth (n + SwcccInternalBusses)

-- TODO: Make this the primary Callisto function once the reset logic is fixed
-- and Callisto is detached from the ILA plotting mechanisms.
callistoSwClockControlC ::
  forall nLinks dom free rx otherWb otherWbMu.
  ( HiddenClockResetEnable dom
  , KnownNat nLinks
  , KnownNat otherWb
  , KnownNat otherWbMu
  , HasSynchronousReset dom
  , HasSynchronousReset free
  , HasSynchronousReset rx
  , HasDefinedInitialValues dom
  , PrefixWidth (otherWb + SwcccInternalBusses) <= 30
  , 1 <= DomainPeriod dom
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  , 4 <= SwcccRemBusWidth otherWb
  ) =>
  -- | Clock of an uncontrolled domain, e.g. the free-running clock. This is
  -- used to generate the SYNC_OUT signal.
  Clock free ->
  Reset free ->
  -- | Clocks from the incoming links. Used to construct domain difference
  -- counters.
  Vec nLinks (Clock rx) ->
  Vec nLinks (Reset rx) ->
  DumpVcd ->
  PeConfig (otherWb + SwcccInternalBusses) ->
  Circuit
    ( ToConstBwd Mm
    , -- Management unit
      ( ToConstBwd Mm
      , Wishbone dom 'Standard otherWbMu (Bytes 4)
      )
    , ( Jtag dom
      , CSignal dom (BitVector nLinks) -- link mask
      , CSignal dom (BitVector nLinks) -- what links are suitable for clock control
      )
    )
    ( Sync dom free
    , CSignal dom (CallistoResult nLinks)
    , Vec
        otherWb
        ( ToConstBwd Mm
        , Wishbone dom 'Standard (SwcccRemBusWidth otherWb) (Bytes 4)
        )
    )
callistoSwClockControlC freeClk freeRst rxClocks rxResets dumpVcd peConfig =
  circuit $ \(mm, muClockControlBus, (jtag, Fwd linkMask, Fwd linksOk)) -> do
    allWishbone <- processingElement dumpVcd peConfig -< (mm, jtag)
    ( [ ccClockControlBus
        , timeWbBus
        , freezeBus
        , syncOutGeneratorBus
        , domainDiffsBus
        ]
      , wbRest
      ) <-
      Vec.split -< allWishbone

    Fwd clockControlData <-
      clockControlWb linkMask linksOk (unbundle diffCounters) -< clockControlBus

    clockControlBus <- arbiterMm -< [ccClockControlBusWide, muClockControlBusWide]
    -- We need to extend the width of both wishbone busses since we don't know which
    -- is wider.
    ccClockControlBusWide <-
      extendAddressWidthWbMm @(Max (SwcccRemBusWidth otherWb) otherWbMu)
        -< ccClockControlBus
    muClockControlBusWide <-
      extendAddressWidthWbMm @(Max (SwcccRemBusWidth otherWb) otherWbMu)
        <| delayWishboneMm
        -< muClockControlBus

    freeze hasClock hasReset
      -< ( freezeBus
         , Fwd diffCounters
         , localCounter
         , pulseCounter
         , cyclesSinceLastPulse
         )

    (syncIn, sync) <- toSync -< syncOut

    (pulseCounter, cyclesSinceLastPulse) <- syncInCounterC hasClock hasReset -< syncIn

    localCounter <- timeWb -< timeWbBus
    syncOut <- syncOutGenerateWbC hasClock hasReset freeClk freeRst -< syncOutGeneratorBus
    Fwd domainDiffs <-
      domainDiffCountersWbC rxClocks rxResets hasClock hasReset -< domainDiffsBus

    let diffCounters = fst <<$>> domainDiffs

    let
      callistoCResult :: Signal dom (CallistoResult nLinks)
      callistoCResult =
        CallistoResult
          <$> clockControlData.clockMod
          <*> clockControlData.stabilities
          <*> clockControlData.allStable
          <*> clockControlData.allSettled

    idC -< (sync, Fwd callistoCResult, wbRest)
