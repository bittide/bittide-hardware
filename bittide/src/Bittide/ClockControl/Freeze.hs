-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.ClockControl.Freeze where

import Bittide.SharedTypes (Bytes)
import Clash.Class.BitPackC (ByteOrder)
import Clash.Explicit.Prelude
import GHC.Stack (HasCallStack)
import Protocols
import Protocols.MemoryMap (Access (ReadOnly, WriteOnly), ConstBwd, MM)
import Protocols.MemoryMap.Registers.WishboneStandard (
  BusActivity (BusWrite),
  RegisterConfig (access, description),
  deviceWbC,
  registerConfig,
  registerWbC,
  registerWbC_,
 )
import Protocols.Wishbone

{- | Component that can freeze a bunch of incoming signals related to clock
control measurements. This makes sure the clock control algorithm works on
measurements that are taken at the same clock cycle.
-}
freeze ::
  forall aw nLinks ebCounterWidth dom.
  ( KnownDomain dom
  , KnownNat aw
  , KnownNat ebCounterWidth
  , KnownNat nLinks
  , HasCallStack
  , 4 <= aw
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  Clock dom ->
  Reset dom ->
  Circuit
    ( ( ConstBwd MM
      , Wishbone dom 'Standard aw (Bytes 4)
      )
    , "elastic_buffer_counters" ::: CSignal dom (Vec nLinks (Signed ebCounterWidth))
    , "local_clock_counter" ::: CSignal dom (Unsigned 64)
    , "sync_in_counter" ::: CSignal dom (Unsigned 32)
    , "cycles_since_sync_in" ::: CSignal dom (Unsigned 32)
    )
    ()
freeze clk rst =
  circuit $ \((mm, wb), ebCounters, localCounter, syncPulseCounter, lastPulseCounter) -> do
    -- Create a bunch of register wishbone interfaces. We don't really care about
    -- ordering, so we just append a number to the end of a generic name.
    [wb0, wb1, wb2, wb3, wb4, wb5] <-
      deviceWbC "Freeze" -< (mm, wb)

    -- Only writeable register in this device: can be used by the wishbone manager
    -- to freeze all the incoming signals.
    --
    -- TODO: Delay acknowledgement until 'freeze_counter' is updated. We currently
    --       don't expect this to cause any problems as we don't critically rely
    --       on the counter being exact (i.e., reading one less just means we
    --       drop one measurement at the end of a clock control experiment). Still,
    --       it would be nice for 'registerWbC' to support this.
    (_a0, Fwd freezeActivity) <- registerWbC clk rst freezeConfig False -< (wb0, Fwd noWrite)
    let shut = freezeActivity .==. pure (BusWrite True)

    -- Read-only register that counts how many times the user requested a freeze
    let freezeCounterWrite = Just <$> counter @(Unsigned 32) clk rst (toEnable shut) 0
    registerWbC_ clk rst freezeCounterConfig 0 -< (wb1, Fwd freezeCounterWrite)

    -- Counters that are frozen when the user requests a freeze.
    localClockWrite <- shutter shut -< localCounter
    registerWbC_ clk rst localClockConfig 0 -< (wb2, localClockWrite)

    syncPulseCounterWrite <- shutter shut -< syncPulseCounter
    registerWbC_ clk rst syncCounterConfig 0 -< (wb3, syncPulseCounterWrite)

    lastPulseWrite <- shutter shut -< lastPulseCounter
    registerWbC_ clk rst lastSyncPulseConfig 0 -< (wb4, lastPulseWrite)

    ebcWrite <- shutter shut -< ebCounters
    registerWbC_ clk rst ebCountersConfig (repeat 0) -< (wb5, ebcWrite)

    idC
 where
  freezeConfig =
    (registerConfig "freeze")
      { access = WriteOnly
      , description = "Freeze all counters"
      }

  ebCountersConfig =
    (registerConfig "eb_counters")
      { access = ReadOnly
      , description =
          "Elastic buffer counters (at last freeze). Note that this is coming from domain difference counters, not actual elastic buffers."
      }

  lastSyncPulseConfig =
    (registerConfig "cycles_since_sync_pulse")
      { access = ReadOnly
      , description = "Number of clock cycles since last synchronization pulse (at last freeze)"
      }

  syncCounterConfig =
    (registerConfig "number_of_sync_pulses_seen")
      { access = ReadOnly
      , description = "Number of synchronization pulses seen (at last freeze)"
      }

  localClockConfig =
    (registerConfig "local_clock_counter")
      { access = ReadOnly
      , description = "Clock counter of local clock domain (at last freeze)"
      }

  freezeCounterConfig =
    (registerConfig "freeze_counter")
      { access = ReadOnly
      , description =
          "Number of times a freeze has been requested. This takes a couple of cycles to update after a freeze has been issued."
      }

  noWrite = pure Nothing

{- | Only lets through the signal @CSignal dom a@ when the the input signal is
-- 'True'. Otherwise, it returns 'Nothing'.
-}
shutter ::
  forall dom a.
  (KnownDomain dom) =>
  -- | Shutter control. If 'True', the signal is let through. If 'False', the
  -- signal is blocked.
  Signal dom Bool ->
  Circuit
    (CSignal dom a)
    (CSignal dom (Maybe a))
shutter shut = Circuit go
 where
  go (a, _) = (units, mux shut (Just <$> a) (pure Nothing))

-- | Simple counter
counter ::
  forall a dom.
  (KnownDomain dom, Num a, NFDataX a) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  a ->
  Signal dom a
counter clk rst ena ini = let c = register clk rst ena ini (c + 1) in c
