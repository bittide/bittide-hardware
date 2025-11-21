-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Circuits for wall-clock synchronizing Bittide boards.

For debug purposes, we are sometimes interested in synchronizing the start of
tests on multiple boards or to distribute a global clock. To this end, our
designs under tests are connected to a SYNC_IN and SYNC_OUT pin and are connected
as follows:

    FPGA 0
    +------------+
    |    SYNC_IN |<--------+
    |            |         |
    |   SYNC_OUT |--X      |
    +------------+         |
                           |
    FPGA 1                 |
    +------------+         |
    |    SYNC_IN |<--------|
    |            |         |
    |   SYNC_OUT |--X      |
    +------------+         |
                           |
    FPGA 2                 |
    +------------+         |
    |    SYNC_IN |<--------+
    |            |         |
    |   SYNC_OUT |>--------+
    +------------+

I.e., only the last FPGA in the chain has its SYNC_OUT connected. A single pulse
can be used to synchronize the start of a test, while multiple pulses can be used
distribute a global clock.
-}
module Bittide.Sync where

import Clash.Explicit.Prelude hiding (PeriodToCycles)
import Protocols

import Bittide.Arithmetic.Time (PeriodToCycles)
import Bittide.SharedTypes (Bytes)
import Clash.Class.BitPackC (ByteOrder)
import Clash.Class.Counter (Counter (countSuccOverflow))
import Clash.Cores.Xilinx.Xpm (xpmCdcSingle, xpmCdcSyncRst)
import Clash.Cores.Xilinx.Xpm.Cdc.SyncRst (Asserted (..))
import Clash.Explicit.Signal.Extra (changepoints)
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (ConstBwd, MM)
import Protocols.MemoryMap.Registers.WishboneStandard (
  deviceWb,
  registerConfig,
  registerWb,
 )
import Protocols.Wishbone (Wishbone, WishboneMode (Standard))

type SyncOutGeneratorHalfPeriod = Milliseconds 5

{- | Count the number of SYNC_IN pulses. Also count the number of cycles since
the last SYNC_IN pulse. Note that this component will silently overflow.

You can directly connect the input to a pin. The incoming signal will be glitch
filtered and synchronized to the clock domain of the circuit. The glitch filter
is hardcoded to 128 clock cycles.
-}
syncInCounterC ::
  forall n dom.
  ( KnownDomain dom
  , HasSynchronousReset dom
  , KnownNat n
  , DomainInitBehavior dom ~ 'Defined
  ) =>
  Clock dom ->
  Reset dom ->
  Circuit
    ("sync_in" ::: CSignal dom Bit)
    ( "count" ::: CSignal dom (Unsigned n)
    , "cycles_since" ::: CSignal dom (Unsigned n)
    )
syncInCounterC clk rst = Circuit go
 where
  go (syncIn, _) = ((), (count, cyclesSince))
   where
    syncInFiltered =
      unsafeToActiveLow
        $ resetGlitchFilter (SNat @128) clk
        $ unsafeFromActiveLow
        $ xpmCdcSingle clk clk
        $ fmap bitToBool syncIn
    syncInChange = changepoints clk rst enableGen syncInFiltered
    syncInChangeRst = unsafeFromActiveHigh syncInChange
    countSince = toEnable (count ./=. pure 0)
    cyclesSince = register clk (rst `orReset` syncInChangeRst) countSince 0 (cyclesSince + 1)
    count = register clk rst (toEnable syncInChange) 0 (count + 1)

{- | Generate a clock signal that flips every @SyncOutGeneratorHalfPeriod@. This
is a registered output, i.e., it can directly be connected to a pin.
-}
syncOutGeneratorC ::
  forall dom.
  (KnownDomain dom) =>
  Clock dom ->
  Reset dom ->
  Circuit () (CSignal dom Bit)
syncOutGeneratorC clk rst = Circuit (const ((), boolToBit <$> syncOut))
 where
  syncOut = register clk rst (toEnable overflow) False (not <$> syncOut)
  (overflow, counter1) = unbundle (countSuccOverflow <$> counter0)
  start = 0 :: Index (PeriodToCycles dom SyncOutGeneratorHalfPeriod)
  counter0 = register clk rst enableGen start counter1

{- | Generate a clock signal that flips every @SyncOutGeneratorHalfPeriod@. This
is a registered output, i.e., it can directly be connected to a pin. On the
wishbone bus, a single register is exposed: whether this component is active.
-}
syncOutGenerateWbC ::
  forall dom counterDom aw.
  ( KnownDomain dom
  , HasSynchronousReset dom
  , HasSynchronousReset counterDom
  , HasCallStack
  , KnownNat aw
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Clock domain of the wishbone bus, typically the controlled domain.
  Clock dom ->
  Reset dom ->
  -- | Clock domain of the counter, typically the uncontrolled domain.
  Clock counterDom ->
  Reset counterDom ->
  Circuit
    (ConstBwd MM, Wishbone dom 'Standard aw (Bytes 4))
    (CSignal counterDom Bit)
syncOutGenerateWbC clk rst counterClk counterRst = circuit $ \(mm, wb) -> do
  [activeWb] <- deviceWb "SyncOutGenerator" -< (mm, wb)
  (Fwd active, _activity) <- registerWb clk rst config False -< (activeWb, Fwd noWrite)
  let
    syncOutRst0 = rst `orReset` unsafeFromActiveLow active
    syncOutRst1 = counterRst `orReset` xpmCdcSyncRst Asserted clk counterClk syncOutRst0
  syncOut <- syncOutGeneratorC counterClk syncOutRst1
  idC -< syncOut
 where
  config = registerConfig "active"
  noWrite = pure Nothing
