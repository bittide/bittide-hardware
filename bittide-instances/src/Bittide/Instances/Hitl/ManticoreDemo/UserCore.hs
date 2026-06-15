-- SPDX-FileCopyrightText: 2026 QBayLogic
--
-- SPDX-License-Identifier: Apache-2.0

{- | User core for the Manticore demo (milestone 1: a single Manticore chip on
one FPGA, seams tied off). It instantiates the 'manticoreBittideChip' blackbox
and exposes the chip's host interface — host registers, @start@, the device
registers, and the DMI gmem window (program-image load + trace readback) — as
Wishbone registers on the management unit's bus, so the MU CPU drives the
whole boot/run/trace flow over GDB (replacing the KCU105 JTAG-to-AXI host).

The chip does not touch the Bittide links here: the handshake TX is forwarded
verbatim to the GTH (as in the soft-UGN demo). Seam wiring is a later
milestone.

Wishbone layout (two MU busses):

  * @ManticoreControl@: schedule_config, gmem_base, trace_base (RW),
    start (RW, write-strobe), and the device registers + done/idle/clock_active
    (RO).
  * @ManticoreDmi@: dmi_addr (RW), dmi_wdata (RW, write-strobe), dmi_rdata (RO).
-}
module Bittide.Instances.Hitl.ManticoreDemo.UserCore (
  UserCoreBusses,
  RingBufferDepth,
  ringBufferDepth,
  mkUserCore,
) where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)
import Protocols

import Bittide.Instances.Domains (Bittide)
import Bittide.Instances.Hitl.GenericDemo.BringUp (NmuRemBusWidth, UserCoreCircuit)
import Bittide.Instances.Hitl.ManticoreDemo.Chip (
  ManticoreChipIn (..),
  ManticoreChipOut (..),
  ManticoreDeviceRegisters (..),
  ManticoreHostRegisters (..),
  manticoreBittideChip,
 )

import Clash.Class.BitPackC (ByteOrder)
import Clash.Prelude (HiddenClockResetEnable)
import Data.Maybe (isJust)
import Protocols.MemoryMap (Access (ReadOnly, ReadWrite))
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access),
  busActivityWrite,
  deviceConfig,
  deviceWbI,
  registerConfig,
  registerWbI,
  registerWbI_,
 )

-- | Two MU busses: one for control + device registers, one for the DMI window.
type UserCoreBusses = 2

{- | Ring-buffer depth of the (unused, milestone-1) link handshake path. Matches
the soft-UGN demo's value; the Manticore chip itself does not use it yet.
-}
type RingBufferDepth = 4000

ringBufferDepth :: SNat RingBufferDepth
ringBufferDepth = SNat

boolToBv32 :: Bool -> BitVector 32
boolToBv32 b = if b then 1 else 0

mkUserCore :: UserCoreCircuit UserCoreBusses (NmuRemBusWidth UserCoreBusses)
mkUserCore bitClk bitRst bitEna _localCounter _maybeDna _appReset =
  circuit $ \(userCoreBusses, _rxs2Raw, handshakeOut) -> do
    [controlBus, dmiBus] <- idC -< userCoreBusses

    -- ---- control + device registers ----
    [ wbSched
      , wbGmem
      , wbTrace
      , wbStart
      , wbVc
      , wbEid
      , wbEc
      , wbTh
      , wbBc
      , wbDi
      , wbCs
      , wbDone
      , wbIdle
      , wbCa
      ] <-
      withCRE (deviceWbI (deviceConfig "ManticoreControl")) -< controlBus

    (Fwd (schedV, _)) <-
      withCRE (registerWbI (rw "schedule_config") (0 :: BitVector 64)) -< (wbSched, Fwd (pure Nothing))
    (Fwd (gmemV, _)) <-
      withCRE (registerWbI (rw "gmem_base") (0 :: BitVector 64)) -< (wbGmem, Fwd (pure Nothing))
    (Fwd (traceV, _)) <-
      withCRE (registerWbI (rw "trace_base") (0 :: BitVector 64)) -< (wbTrace, Fwd (pure Nothing))
    (Fwd (_, startAct)) <-
      withCRE (registerWbI (rw "start") (0 :: BitVector 32)) -< (wbStart, Fwd (pure Nothing))

    -- ---- DMI window ----
    [wbDmiAddr, wbDmiWdata, wbDmiRdata] <-
      withCRE (deviceWbI (deviceConfig "ManticoreDmi")) -< dmiBus

    (Fwd (dmiAddrV, _)) <-
      withCRE (registerWbI (rw "dmi_addr") (0 :: BitVector 64)) -< (wbDmiAddr, Fwd (pure Nothing))
    (Fwd (dmiWdataV, dmiWdataAct)) <-
      withCRE (registerWbI (rw "dmi_wdata") (0 :: BitVector 16)) -< (wbDmiWdata, Fwd (pure Nothing))

    let
      hostRegs = ManticoreHostRegisters <$> schedV <*> gmemV <*> traceV
      startPulse = isJust . busActivityWrite <$> startAct
      dmiWenPulse = isJust . busActivityWrite <$> dmiWdataAct

      chipOut =
        manticoreBittideChip
          bitClk
          bitRst
          ManticoreChipIn
            { hostRegs
            , start = startPulse
            , dmiAddr = dmiAddrV
            , dmiWdata = dmiWdataV
            , dmiWen = dmiWenPulse
            }

      devRegs = chipOut.deviceRegs

    -- ---- device registers read back by the MU (read-only) ----
    withCRE (registerWbI_ (ro "virtual_cycles") (0 :: BitVector 64))
      -< (wbVc, Fwd (Just . (.virtualCycles) <$> devRegs))
    withCRE (registerWbI_ (ro "exception_id") (0 :: BitVector 32))
      -< (wbEid, Fwd (Just . (.exceptionId) <$> devRegs))
    withCRE (registerWbI_ (ro "execution_cycles") (0 :: BitVector 64))
      -< (wbEc, Fwd (Just . (.executionCycles) <$> devRegs))
    withCRE (registerWbI_ (ro "trace_dump_head") (0 :: BitVector 64))
      -< (wbTh, Fwd (Just . (.traceDumpHead) <$> devRegs))
    withCRE (registerWbI_ (ro "bootloader_cycles") (0 :: BitVector 32))
      -< (wbBc, Fwd (Just . (.bootloaderCycles) <$> devRegs))
    withCRE (registerWbI_ (ro "device_info") (0 :: BitVector 32))
      -< (wbDi, Fwd (Just . (.deviceInfo) <$> devRegs))
    withCRE (registerWbI_ (ro "clock_stalls") (0 :: BitVector 64))
      -< (wbCs, Fwd (Just . (.clockStalls) <$> devRegs))
    withCRE (registerWbI_ (ro "done") (0 :: BitVector 32))
      -< (wbDone, Fwd (Just . boolToBv32 <$> chipOut.done))
    withCRE (registerWbI_ (ro "idle") (0 :: BitVector 32))
      -< (wbIdle, Fwd (Just . boolToBv32 <$> chipOut.idle))
    withCRE (registerWbI_ (ro "clock_active") (0 :: BitVector 32))
      -< (wbCa, Fwd (Just . boolToBv32 <$> chipOut.clockActive))

    withCRE (registerWbI_ (ro "dmi_rdata") (0 :: BitVector 16))
      -< (wbDmiRdata, Fwd (Just <$> chipOut.dmiRdata))

    -- Milestone 1: forward the handshake TX verbatim; the chip does not drive
    -- the links yet.
    idC -< handshakeOut
 where
  withCRE :: forall r. ((HiddenClockResetEnable Bittide) => r) -> r
  withCRE = withClockResetEnable bitClk bitRst bitEna

  rw :: String -> RegisterConfig
  rw name = (registerConfig name){access = ReadWrite}

  ro :: String -> RegisterConfig
  ro name = (registerConfig name){access = ReadOnly}
