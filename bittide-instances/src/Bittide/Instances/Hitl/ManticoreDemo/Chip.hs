-- SPDX-FileCopyrightText: 2026 QBayLogic
--
-- SPDX-License-Identifier: Apache-2.0

{- | Clash blackbox for one Manticore chip (`ManticoreBittideChip`, emitted by
the manticore-hw Chisel generator with @-t bittide@). It is a plain-Verilog
@inst@ blackbox, exactly the pattern of
'Clash.Cores.Xilinx.SystemMonitor.sysMon' / the GTH core: a synthesis path
that instantiates the foreign module (its body provided to Vivado through the
test group's @externalHdl@), and a trivial Clash-simulation fallback.

The chip runs entirely on the Bittide clock (no MMCM — the compute array sits
behind a BUFGCE inside the chip). Its host-facing ports — host registers,
@start@/@done@/@idle@, the device registers, and the raw gmem host BRAM port
(program image load + trace readback) — are flat signals here; the demo's user
core ('Bittide.Instances.Hitl.ManticoreDemo.UserCore') maps them onto the
management unit's Wishbone bus (a memory-mapped gmem region), replacing the
JTAG-to-AXI host of the KCU105 standalone port.

NB: this is milestone 1 (single chip, seams tied off inside the chip). The
chip does not touch the Bittide links yet.
-}
module Bittide.Instances.Hitl.ManticoreDemo.Chip (
  ManticoreHostRegisters (..),
  ManticoreDeviceRegisters (..),
  ManticoreChipIn (..),
  ManticoreChipOut (..),
  GmemHostAddrBits,
  GmemHostWords,
  manticoreBittideChip,
) where

import Clash.Prelude

import Clash.Cores.Xilinx.Xpm.Cdc.Internal

import Bittide.Instances.Domains (Bittide)

{- | Width of the gmem host port's 32-bit-word address. The chip's gmem is
@gmemAddrBits = 16@ half-words (128 KiB); as 32-bit words that is
@16 - 1 = 15@ address bits. Keep in sync with @ManticoreBittideChip@'s
@gmemAddrBits@ in manticore-hw.
-}
type GmemHostAddrBits = 15

-- | Number of 32-bit words in the gmem host window (@2 ^ GmemHostAddrBits@).
type GmemHostWords = 2 ^ GmemHostAddrBits

{- | Host registers written by the management unit (mirror of the Chisel
@HostRegisters@ bundle, in declaration order).
-}
data ManticoreHostRegisters = ManticoreHostRegisters
  { scheduleConfig :: BitVector 64
  -- ^ @[63:56]@ CMD, @[55:0]@ CMD data (START / RESUME / FLUSH + timeout)
  , gmemBase :: BitVector 64
  -- ^ base address of the program image in gmem
  , traceBase :: BitVector 64
  -- ^ base address of the trace dump buffer in gmem
  }
  deriving (Generic, NFDataX)

{- | Device registers read back by the management unit (mirror of the Chisel
@DeviceRegisters@ bundle, in declaration order).
-}
data ManticoreDeviceRegisters = ManticoreDeviceRegisters
  { virtualCycles :: BitVector 64
  , bootloaderCycles :: BitVector 32
  , exceptionId :: BitVector 32
  , executionCycles :: BitVector 64
  , traceDumpHead :: BitVector 64
  , deviceInfo :: BitVector 32
  , clockStalls :: BitVector 64
  }
  deriving (Generic, NFDataX)

-- | Everything driven into the chip, all in the 'Bittide' domain.
data ManticoreChipIn = ManticoreChipIn
  { hostRegs :: Signal Bittide ManticoreHostRegisters
  , start :: Signal Bittide Bool
  -- ^ rising edge launches the configured command (the chip edge-detects it)
  , gmemEn :: Signal Bittide Bool
  -- ^ chip-enable for the raw gmem host BRAM port (port A)
  , gmemWe :: Signal Bittide (BitVector 4)
  -- ^ per-byte write enables for the gmem host port
  , gmemAddr :: Signal Bittide (BitVector GmemHostAddrBits)
  -- ^ 32-bit-word address into gmem
  , gmemDin :: Signal Bittide (BitVector 32)
  -- ^ write data for the gmem host port
  }

-- | Everything produced by the chip, all in the 'Bittide' domain.
data ManticoreChipOut = ManticoreChipOut
  { deviceRegs :: Signal Bittide ManticoreDeviceRegisters
  , done :: Signal Bittide Bool
  , idle :: Signal Bittide Bool
  , clockActive :: Signal Bittide Bool
  -- ^ diagnostics: compute clock currently enabled
  , gmemDout :: Signal Bittide (BitVector 32)
  -- ^ read data from the raw gmem host BRAM port (1-cycle read latency)
  }

{- | Instantiate one Manticore chip on the Bittide clock.

In synthesis this emits an instance of the foreign Verilog module
@ManticoreBittideChip@ (body supplied via @externalHdl@). In Clash simulation
the whole chip is NOT modelled — it is replaced by an idle stub (Manticore is
verified by the Chisel testers and on hardware, not in Clash simulation).
-}
manticoreBittideChip ::
  Clock Bittide ->
  Reset Bittide ->
  ManticoreChipIn ->
  ManticoreChipOut
manticoreBittideChip clk rst input
  | clashSimulation = simChip
  | otherwise = synthChip
 where
  ManticoreChipIn{hostRegs, start, gmemEn, gmemWe, gmemAddr, gmemDin} = input

  -- Clash-simulation stub: the chip sits idle and the gmem port reads zero.
  simChip =
    ManticoreChipOut
      { deviceRegs = pure (deepErrorX "ManticoreBittideChip: not modelled in Clash simulation")
      , done = pure False
      , idle = pure True
      , clockActive = pure False
      , gmemDout = pure 0
      }

  synthChip =
    ManticoreChipOut
      { deviceRegs =
          ManticoreDeviceRegisters
            <$> virtualCycles
            <*> bootloaderCycles
            <*> exceptionId
            <*> executionCycles
            <*> traceDumpHead
            <*> deviceInfo
            <*> clockStalls
      , done
      , idle
      , clockActive
      , gmemDout
      }
   where
    scheduleConfig = (.scheduleConfig) <$> hostRegs
    gmemBase = (.gmemBase) <$> hostRegs
    traceBase = (.traceBase) <$> hostRegs

    ( unPort @(Port "device_regs_virtual_cycles" Bittide (BitVector 64)) -> virtualCycles
      , unPort @(Port "device_regs_bootloader_cycles" Bittide (BitVector 32)) -> bootloaderCycles
      , unPort @(Port "device_regs_exception_id" Bittide (BitVector 32)) -> exceptionId
      , unPort @(Port "device_regs_execution_cycles" Bittide (BitVector 64)) -> executionCycles
      , unPort @(Port "device_regs_trace_dump_head" Bittide (BitVector 64)) -> traceDumpHead
      , unPort @(Port "device_regs_device_info" Bittide (BitVector 32)) -> deviceInfo
      , unPort @(Port "device_regs_clock_stalls" Bittide (BitVector 64)) -> clockStalls
      , unPort @(Port "ctrl_done" Bittide Bool) -> done
      , unPort @(Port "ctrl_idle" Bittide Bool) -> idle
      , unPort @(Port "clock_active" Bittide Bool) -> clockActive
      , unPort @(Port "gmem_host_dout" Bittide (BitVector 32)) -> gmemDout
      ) = go

    go =
      inst
        (instConfig "ManticoreBittideChip")
        (ClockPort @"clk" clk)
        (ResetPort @"rst" @ActiveHigh rst)
        (Port @"host_regs_schedule_config" scheduleConfig)
        (Port @"host_regs_global_memory_instruction_base" gmemBase)
        (Port @"host_regs_trace_dump_base" traceBase)
        (Port @"ctrl_start" start)
        (Port @"gmem_host_en" gmemEn)
        (Port @"gmem_host_we" gmemWe)
        (Port @"gmem_host_addr" gmemAddr)
        (Port @"gmem_host_din" gmemDin)
