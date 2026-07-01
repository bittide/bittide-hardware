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
  SeamIn (..),
  SeamOut (..),
  SeamFrameBits,
  SeamFrame (..),
  NoCPacket (..),
  toSeamFrame,
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

{- | Width of one seam (TDM-serialized chip-edge) frame, flattened from the
Chisel @TdmFrame@: @1 valid + ceil(log2 (2*nLinks)) tag + ceil(log2 (2*nLinks+2))
age + NoCBundle@. For a 4x4 chip in an 8x16 torus (nLinks = 4, NoCBundle =
16 data + 11 addr + 1 valid + 4 xHops + 5 yHops = 37) that is
@1 + 3 + 4 + 37 = 45@. Keep in sync with @ManticoreBittideChip@'s seam pins
(it is <= 64, so one edge rides one Bittide link).
-}
type SeamFrameBits = 45

{- | The NoC packet carried inside a seam frame — a Clash mirror of the Chisel
@NoCBundle@ for the GLOBAL @8x16@ torus (DimX=8 ⇒ 4 @xHops@ bits, DimY=16 ⇒ 5
@yHops@ bits). Fields are in Chisel declaration order (first field = most
significant), so the derived 'BitPack' packs bit-for-bit like the Verilog seam
port: @data[36:21], address[20:10], valid[9], xHops[8:5], yHops[4:0]@.
-}
data NoCPacket = NoCPacket
  { npData :: BitVector 16
  , npAddress :: BitVector 11
  , npValid :: Bit
  -- ^ bit 9 of the frame — the real "a NoC message is present" flag
  , npXHops :: BitVector 4
  , npYHops :: BitVector 5
  }
  deriving (Generic, NFDataX, BitPack)

{- | One TDM seam frame — a Clash mirror of the Chisel @TdmFrame@ (@nBanks =
2*nLinks = 8@ ⇒ 3 @tag@ bits, 4 @age@ bits). The derived 'BitPack' produces the
45-bit 'SeamFrameBits' word that rides one Bittide link, with @sfValid@ at the MSB
(bit 44). Prefer this over slicing raw bit indices out of a 'BitVector': the field
names document the layout and 'toSeamFrame' stays in sync by construction.
-}
data SeamFrame = SeamFrame
  { sfValid :: Bit
  {- ^ bit 44: this TDM slot is transmitting an occupied bank (frame present).
  Only asserted while the edge is @extend@ed (connected) — see 'TdmLink'.
  -}
  , sfTag :: BitVector 3
  -- ^ bits 43..41: which logical link (bank) this frame carries
  , sfAge :: BitVector 4
  -- ^ bits 40..37: cycles the packet waited in its input bank
  , sfPacket :: NoCPacket
  -- ^ bits 36..0: the NoC packet
  }
  deriving (Generic, NFDataX, BitPack)

{- | Decode a raw 45-bit seam word into a typed 'SeamFrame'. This is just 'unpack',
but the type ascription makes it a compile-time check that @'BitSize' 'SeamFrame' ==
'SeamFrameBits'@ — if the record layout ever drifts from the 45-bit port it will not
compile.
-}
toSeamFrame :: BitVector SeamFrameBits -> SeamFrame
toSeamFrame = unpack

{- | One chip edge's seam inputs. @extend@ is high iff this edge is wired to a
neighbour chip (set by the chip's grid position); a low edge U-turns inside the
chip and its @tx@ is idle. @rx@ is the 'TdmFrame' arriving from the neighbour's
Bittide link.
-}
data SeamIn = SeamIn
  { extend :: Signal Bittide Bool
  , rx :: Signal Bittide (BitVector SeamFrameBits)
  }

{- | One chip edge's seam outputs: the 'TdmFrame' to transmit on this edge's
Bittide link, and a demux-overflow (data-loss) diagnostic.
-}
data SeamOut = SeamOut
  { tx :: Signal Bittide (BitVector SeamFrameBits)
  , overflow :: Signal Bittide Bool
  }

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
  , seamInE :: SeamIn
  , seamInW :: SeamIn
  , seamInN :: SeamIn
  , seamInS :: SeamIn
  -- ^ per-edge seam inputs (extend + rx); a multi-chip build only
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
  , seamOutE :: SeamOut
  , seamOutW :: SeamOut
  , seamOutN :: SeamOut
  , seamOutS :: SeamOut
  -- ^ per-edge seam outputs (tx + overflow)
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
  ManticoreChipIn
    { hostRegs
    , start
    , gmemEn
    , gmemWe
    , gmemAddr
    , gmemDin
    , seamInE
    , seamInW
    , seamInN
    , seamInS
    } = input

  idleSeamOut = SeamOut{tx = pure 0, overflow = pure False}

  -- Clash-simulation stub: the chip sits idle and the gmem port reads zero.
  simChip =
    ManticoreChipOut
      { deviceRegs = pure (deepErrorX "ManticoreBittideChip: not modelled in Clash simulation")
      , done = pure False
      , idle = pure True
      , clockActive = pure False
      , gmemDout = pure 0
      , seamOutE = idleSeamOut
      , seamOutW = idleSeamOut
      , seamOutN = idleSeamOut
      , seamOutS = idleSeamOut
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
      , seamOutE = SeamOut{tx = seamEastTx, overflow = seamEastOvf}
      , seamOutW = SeamOut{tx = seamWestTx, overflow = seamWestOvf}
      , seamOutN = SeamOut{tx = seamNorthTx, overflow = seamNorthOvf}
      , seamOutS = SeamOut{tx = seamSouthTx, overflow = seamSouthOvf}
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
      , unPort @(Port "seam_east_tx" Bittide (BitVector SeamFrameBits)) -> seamEastTx
      , unPort @(Port "seam_east_overflow" Bittide Bool) -> seamEastOvf
      , unPort @(Port "seam_west_tx" Bittide (BitVector SeamFrameBits)) -> seamWestTx
      , unPort @(Port "seam_west_overflow" Bittide Bool) -> seamWestOvf
      , unPort @(Port "seam_north_tx" Bittide (BitVector SeamFrameBits)) -> seamNorthTx
      , unPort @(Port "seam_north_overflow" Bittide Bool) -> seamNorthOvf
      , unPort @(Port "seam_south_tx" Bittide (BitVector SeamFrameBits)) -> seamSouthTx
      , unPort @(Port "seam_south_overflow" Bittide Bool) -> seamSouthOvf
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
        (Port @"seam_east_extend" seamInE.extend)
        (Port @"seam_east_rx" seamInE.rx)
        (Port @"seam_west_extend" seamInW.extend)
        (Port @"seam_west_rx" seamInW.rx)
        (Port @"seam_north_extend" seamInN.extend)
        (Port @"seam_north_rx" seamInN.rx)
        (Port @"seam_south_extend" seamInS.extend)
        (Port @"seam_south_rx" seamInS.rx)
