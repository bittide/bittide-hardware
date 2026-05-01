-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Tests.DnaPortE2 where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)

import Clash.Cores.Xilinx.Unisim.DnaPortE2
import Protocols
import Protocols.Idle
import Protocols.MemoryMap
import VexRiscv (DumpVcd (NoDumpVcd))

import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.Instances.Common (PeConfigElfSource (NameOnly), emptyPeConfig, peConfigFromElf)
import Bittide.ProcessingElement
import Bittide.SharedTypes (withLittleEndian)
import Bittide.Wishbone hiding (MemoryMap)
import Project.FilePath (CargoBuildType (Release))

{- | A simple instance containing just VexRisc with UART and the DNA peripheral which
runs the `dna_port_e2_test` binary from `firmware-binaries`.
-}
dutWithMm ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  PeConfig 4 ->
  Circuit (ToConstBwd Mm, ()) (Df dom (BitVector 8))
dutWithMm peConfig = withLittleEndian $ circuit $ \(mm, _unit) -> do
  (uartRx, jtag) <- idleSource
  [uartBus, dnaBus] <-
    processingElement @dom NoDumpVcd peConfig -< (mm, jtag)
  (uartTx, _uartStatus) <- uartInterfaceWb d2 d2 uartBytes -< (uartBus, uartRx)
  _dna <- readDnaPortE2Wb simDna2 -< dnaBus
  idC -< uartTx
{-# OPAQUE dutWithMm #-}

type IMemWords = DivRU (4 * 1024) 4
type DMemWords = DivRU (4 * 1024) 4

peConfigSim :: IO (PeConfig 4)
peConfigSim =
  peConfigFromElf
    (SNat @IMemWords)
    (SNat @DMemWords)
    (NameOnly "dna_port_e2_test")
    Release
    d0
    d0
    False
    vexRiscv0

dutMm :: MemoryMap
dutMm =
  getMMAny
    $ withClockResetEnable @System clockGen (resetGenN d2) enableGen
    $ dutWithMm
    $ emptyPeConfig (SNat @IMemWords) (SNat @DMemWords) d0 d0 False vexRiscv0

dut ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  PeConfig 4 ->
  Circuit () (Df dom (BitVector 8))
dut = unMemmap . dutWithMm
