-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Tests.Axi where

-- Preludes
import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

-- Local
import Bittide.Axi4
import Bittide.ProcessingElement
import Bittide.Wishbone hiding (MemoryMap)
import Project.FilePath

-- Other
import Bittide.Instances.Common (PeConfigElfSource (NameOnly), emptyPeConfig, peConfigFromElf)
import Bittide.SharedTypes (withLittleEndian)
import Data.Proxy
import Protocols
import Protocols.Experimental.Axi4.Stream
import Protocols.Idle
import Protocols.MemoryMap
import VexRiscv (DumpVcd (NoDumpVcd))

-- Qualified
import qualified Bittide.Cpus.Riscv32imc as Riscv32imc
import qualified Protocols.DfConv as DfConv

{- | A simple instance containing just VexRisc and UART as peripheral.
Runs the `hello` binary from `firmware-binaries`.
-}
dutMm :: PeConfig 5 -> Circuit (ToConstBwd Mm, ()) (Df System (BitVector 8))
dutMm peConfig =
  withLittleEndian
    $ withClockResetEnable clockGen (resetGenN d2) enableGen
    $ circuit
    $ \(mm, _unit) -> do
      (uartTx, jtag) <- idleSource
      [uartBus, (mmAxiTx, axiTxBus), (mmAxiRx, axiRxBus)] <-
        processingElement NoDumpVcd peConfig -< (mm, jtag)

      (uartRx, _uartStatus) <- uartInterfaceWb d2 d2 uartBytes -< (uartBus, uartTx)

      _interrupts <- wbAxisRxBufferCircuit (SNat @128) -< ((mmAxiRx, axiRxBus), axiStream)

      axiStream <-
        axiUserMapC (const False)
          <| DfConv.fifo axiProxy axiProxy (SNat @1024)
          <| axiPacking
          <| wbToAxi4StreamTx
          -< (mmAxiTx, axiTxBus)
      idC -< uartRx
 where
  axiProxy = Proxy @(Axi4Stream System ('Axi4StreamConfig 4 0 0) ())

{-# OPAQUE dut #-}

type IMemWords = DivRU (16 * 1024) 4
type DMemWords = DivRU (16 * 1024) 4

memoryMap :: MemoryMap
memoryMap =
  getMMAny
    $ dutMm
    $ emptyPeConfig (SNat @IMemWords) (SNat @DMemWords) d0 d0 False Riscv32imc.vexRiscv0

peConfigSim :: IO (PeConfig 5)
peConfigSim =
  peConfigFromElf
    (SNat @IMemWords)
    (SNat @DMemWords)
    (NameOnly "axi_stream_self_test")
    Release
    d0
    d0
    False
    Riscv32imc.vexRiscv0

dut :: PeConfig 5 -> Circuit () (Df System (BitVector 8))
dut = unMemmap . dutMm
