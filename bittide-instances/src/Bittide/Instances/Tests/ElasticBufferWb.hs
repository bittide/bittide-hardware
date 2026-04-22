-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Tests.ElasticBufferWb where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.ElasticBuffer
import Bittide.Instances.Common (
  PeConfigElfSource (NameOnly),
  dumpVcdFromEnvVar,
  emptyPeConfig,
  peConfigFromElf,
 )
import Bittide.ProcessingElement
import Bittide.SharedTypes (withLittleEndian)
import Bittide.Wishbone
import GHC.Stack (HasCallStack)
import Project.FilePath
import Protocols
import Protocols.Idle
import Protocols.MemoryMap
import VexRiscv (DumpVcd (..))

peConfigSim :: IO (PeConfig 5)
peConfigSim =
  peConfigFromElf
    (SNat @IMemWords)
    (SNat @DMemWords)
    (NameOnly "elastic_buffer_wb_test")
    Release
    d0
    d0
    False
    vexRiscv0

getDumpVcd :: IO DumpVcd
getDumpVcd = dumpVcdFromEnvVar "ELASTIC_BUFFER_WB_DUMP_VCD"

-- | Strip "SimOnly" constructor
unSimOnly :: SimOnly a -> a
unSimOnly (SimOnly x) = x

dutMM :: (HasCallStack) => Protocols.MemoryMap.MemoryMap
dutMM =
  (\(SimOnly mm, _) -> mm)
    $ withClockResetEnable @XilinxSystem clockGen resetGen enableGen
    $ flip toSignals ((), pure $ deepErrorX "uart_bwd")
    $ dut NoDumpVcd
    $ emptyPeConfig (SNat @IMemWords) (SNat @DMemWords) d0 d0 False vexRiscv0

{- | A simulation-only instance containing VexRisc with UART and an elastic buffer
controlled via Wishbone. The processor runs the `elastic_buffer_wb_test` program
performing a series of tests on the elastic buffer via Wishbone and prints the
results over UART.
-}
dut ::
  (HasCallStack) =>
  DumpVcd ->
  PeConfig 5 ->
  Circuit
    (ToConstBwd Mm)
    (Df XilinxSystem (BitVector 8)) -- UART output
dut dumpVcd peConfig =
  withLittleEndian
    $ withClockResetEnable clockGen (resetGenN d2) enableGen
    $ circuit
    $ \mm -> do
      (uartRx, jtagIdle) <- idleSource
      [ebWbBus, uartBus, (mmTime, timeBus)] <-
        processingElement @_ dumpVcd peConfig -< (mm, jtagIdle)
      (uartTx, _uartStatus) <- uartInterfaceWb d16 d2 uartBytes -< (uartBus, uartRx)

      -- Timer for delays instead of NOP loops
      Fwd localCounter <- timeWb Nothing -< (mmTime, timeBus)

      -- Elastic buffer with Wishbone control and monitoring
      -- We use the same clock for both read and write domains for simplicity in testing
      _readData <-
        xilinxElasticBufferWb
          clockGen
          (resetGenN d2)
          d5
          localCounter
          clockGen
          (pure () :: Signal XilinxSystem ())
          -< ebWbBus

      idC -< uartTx

type IMemWords = DivRU (16 * 1024) 4
type DMemWords = DivRU (16 * 1024) 4
