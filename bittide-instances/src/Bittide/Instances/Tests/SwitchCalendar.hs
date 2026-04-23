-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Tests.SwitchCalendar where

import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.Instances.Common (
  PeConfigElfSource (NameOnly),
  dumpVcdFromEnvVar,
  emptyPeConfig,
  peConfigFromElf,
 )
import Bittide.Instances.Domains (Basic200)
import Bittide.Instances.Pnr.Switch
import Bittide.ProcessingElement
import Bittide.SharedTypes
import Bittide.Wishbone hiding (MemoryMap)
import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)
import Data.Char
import Data.Maybe
import GHC.Stack (HasCallStack)
import Project.FilePath (
  CargoBuildType (..),
 )
import Protocols
import Protocols.Idle
import Protocols.MemoryMap
import VexRiscv (DumpVcd (NoDumpVcd))

memoryMap :: (HasCallStack) => MemoryMap
memoryMap =
  getMMAny
    $ dut NoDumpVcd
    $ emptyPeConfig
      (SNat @IMemWords)
      (SNat @DMemWords)
      d0
      d0
      False
      vexRiscv0

main :: IO ()
main = sim

sim :: IO ()
sim = do
  dumpVcd <- getDumpVcd
  peConfig <- peConfigSim
  putStrLn $ simResult dumpVcd peConfig

simResult :: DumpVcd -> PeConfig 4 -> String
simResult dumpVcd peConfig = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream = sampleC def $ unMemmap $ dut dumpVcd peConfig

dut :: DumpVcd -> PeConfig 4 -> Circuit (ToConstBwd Mm, ()) (Df Basic200 (BitVector 8))
dut dumpVcd peConfig =
  withLittleEndian
    $ withClockResetEnable clockGen (resetGenN d2) enableGen
    $ circuit
    $ \(mm, _unit) -> do
      (uartRx, jtag) <- idleSource
      [uartBus, (switchMm, switchCalWb)] <-
        processingElement dumpVcd peConfig -< (mm, jtag)
      (uartTx, _uartStatus) <- uartInterfaceWb d2 d2 uartBytes -< (uartBus, uartRx)
      _switchResult <-
        switchExample clockGen (resetGenN d2) -< (switchMm, (Fwd (repeat $ pure 0), switchCalWb))
      idC -< uartTx
{-# OPAQUE dut #-}

type IMemWords = DivRU (8 * 1024) 4
type DMemWords = DivRU (16 * 1024) 4

getDumpVcd :: IO DumpVcd
getDumpVcd = dumpVcdFromEnvVar "SWITCHCALENDAR_DUMP_VCD"

peConfigSim :: IO (PeConfig 4)
peConfigSim =
  peConfigFromElf
    (SNat @IMemWords)
    (SNat @DMemWords)
    (NameOnly "switch_calendar_test")
    Release
    d0
    d0
    False
    vexRiscv0
