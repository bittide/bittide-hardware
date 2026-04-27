-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Tests.AddressableBytesWb where

import Clash.Prelude

-- Local
import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.DoubleBufferedRam (wbStorage)
import Bittide.Instances.Common (
  PeConfigElfSource (NameOnly),
  dumpVcdFromEnvVar,
  emptyPeConfig,
  peConfigFromElf,
 )
import Bittide.Instances.Domains (Basic50)
import Bittide.ProcessingElement (
  PeConfig (..),
  processingElement,
 )
import Bittide.SharedTypes (withByteOrder)
import Bittide.Wishbone (uartBytes, uartInterfaceWb)
import Clash.Class.BitPackC (ByteOrder (LittleEndian))
import Data.Char (chr)
import Data.Maybe (catMaybes)
import Project.FilePath (CargoBuildType (Release))
import Protocols
import Protocols.Idle (idleSource)
import Protocols.MemoryMap (MemoryMap, Mm, getMMAny, unMemmap)
import Test.Tasty.HUnit (HasCallStack)
import VexRiscv (DumpVcd (NoDumpVcd))

peConfigSim :: IO (PeConfig 4)
peConfigSim =
  peConfigFromElf
    (SNat @IMemWords)
    (SNat @DMemWords)
    (NameOnly "addressable_bytes_wb_test")
    Release
    d0
    d0
    False
    vexRiscv0

getDumpVcd :: IO DumpVcd
getDumpVcd = dumpVcdFromEnvVar "ADDRESSABLE_BYTES_WB_DUMP_VCD"

sim :: IO ()
sim = do
  dumpVcd <- getDumpVcd
  peConfig <- peConfigSim
  let simResult = dutUartStream dumpVcd peConfig
  -- Print the UART output to terminal for debugging
  putStrLn "=== UART Output ==="
  putStrLn simResult
  putStrLn "=================="

dutUartStream :: (HasCallStack) => DumpVcd -> PeConfig 4 -> String
dutUartStream dumpVcd peConfig = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream =
    sampleC def{timeoutAfter = 1000000} $ unMemmap $ dutWithVcdAndPeConfig dumpVcd peConfig

-- | Parameterized DUT that loads a specific firmware binary.
dutWithVcdAndPeConfig ::
  (HasCallStack) =>
  DumpVcd ->
  PeConfig 4 ->
  Circuit (ToConstBwd Mm, ()) (Df Basic50 (BitVector 8))
dutWithVcdAndPeConfig dumpVcd peConfig =
  withByteOrder LittleEndian
    $ withClockResetEnable clockGen (resetGenN d2) enableGen
    $ circuit
    $ \(mm, _unit) -> do
      (uartRx, jtag) <- idleSource
      [uartBus, addressableBus] <- processingElement dumpVcd peConfig -< (mm, jtag)
      (uartTx, _uartStatus) <- uartInterfaceWb d2 d2 uartBytes -< (uartBus, uartRx)
      wbStorage "AddressableBuffer" d8 Nothing -< addressableBus
      idC -< uartTx
{-# OPAQUE dutWithVcdAndPeConfig #-}

memoryMap :: MemoryMap
memoryMap =
  getMMAny
    $ dutWithVcdAndPeConfig NoDumpVcd
    $ emptyPeConfig (SNat @IMemWords) (SNat @DMemWords) d0 d0 False vexRiscv0

type IMemWords = DivRU (16 * 1024) 4
type DMemWords = DivRU (16 * 1024) 4
